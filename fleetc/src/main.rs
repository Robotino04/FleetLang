use std::{env::args, fs::read_to_string, path::Path, process::exit};

use fleet::infra::{ErrorSeverity, FleetError, TokenizerOutput, print_error_message};
use fleet::inkwell::context::Context;
use fleet::inkwell::targets::{CodeModel, FileType, RelocMode, Target, TargetTriple};
use fleet::inkwell::{self, OptimizationLevel};

fn generate_header(text: impl AsRef<str>, length: usize) -> String {
    format!("{:-^length$}", "|".to_string() + text.as_ref() + "|")
}
fn ansi_color_for_severity(severity: ErrorSeverity) -> &'static str {
    match severity {
        ErrorSeverity::Error => "31",
        ErrorSeverity::Warning => "33",
        ErrorSeverity::Note => "34",
    }
}

fn main() {
    let input_path = args().nth(1).unwrap_or("test.fl".into());
    let src = read_to_string(&input_path).unwrap_or_else(|_| {
        eprintln!("Input file {input_path:?} doesn't exist or isn't readable");
        exit(1);
    });

    let print_all_errors_and_message = |msg, errors: &Vec<FleetError>| {
        let mut worst_error = None;
        for error in errors {
            if let Some(prev) = worst_error {
                match (prev, error.severity) {
                    (ErrorSeverity::Warning, ErrorSeverity::Error)
                    | (ErrorSeverity::Note, ErrorSeverity::Error)
                    | (ErrorSeverity::Note, ErrorSeverity::Warning) => {
                        worst_error = Some(error.severity);
                    }
                    _ => {}
                }
            } else {
                worst_error = Some(error.severity);
            }
        }
        if let Some(worst_severity) = worst_error {
            println!("{}", generate_header("Errors", 50));
            for error in errors {
                print_error_message(&src, error);
            }
            let ansi_color = ansi_color_for_severity(worst_severity);
            println!("\x1B[{}m{msg}\x1B[0m", ansi_color);
        }
    };

    let mut errors = vec![];
    let Some(tokenizer_output) = TokenizerOutput::new(&src, &mut errors) else {
        print_all_errors_and_message("The tokenizer failed completely", &errors);
        exit(1);
    };
    println!("{}", generate_header("Tokens", 50));
    println!("{:#?}", tokenizer_output.tokens);

    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        print_all_errors_and_message("The tokenizer failed partially", &errors);
    }

    let Ok(parser_output) = tokenizer_output.parse(&mut errors) else {
        print_all_errors_and_message("The parser failed completely", &errors);
        exit(1);
    };
    println!("{}", generate_header("AST", 50));
    println!("{:#?}", parser_output.program);

    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        print_all_errors_and_message("The parser failed partially", &errors);
    }

    let Some(analysis_output) = parser_output.analyze(&mut errors) else {
        print_all_errors_and_message("Analysis failed completely", &errors);
        exit(1);
    };

    println!("{}", generate_header("Function Terminations", 50));
    println!("{:#?}", analysis_output.function_terminations);

    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        print_all_errors_and_message("Program analysis found some errors", &errors);
    }

    let context = Context::create();
    let Some(llvm_output) = analysis_output.compile_llvm(&mut errors, &context) else {
        print_all_errors_and_message("LLVM compilation failed completely", &errors);
        exit(1);
    };

    println!("{}", generate_header("LLVM IR (unoptimized)", 50));
    println!("{:#?}", analysis_output.function_terminations);

    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        print_all_errors_and_message("LLVM compilation failed partially", &errors);
    }

    let c_code = analysis_output.compile_c(&mut errors);
    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        print_all_errors_and_message("C generation failed partially", &errors);
    }

    println!("{}", generate_header("C Code", 50));
    println!("{}", c_code);

    std::fs::write("./out.c", c_code).expect("Writing to './out.c' failed");

    /*
    println!("{}", generate_header("Document Model", 50));
    RemoveParensPass::new().visit_program(&mut program);
    let document = convert_program_to_document_model(&program);
    println!("{:#?}", document.clone());

    println!("{}", generate_header("Flattened Document Model", 50));
    let document = fully_flatten_document(document);
    println!("{:#?}", document.clone());
    */

    println!("{}", generate_header("Pretty-Printed", 50));
    println!("{}", parser_output.format());

    Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    let triple = TargetTriple::create("x86_64-pc-linux-gnu");
    let target = Target::from_triple(&triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &triple,
            "x86-64",
            "+avx2",
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    println!("{}", generate_header("LLVM IR (optimized)", 50));

    llvm_output
        .run_default_optimization_passes(&target_machine)
        .unwrap();

    println!("{}", llvm_output.module.print_to_string().to_str().unwrap());

    let output_path = Path::new("output.o");
    target_machine
        .write_to_file(&llvm_output.module, FileType::Object, output_path)
        .unwrap();

    println!("Object file written to {:?}", output_path);

    print_all_errors_and_message("There are warnings", &errors);
}
