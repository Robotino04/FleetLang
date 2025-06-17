use std::{env::args, fs::read_to_string, path::Path, process::exit};

use fleet::infra::{ErrorSeverity, format_program, run_default_optimization_passes};
use fleet::{
    ast::AstNode,
    generate_c::generate_c,
    infra::{CompileStatus, compile_program, print_error_message},
};
use inkwell::{
    OptimizationLevel,
    context::Context,
    targets::{CodeModel, FileType, RelocMode, Target, TargetTriple},
};

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
    let input_path = args().nth(1).unwrap_or_else(|| {
        eprintln!("No input file was given.");
        exit(1);
    });
    let src = read_to_string(&input_path).unwrap_or_else(|_| {
        eprintln!("Input file {input_path:?} doesn't exist or isn't readable");
        exit(1);
    });

    let context = Context::create();
    let res = compile_program(&context, &src);

    let print_all_errors_and_message = |msg| {
        let mut worst_error = None;
        for error in &res.errors {
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
            for error in &res.errors {
                print_error_message(&src, error);
            }
            let ansi_color = ansi_color_for_severity(worst_severity);
            println!("\x1B[{}m{msg}\x1B[0m", ansi_color);
        }
    };

    match &res.status {
        CompileStatus::ParserFailure { tokens } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            print_all_errors_and_message("The parser failed completely");
            exit(1);
        }
        CompileStatus::TokenizerOrParserErrors {
            tokens,
            partial_parsed_program,
            id_generator: _,
        } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            println!("{}", generate_header("Partial AST", 50));
            println!("{:#?}", partial_parsed_program);
            print_all_errors_and_message("The parser or tokenizer failed partially");
            exit(1);
        }
        CompileStatus::AnalysisErrors {
            tokens,
            parsed_program: _,
            parsed_id_generator: _,
            program,
            id_generator: _,
            function_terminations,
            type_data: _,
            variable_data: _,
            function_data: _,
        } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            println!("{}", generate_header("AST", 50));
            println!("{:#?}", program);
            println!("{}", generate_header("Function Terminations", 50));
            println!("{:#?}", function_terminations);
            print_all_errors_and_message("Program analysis found some errors");
            exit(1);
        }
        CompileStatus::IrGeneratorFailure {
            tokens,
            parsed_program: _,
            program,
            function_terminations,
            type_data: _,
            variable_data: _,
            function_data: _,
            id_generator: _,
            parsed_id_generator: _,
        } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            println!("{}", generate_header("AST", 50));
            println!("{:#?}", program);
            println!("{}", generate_header("Function Terminations", 50));
            println!("{:#?}", function_terminations);
            print_all_errors_and_message("The ir generator failed completely");
            exit(1);
        }
        CompileStatus::IrGeneratorErrors {
            tokens,
            parsed_program: _,
            program,
            partial_module,
            function_terminations,
            type_data: _,
            variable_data: _,
            function_data: _,
            id_generator: _,
            parsed_id_generator: _,
        } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);

            println!("{}", generate_header("AST", 50));
            println!("{:#?}", program);

            println!("{}", generate_header("Function Terminations", 50));
            println!("{:#?}", function_terminations);

            println!("{}", generate_header("Partial LLVM IR (unoptimized)", 50));
            println!(
                "{}",
                partial_module
                    .clone()
                    .map(|module| module.print_to_string().to_str().unwrap().to_string())
                    .unwrap_or("<Module is invalid>".to_string())
            );

            print_all_errors_and_message("The ir generator failed partially");
            exit(1);
        }
        CompileStatus::Success {
            tokens,
            parsed_program: _,
            program,
            module,
            function_terminations,
            type_data: _,
            variable_data: _,
            function_data: _,
            id_generator: _,
            parsed_id_generator: _,
        } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);

            println!("{}", generate_header("AST", 50));
            println!("{:#?}", program);

            println!("{}", generate_header("Function Terminations", 50));
            println!("{:#?}", function_terminations);

            println!("{}", generate_header("LLVM IR (unoptimized)", 50));
            println!("{}", module.print_to_string().to_str().unwrap());

            assert!(
                !res.errors
                    .iter()
                    .any(|err| matches!(err.severity, ErrorSeverity::Error))
            );
        }
    }
    let program = res.status.program().unwrap().clone();
    let module = res.status.module().unwrap();

    println!("{}", generate_header("C Code", 50));
    println!("{}", generate_c(AstNode::Program(program.clone())));

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
    println!(
        "{}",
        format_program(
            res.status.parsed_program().unwrap().clone(),
            res.status.parsed_id_generator().unwrap().clone()
        )
    );
    println!("{}", generate_header("", 50));

    Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    let triple = TargetTriple::create("x86_64-pc-linux-gnu");
    let target = Target::from_triple(&triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &triple,
            "x86-64",
            "+avx2",
            OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    println!("{}", generate_header("LLVM IR (optimized)", 50));

    run_default_optimization_passes(module, &target_machine).unwrap();

    println!("{}", module.print_to_string().to_str().unwrap());

    let output_path = Path::new("output.o");
    target_machine
        .write_to_file(module, FileType::Object, output_path)
        .unwrap();

    println!("Object file written to {:?}", output_path);

    print_all_errors_and_message("There are warnings");
}
