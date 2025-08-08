use std::{env::args, fs::read_to_string, path::Path, process::exit};

use fleet::NewtypeDerefNoDefault;
use fleet::ast::Program;
use fleet::ast_to_dm::AstToDocumentModelConverter;
use fleet::document_model::{DocumentElement, stringify_document};
use fleet::infra::{
    ErrorSeverity, FleetError, insert_c_passes, insert_compile_passes, insert_fix_passes,
    insert_minimal_pipeline, print_error_message,
};
use fleet::inkwell::module::Module;
use fleet::inkwell::targets::{CodeModel, RelocMode, Target, TargetTriple};
use fleet::inkwell::{self, OptimizationLevel};
use fleet::ir_generator::{IrGenerator, LLVMOptimizerPass};
use fleet::passes::pass_manager::{
    CCodeOutput, Errors, FunctionData, InputSource, PassError, PassManager, StatData, VariableData,
};
use fleet::passes::save_artifact_pass::{ArtifactType, SaveArtifactPass};
use fleet::passes::store_pass::StorePass;
use fleet::passes::swap_pass::SwapPass;
use fleet::tokenizer::Token;

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

NewtypeDerefNoDefault!(pub RawProgram, Program);
NewtypeDerefNoDefault!(pub FixedProgram, Program);
NewtypeDerefNoDefault!(pub UnoptimizedModule, Module<'static>);

fn main() {
    let input_path = args().nth(1).unwrap_or("test.fl".into());
    let output_path = Path::new("output.o");

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
            println!("\x1B[{ansi_color}m{msg}\x1B[0m");
        }
    };

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

    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    pm.insert::<StorePass<Program, RawProgram>>();
    insert_fix_passes(&mut pm);
    pm.insert::<StorePass<Program, FixedProgram>>();
    insert_compile_passes(&mut pm);
    insert_c_passes(&mut pm);
    pm.insert_params::<SaveArtifactPass>(|| ("./out.c".into(), ArtifactType::CCode));

    pm.insert::<IrGenerator>();
    pm.insert_params::<SaveArtifactPass>(|| ("./out_unopt.ll".into(), ArtifactType::LlvmIr));
    pm.insert::<StorePass<Module, UnoptimizedModule>>();
    pm.insert::<LLVMOptimizerPass>();
    pm.insert_params::<SaveArtifactPass>(|| ("./out_opt.ll".into(), ArtifactType::LlvmIr));
    pm.insert_params::<SaveArtifactPass>(|| (output_path.to_owned(), ArtifactType::Object));

    pm.insert::<SwapPass<Program, FixedProgram>>();
    pm.insert::<AstToDocumentModelConverter>();
    pm.insert::<SwapPass<Program, FixedProgram>>();

    let errors = pm.state.insert_default::<Errors>();
    pm.state.insert(InputSource(src.to_string()));
    let _target_machine = pm.state.insert(target_machine);

    match pm.run() {
        Err(err @ PassError::CompilerError { .. }) => {
            let errors = errors.get(&pm.state).clone();
            print_all_errors_and_message("Compilation failed internally", &errors);
            println!("{err}");
            exit(1);
        }
        Err(PassError::InvalidInput { .. }) => {
            let errors = errors.get(&pm.state).clone();
            print_all_errors_and_message("Program has errors", &errors);
            exit(1);
        }
        Ok(()) => (),
    };

    let errors = errors.get(&pm.state).clone();
    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        print_all_errors_and_message("Compilation has errors", &errors);
        panic!("these errors should have resulted in a PassError::InvalidInput");
    }

    println!("{}", generate_header("Tokens", 50));
    println!("{:#?}", pm.state.get::<Vec<Token>>());

    println!("{}", generate_header("AST", 50));
    println!("{:#?}", pm.state.get::<RawProgram>());

    println!("{}", generate_header("Referenced Variables", 50));
    println!("{:#?}", pm.state.get::<VariableData>());
    println!("{}", generate_header("Referenced Functions", 50));
    println!("{:#?}", pm.state.get::<FunctionData>());

    println!("{}", generate_header("Node Stats", 50));
    println!("{:#?}", pm.state.get::<StatData>());

    println!("{}", generate_header("LLVM IR (unoptimized)", 50));
    println!("{}", pm.state.get::<Module>().unwrap().to_string());

    println!("{}", generate_header("C Code", 50));
    println!("{}", &pm.state.get::<CCodeOutput>().unwrap().0);

    let dm = pm.state.get::<DocumentElement>().unwrap().clone();

    println!("{}", generate_header("Pretty-Printed", 50));
    println!("{}", stringify_document(dm));

    println!("{}", generate_header("LLVM IR (optimized)", 50));
    println!("{}", pm.state.get::<Module>().unwrap().to_string());

    println!("Object file written to {output_path:?}");

    print_all_errors_and_message("There are warnings", &errors);
}
