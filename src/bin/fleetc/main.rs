use std::{path::Path, process::exit};

use fleet::{
    ast::AstNode,
    generate_c::generate_c,
    infra::{CompileStatus, compile, fleet_error, print_error_message},
    pretty_print::pretty_print,
};
use inkwell::{
    OptimizationLevel,
    context::Context,
    targets::{CodeModel, FileType, RelocMode, Target, TargetTriple},
};

fn generate_header(text: impl AsRef<str>, length: usize) -> String {
    return format!("{:-^length$}", "|".to_string() + text.as_ref() + "|");
}

fn main() {
    let src = include_str!("../../../test.fl");

    let context = Context::create();
    let res = compile(&context, src);

    let print_all_errors_and_message = |msg| {
        println!("{}", generate_header("Errors", 50));
        for error in &res.errors {
            print_error_message(&src.to_string(), error);
        }
        fleet_error(msg);
    };

    match &res.status {
        CompileStatus::TokenizerFailure {} => {
            print_all_errors_and_message("The tokenizer failed completely");
            exit(1);
        }
        CompileStatus::ParserFailure { tokens } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            print_all_errors_and_message("The parser failed completely");
            exit(1);
        }
        CompileStatus::TokenizerOrParserErrors {
            tokens,
            partial_program,
        } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            println!("{}", generate_header("Partial AST", 50));
            println!("{:#?}", partial_program);
            print_all_errors_and_message("The parser or tokenizer failed partially");
            exit(1);
        }
        CompileStatus::IrGeneratorFailure { tokens, program } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            println!("{}", generate_header("AST", 50));
            println!("{:#?}", program);
            print_all_errors_and_message("The ir generator failed completely");
            exit(1);
        }
        CompileStatus::Success {
            tokens,
            program,
            module,
        } => {
            println!("{}", generate_header("Tokens", 50));
            println!("{:#?}", tokens);
            println!("{}", generate_header("AST", 50));
            println!("{:#?}", program);
            println!("{}", generate_header("LLVM IR", 50));
            println!("{:#?}", module.print_to_string());
            assert!(res.errors.is_empty());
        }
    }
    let program = res.status.program().unwrap();
    let module = res.status.module().unwrap();

    println!("{}", generate_header("Pretty-Printed", 50));
    println!("{}", pretty_print(AstNode::Program(program.clone())));
    println!("{}", generate_header("C Code", 50));
    println!("{}", generate_c(AstNode::Program(program.clone())));

    Target::initialize_all(&inkwell::targets::InitializationConfig::default());

    let triple = TargetTriple::create("x86_64-pc-linux-gnu");
    let target = Target::from_triple(&triple).unwrap();
    let target_machine = target
        .create_target_machine(
            &triple,
            "x86-64",
            "+avx2",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();
    println!("target created");

    let output_path = Path::new("output.o");
    target_machine
        .write_to_file(&module, FileType::Object, output_path)
        .unwrap();

    println!("Object file written to {:?}", output_path);
}
