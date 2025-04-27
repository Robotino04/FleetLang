use std::path::Path;

use fleet::{
    ast::AstNode,
    generate_c::generate_c,
    ir_generator::IrGenerator,
    parser::{ParseError, Parser},
    pretty_print::pretty_print,
    tokenizer::Tokenizer,
};
use inkwell::{
    OptimizationLevel,
    context::Context,
    targets::{CodeModel, FileType, RelocMode, Target, TargetTriple},
};

fn generate_header(text: impl AsRef<str>, length: usize) -> String {
    return format!("{:-^length$}", "|".to_string() + text.as_ref() + "|");
}

fn print_error_message(source: &String, error: &ParseError) {
    let num_before_error_lines = 3;
    let num_after_error_lines = 3;

    let max_line_number_len = (error.end.line + num_after_error_lines).to_string().len();

    let pad_with_line_number = |(line, text): (usize, &str)| {
        format!("{line:<pad_size$}| {text}", pad_size = max_line_number_len)
    };

    let source_lines = source
        .split("\n")
        .enumerate()
        .map(|(line, text)| (line + 1, text));

    let before_err = source_lines.clone().take(error.start.line - 1);
    let err = source_lines
        .clone()
        .skip(error.start.line - 1)
        .take(error.end.line - error.start.line + 1)
        .map(|(line, text)| {
            pad_with_line_number((line, format!("\x1B[31m{}\x1B[0m", text).as_str()))
        })
        .collect::<Vec<_>>()
        .join("\n");
    let after_err = source_lines.skip(error.end.line);

    let before_err_trunc = before_err
        .skip(error.start.line.saturating_sub(num_before_error_lines + 1))
        .skip_while(|(_line, text)| text.trim() == "")
        .map(pad_with_line_number)
        .collect::<Vec<_>>()
        .join("\n");
    let after_err_trunc = after_err
        .take(num_after_error_lines)
        .map(pad_with_line_number)
        .collect::<Vec<_>>()
        .join("\n");

    println!("\x1B[31m[FLEETC: ERROR] {}\x1B[0m", error.message);
    println!("{}", before_err_trunc);
    println!("{}", err);
    println!("{}\n", after_err_trunc);
}

fn main() {
    let src = include_str!("../../../test.fl");
    let mut tokenizer = Tokenizer::new(src.to_string());

    let tokens = tokenizer.tokenize().unwrap();
    println!("{:#?}", tokens);

    println!("{}", generate_header("Tokens", 50));

    let mut parser = Parser::new(tokens.clone());
    let program = parser.parse_program().unwrap();

    println!("{:#?}", program);

    println!("{}", generate_header("AST", 50));

    println!("{}", pretty_print(AstNode::Program(program.clone())));

    println!("{}", generate_header("Errors", 50));

    for error in parser.errors() {
        print_error_message(&src.to_string(), error);
    }

    println!("{}", generate_header("C Code", 50));

    println!("{}", generate_c(AstNode::Program(program.clone())));

    println!("{}", generate_header("LLVM IR", 50));

    let context = Context::create();
    let mut ir_generator = IrGenerator::new(&context);
    let module = ir_generator.generate_program_ir(&program);

    println!("{}", module.print_to_string().to_str().unwrap());

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
        .write_to_file(module, FileType::Object, output_path)
        .unwrap();

    println!("Object file written to {:?}", output_path);
}
