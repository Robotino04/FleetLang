#[cfg(test)]

use std::fmt::Debug;

use inkwell::{
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target},
};

use fleet::{
    ast::AstNode,
    infra::{CompileResult, CompileStatus, compile},
    pretty_print::pretty_print,
    tokenizer::SourceLocation,
};

pub fn assert_parser_or_tokenizer_error<'a>(src: &str, error_start: SourceLocation) {
    let context = Context::create();
    let result = compile(&context, src);

    assert!(
        match result.status {
            CompileStatus::TokenizerFailure {} => false,
            CompileStatus::ParserFailure { tokens: _ } => false,
            CompileStatus::TokenizerOrParserErrors {
                tokens: _,
                partial_program: _,
            } => true,
            CompileStatus::IrGeneratorFailure {
                tokens: _,
                program: _,
            } => false,
            CompileStatus::Success {
                tokens: _,
                program: _,
                module: _,
            } => false,
        },
        "Expected compilation to error at tokenizer or parser. Got {:#?}",
        result.status
    );
    assert!(
        result.errors.iter().any(|err| err.start == error_start),
        "Expected an error at {error_start:?}. Got {:#?}",
        result.errors
    );
}

fn assert_successful_compilation_context<'a>(context: &'a Context, src: &str) -> CompileResult<'a> {
    let result = compile(context, src);

    assert!(result.errors.is_empty(), "{:#?}", result.errors);
    assert!(
        matches!(
            result.status,
            CompileStatus::Success {
                tokens: _,
                program: _,
                module: _
            }
        ),
        "result.status = {:#?}",
        result.status
    );

    assert!(result.status.tokens().is_some());
    assert!(result.status.program().is_some());
    assert!(result.status.module().is_some());

    let module = result.status.module().unwrap();

    module.verify().unwrap();
    return result;
}

fn assert_function_return_value<ReturnType>(
    module: &Module,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq,
{
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native LLVM target");

    let execution_engine = module
        .create_jit_execution_engine(inkwell::OptimizationLevel::None)
        .unwrap();
    let main_function = unsafe {
        execution_engine
            .get_function::<unsafe extern "C" fn() -> ReturnType>(function_name)
            .unwrap()
    };

    let actual_return_value: ReturnType = unsafe { main_function.call() };
    assert_eq!(
        actual_return_value, expected_return_value,
        "expected {function_name:?} to return {expected_return_value:?} instead of {actual_return_value:?}"
    );
}

pub fn assert_successful_compilation(src: &str) {
    let context = Context::create();
    assert_successful_compilation_context(&context, src);
}

pub fn assert_compile_and_return_value<ReturnType>(
    src: &str,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq,
{
    let context = Context::create();
    let result = assert_successful_compilation_context(&context, src);
    let module = result.status.module().unwrap();
    assert_function_return_value(module, function_name, expected_return_value);
}

pub fn assert_formatting(src: &str, expected_fmt: &str) {
    let context = Context::create();
    let result = assert_successful_compilation_context(&context, src);
    let formatted_src = pretty_print(AstNode::Program(result.status.program().unwrap().clone()));
    assert_eq!(
        expected_fmt, formatted_src,
        "expected left to format as right"
    );
    assert_is_formatted(formatted_src.as_str());
}

fn assert_is_formatted(src: &str) {
    let context = Context::create();
    let result = assert_successful_compilation_context(&context, src);
    let formatted_src = pretty_print(AstNode::Program(result.status.program().unwrap().clone()));
    assert_eq!(src, formatted_src, "Expected left to be formatted");
}
