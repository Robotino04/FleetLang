#[cfg(test)]
use std::fmt::Debug;

use inkwell::{
    context::Context,
    module::Module,
    targets::{InitializationConfig, Target},
};

use fleet::{
    infra::{
        CompileResult, CompileStatus, ErrorSeverity, FleetError, compile_program, format_program,
    },
    tokenizer::SourceLocation,
};

pub fn assert_parser_or_tokenizer_error<'a>(src: &str, error_start: SourceLocation) {
    let context = Context::create();
    let result = compile_program(&context, src);

    assert!(
        match result.status {
            CompileStatus::TokenizerFailure {} => false,
            CompileStatus::ParserFailure { .. } => false,
            CompileStatus::TokenizerOrParserErrors { .. } => true,
            CompileStatus::AnalysisErrors { .. } => false,
            CompileStatus::IrGeneratorFailure { .. } => false,
            CompileStatus::IrGeneratorErrors { .. } => false,
            CompileStatus::Success { .. } => false,
        },
        "Expected compilation to error at tokenizer or parser. Got {:#?}",
        result.status
    );

    assert_error_at_position(&result.errors, error_start);
}

fn assert_error_at_position(errors: &Vec<FleetError>, error_start: SourceLocation) {
    assert!(
        errors
            .iter()
            .any(|err| err.start == error_start && err.severity == ErrorSeverity::Error),
        "Expected an error at {error_start:?}. Got {:#?}",
        errors
    );
}

fn assert_warning_at_position(errors: &Vec<FleetError>, warning_start: SourceLocation) {
    assert!(
        errors
            .iter()
            .any(|err| err.start == warning_start && err.severity == ErrorSeverity::Warning),
        "Expected a warning at {warning_start:?}. Got {:#?}",
        errors
    );
}

pub fn assert_compile_error<'a>(src: &str, error_start: SourceLocation) {
    let context = Context::create();
    let result = compile_program(&context, src);

    assert!(
        match result.status {
            CompileStatus::TokenizerFailure {} => false,
            CompileStatus::ParserFailure { .. } => false,
            CompileStatus::TokenizerOrParserErrors { .. } => false,
            CompileStatus::AnalysisErrors { .. } => true,
            CompileStatus::IrGeneratorFailure { .. } => false,
            CompileStatus::IrGeneratorErrors { .. } => true,
            CompileStatus::Success { .. } => false,
        },
        "Expected compilation to error after parsing. Got {:#?}",
        result.status
    );
    assert_error_at_position(&result.errors, error_start);
}
pub fn assert_compile_and_warning<'a>(src: &str, warning_start: SourceLocation) {
    let context = Context::create();
    let result = compile_program(&context, src);

    assert!(
        match result.status {
            CompileStatus::TokenizerFailure {} => false,
            CompileStatus::ParserFailure { .. } => false,
            CompileStatus::TokenizerOrParserErrors { .. } => false,
            CompileStatus::AnalysisErrors { .. } => false,
            CompileStatus::IrGeneratorFailure { .. } => false,
            CompileStatus::IrGeneratorErrors { .. } => false,
            CompileStatus::Success { .. } => true,
        },
        "Expected compilation to succeed with warnings. Got {:#?}",
        result.status
    );
    assert_warning_at_position(&result.errors, warning_start);
}

pub fn assert_successful_compilation(src: &str) {
    let context = Context::create();
    compile_or_panic(&context, src);
}

pub fn assert_compile_and_return_value<ReturnType>(
    src: &str,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq,
{
    assert_compile_and_return_value_unformatted(src, function_name, expected_return_value);
    assert_is_formatted(src.trim());
}

pub fn assert_compile_and_return_value_unformatted<ReturnType>(
    src: &str,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq,
{
    let context = Context::create();
    let result = compile_or_panic(&context, src);
    let module = result.status.module().unwrap();

    let actual_return_value: ReturnType = execute_function(module, function_name);
    assert_eq!(
        actual_return_value, expected_return_value,
        "expected {function_name:?} to return {expected_return_value:?} instead of {actual_return_value:?}"
    );
}

pub fn assert_formatting<'a>(src: &str, expected_fmt: &'a str) -> &'a str {
    let formatted_src = format_or_panic(src);
    assert_eq!(
        formatted_src, expected_fmt,
        "expected left to be formatted like right"
    );
    assert_is_formatted(formatted_src.as_str());
    return expected_fmt;
}

pub fn assert_formatting_and_same_behaviour<ReturnType>(
    src: &str,
    expected_fmt: &str,
    function_name: &str,
) where
    ReturnType: Debug + PartialEq,
{
    let formatted_src = assert_formatting(src, expected_fmt);

    let unformatted_retvalue = execute_or_panic::<ReturnType>(src, function_name);
    let formatted_retvalue = execute_or_panic::<ReturnType>(&formatted_src, function_name);

    assert_eq!(
        unformatted_retvalue, formatted_retvalue,
        "formatting changed behaviour from returning left to right"
    );
}

fn compile_or_panic<'a>(context: &'a Context, src: &str) -> CompileResult<'a> {
    let result = compile_program(context, src);

    assert!(result.errors.is_empty(), "{:#?}", result.errors);
    assert!(
        matches!(result.status, CompileStatus::Success { .. }),
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

fn format_or_panic(src: &str) -> String {
    let context = Context::create();
    let result = compile_or_panic(&context, src);
    return format_program(
        result.status.parsed_program().unwrap().clone(),
        result.status.parsed_id_generator().unwrap().clone(),
    );
}

fn execute_or_panic<ReturnType>(src: &str, function_name: &str) -> ReturnType {
    let context = Context::create();
    let result = compile_or_panic(&context, src);
    let retval: ReturnType = execute_function(result.status.module().unwrap(), function_name);
    return retval;
}

fn execute_function<ReturnType>(module: &Module, function_name: &str) -> ReturnType {
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

    return unsafe { main_function.call() };
}

fn assert_is_formatted(src: &str) {
    let formatted_src = format_or_panic(src);
    assert_eq!(src, formatted_src, "Expected left to be formatted");
}
