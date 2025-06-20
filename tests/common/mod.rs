#[cfg(test)]
use std::fmt::Debug;
use std::process::Command;

use inkwell::{
    OptimizationLevel,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
};

use fleet::{
    ast::AstVisitor,
    generate_c::CCodeGenerator,
    infra::{
        CompileResult, CompileStatus, ErrorSeverity, FleetError, compile_program, format_program,
        run_default_optimization_passes,
    },
    tokenizer::SourceLocation,
};
use tempfile::{TempDir, tempdir};

pub trait SubprocessTestableReturnType {
    fn into_return_value(self) -> i8;
}
impl SubprocessTestableReturnType for i64 {
    fn into_return_value(self) -> i8 {
        assert_eq!(
            self as i8 as i64, self,
            "{} (i64) would be truncated and can't be tested against",
            self
        );
        self as i8
    }
}
impl SubprocessTestableReturnType for i32 {
    fn into_return_value(self) -> i8 {
        assert_eq!(
            self as i8 as i32, self,
            "{} (i32) would be truncated and can't be tested against",
            self
        );
        self as i8
    }
}
impl SubprocessTestableReturnType for i16 {
    fn into_return_value(self) -> i8 {
        assert_eq!(
            self as i8 as i16, self,
            "{} (i16) would be truncated and can't be tested against",
            self
        );
        self as i8
    }
}
impl SubprocessTestableReturnType for i8 {
    fn into_return_value(self) -> i8 {
        self
    }
}
impl SubprocessTestableReturnType for bool {
    fn into_return_value(self) -> i8 {
        self as i8
    }
}

pub fn assert_parser_or_tokenizer_error(src: &str, error_start: SourceLocation) {
    let context = Context::create();
    let result = compile_program(&context, src);

    assert!(
        match result.status {
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

pub fn assert_compile_error(src: &str, error_start: SourceLocation) {
    assert_is_formatted(src);
    assert_compile_error_no_formatting(src, error_start);
}
pub fn assert_compile_error_no_formatting(src: &str, error_start: SourceLocation) {
    let context = Context::create();
    let result = compile_program(&context, src);

    assert!(
        match result.status {
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
pub fn assert_compile_and_warning(src: &str, warning_start: SourceLocation) {
    let context = Context::create();
    let result = compile_program(&context, src);

    assert!(
        match result.status {
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

pub fn assert_compile_and_return_value_llvm_only<ReturnType>(
    src: &str,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq,
{
    assert_compile_and_return_value_unformatted(src, function_name, expected_return_value);
    assert_is_formatted(src.trim());
}
pub fn assert_compile_and_return_value<ReturnType>(
    src: &str,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq + SubprocessTestableReturnType + Clone,
{
    assert_is_formatted(src.trim());
    println!("Source is formatted");
    assert_compile_and_return_value_unformatted::<ReturnType>(
        src,
        function_name,
        expected_return_value.clone(),
    );
    println!("LLVM execution succeeded");
    assert_compile_and_output_subprocess(src, expected_return_value.into_return_value(), "", "");
    println!("Subprocess execution succeeded");
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
    expected_fmt
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
    let formatted_retvalue = execute_or_panic::<ReturnType>(formatted_src, function_name);

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
    result
}

fn format_or_panic(src: &str) -> String {
    let context = Context::create();
    let result = compile_program(&context, src);

    assert!(
        match result.status {
            CompileStatus::ParserFailure { .. } => false,
            CompileStatus::TokenizerOrParserErrors { .. } => false,
            CompileStatus::AnalysisErrors { .. } => true,
            CompileStatus::IrGeneratorFailure { .. } => true,
            CompileStatus::IrGeneratorErrors { .. } => true,
            CompileStatus::Success { .. } => true,
        },
        "Cannot format something that doesn't parse: {:#?}",
        result.status
    );

    format_program(
        result.status.parsed_program().unwrap().clone(),
        result.status.parsed_id_generator().unwrap().clone(),
    )
}

fn execute_or_panic<ReturnType>(src: &str, function_name: &str) -> ReturnType {
    let context = Context::create();
    let result = compile_or_panic(&context, src);
    let retval: ReturnType = execute_function(result.status.module().unwrap(), function_name);
    retval
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

    unsafe { main_function.call() }
}

fn assert_is_formatted(src: &str) {
    let formatted_src = format_or_panic(src.trim_end());
    assert_eq!(
        src.trim_end(),
        formatted_src,
        "Expected left to be formatted"
    );
}

fn compile_to_binary_llvm(src: &str, dir: &TempDir) -> String {
    let context = Context::create();
    let result = compile_or_panic(&context, src);

    let module = result.status.module().unwrap();

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

    run_default_optimization_passes(module, &target_machine).unwrap();

    let object_file = dir.path().join("test.o");
    let binary_file = dir.path().join("test");

    target_machine
        .write_to_file(module, FileType::Object, object_file.as_path())
        .unwrap();

    println!("Calling clang to link {object_file:?} to {binary_file:?}");
    let clang_out = Command::new("clang")
        .arg("-fdiagnostics-color=always")
        .arg(object_file.to_str().unwrap())
        .arg("-o")
        .arg(binary_file.to_str().unwrap())
        .output()
        .unwrap();
    print!(
        "Clang stdout:\n{}",
        String::from_utf8(clang_out.stdout).unwrap()
    );
    print!(
        "Clang stderr:\n{}",
        String::from_utf8(clang_out.stderr).unwrap()
    );
    assert!(clang_out.status.success());

    binary_file.to_string_lossy().to_string()
}
fn compile_to_binary_c(src: &str, dir: &TempDir) -> String {
    let context = Context::create();
    let mut result = compile_or_panic(&context, src);

    let mut program = result.status.program().unwrap().clone();

    let c_code = CCodeGenerator::new(result.status.type_analysis_data().unwrap())
        .visit_program(&mut program);

    let c_file = dir.path().join("test.c");
    let binary_file = dir.path().join("test");

    println!("Writing C code to {c_file:?}");
    std::fs::write(&c_file, c_code).unwrap();

    println!("Calling clang to compile {c_file:?} to {binary_file:?}");
    let clang_out = Command::new("clang")
        .arg("-fdiagnostics-color=always")
        .arg(c_file.to_str().unwrap())
        .arg("-o")
        .arg(binary_file.to_str().unwrap())
        .output()
        .unwrap();
    print!(
        "Clang stdout:\n{}",
        String::from_utf8(clang_out.stdout).unwrap()
    );
    print!(
        "Clang stderr:\n{}",
        String::from_utf8(clang_out.stderr).unwrap()
    );
    assert!(clang_out.status.success());

    binary_file.to_str().unwrap().to_string()
}

fn run_and_check_output(
    binary: String,
    expected_exit_code: impl SubprocessTestableReturnType,
    expected_stdout: impl AsRef<str>,
    expected_stderr: impl AsRef<str>,
) {
    let output = Command::new(binary).output().unwrap();

    assert_eq!(
        output.status.code().unwrap() as i8,
        expected_exit_code.into_return_value(),
        "exit code doesn't match (left should be right)"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_stdout.as_ref(),
        "stdout doesn't match (left should be right)"
    );
    assert_eq!(
        String::from_utf8(output.stderr).unwrap(),
        expected_stderr.as_ref(),
        "stderr doesn't match (left should be right)"
    );
}

pub fn assert_compile_and_output_subprocess(
    src: &str,
    expected_exit_code: impl SubprocessTestableReturnType + Clone,
    expected_stdout: impl AsRef<str> + Clone,
    expected_stderr: impl AsRef<str> + Clone,
) {
    assert_is_formatted(src);
    println!("Source is formatted");

    {
        let llvm_tmpdir = tempdir().unwrap();
        let llvm_bin = compile_to_binary_llvm(src, &llvm_tmpdir);
        run_and_check_output(
            llvm_bin,
            expected_exit_code.clone(),
            expected_stdout.clone(),
            expected_stderr.clone(),
        );
        println!("LLVM subprocess execution succeeded");
    }

    {
        let c_tmpdir = tempdir().unwrap();
        let c_bin = compile_to_binary_c(src, &c_tmpdir);
        run_and_check_output(
            c_bin,
            expected_exit_code.clone(),
            expected_stdout.clone(),
            expected_stderr.clone(),
        );
        println!("C subprocess execution succeeded");
    }
}
