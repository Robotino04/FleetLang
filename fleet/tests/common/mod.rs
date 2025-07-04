#[cfg(test)]
use std::fmt::Debug;
use std::{
    ffi::{CStr, CString, c_char, c_void},
    mem::MaybeUninit,
    path::PathBuf,
    process::Command,
    ptr::null_mut,
};

use inkwell::{
    OptimizationLevel,
    context::Context,
    llvm_sys::support::LLVMLoadLibraryPermanently,
    memory_buffer::MemoryBuffer,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
};

use fleet::{
    infra::{
        AnalysisOutput, ErrorSeverity, FleetError, LLVMCompilationOutput, ParserOutput,
        TokenizerOutput,
    },
    tokenizer::SourceLocation,
};
use libc::{RTLD_DI_LINKMAP, RTLD_LAZY, dlclose, dlinfo, dlopen};
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
    let mut errors = vec![];
    let Some(tokenizer_output) = TokenizerOutput::new(src, &mut errors) else {
        panic!("Tokenizer failed completely");
    };
    let Ok(_parser_output) = tokenizer_output.parse(&mut errors) else {
        panic!("Tokenizer failed completely");
    };

    assert_error_at_position(&errors, error_start);
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

pub fn assert_no_fatal_errors(errors: &Vec<FleetError>) {
    assert!(
        !errors
            .iter()
            .any(|err| err.severity == ErrorSeverity::Error),
        "There are fatal errors: {errors:#?}"
    );
}

pub fn assert_compile_error(src: &str, error_start: SourceLocation) {
    assert_is_formatted(src);
    assert_compile_error_no_formatting(src, error_start);
}
pub fn assert_compile_error_no_formatting(src: &str, error_start: SourceLocation) {
    let mut errors = vec![];
    let Some(tokenizer_output) = TokenizerOutput::new(src, &mut errors) else {
        panic!("Tokenizer failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Ok(parser_output) = tokenizer_output.parse(&mut errors) else {
        panic!("Parser failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Some(_analysis_output) = parser_output.analyze(&mut errors) else {
        panic!("Analysis failed completely");
    };
    //_analysis_output.compile_llvm(&mut errors, &Context::create());

    assert_error_at_position(&errors, error_start);
}
pub fn assert_compile_and_warning(src: &str, warning_start: SourceLocation) {
    let context = Context::create();
    let mut errors = vec![];
    let Some(tokenizer_output) = TokenizerOutput::new(src, &mut errors) else {
        panic!("Tokenizer failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Ok(parser_output) = tokenizer_output.parse(&mut errors) else {
        panic!("Parser failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Some(analysis_output) = parser_output.analyze(&mut errors) else {
        panic!("Analysis failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Some(_llvm_output) = analysis_output.compile_llvm(&mut errors, &context) else {
        panic!("LLVM compilation failed completely");
    };
    assert_no_fatal_errors(&errors);

    assert_warning_at_position(&errors, warning_start);
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
    let (_tokenizer_output, _parser_output, _analysis_output, llvmcompilation_output) =
        compile_or_panic(&context, src);

    let actual_return_value: ReturnType = execute_function(&llvmcompilation_output, function_name);
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

fn compile_or_panic<'a>(
    context: &'a Context,
    src: &str,
) -> (
    TokenizerOutput,
    ParserOutput,
    AnalysisOutput,
    LLVMCompilationOutput<'a>,
) {
    let mut errors = vec![];
    let Some(tokenizer_output) = TokenizerOutput::new(src, &mut errors) else {
        panic!("Tokenizer failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Ok(parser_output) = tokenizer_output.parse(&mut errors) else {
        panic!("Parser failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Some(analysis_output) = parser_output.analyze(&mut errors) else {
        panic!("Analysis failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Some(llvm_output) = analysis_output.compile_llvm(&mut errors, context) else {
        panic!("LLVM compilation failed completely");
    };
    assert_no_fatal_errors(&errors);

    assert!(errors.is_empty(), "{:#?}", errors);

    llvm_output.module.verify().unwrap();

    (
        tokenizer_output,
        parser_output,
        analysis_output,
        llvm_output,
    )
}

fn format_or_panic(src: &str) -> String {
    let mut errors = vec![];
    let Some(tokenizer_output) = TokenizerOutput::new(src, &mut errors) else {
        panic!("Tokenizer failed completely");
    };
    assert_no_fatal_errors(&errors);
    let Ok(parser_output) = tokenizer_output.parse(&mut errors) else {
        panic!("Parser failed completely");
    };
    assert_no_fatal_errors(&errors);

    parser_output.format()
}

fn execute_or_panic<ReturnType>(src: &str, function_name: &str) -> ReturnType {
    let context = Context::create();
    let (_tokenizer_output, _parser_output, _analysis_output, llvmcompilation_output) =
        compile_or_panic(&context, src);

    execute_function::<ReturnType>(&llvmcompilation_output, function_name)
}

fn execute_function<ReturnType>(
    compilation_output: &LLVMCompilationOutput<'_>,
    function_name: &str,
) -> ReturnType {
    Target::initialize_native(&InitializationConfig::default())
        .expect("Failed to initialize native LLVM target");

    fn find_library(name: &str) -> PathBuf {
        #[repr(C)]
        struct LinkMap {
            l_addr: usize,
            l_name: *const c_char,
            l_ld: *mut c_void,
            l_next: *mut LinkMap,
            l_prev: *mut LinkMap,
        }

        unsafe {
            let library = dlopen(CString::new(name).unwrap().as_c_str().as_ptr(), RTLD_LAZY);
            if library.is_null() {
                panic!("Failed to open library");
            }
            let mut link_map: MaybeUninit<*mut LinkMap> = MaybeUninit::uninit();
            let result = dlinfo(
                library,
                RTLD_DI_LINKMAP,
                link_map.as_mut_ptr() as *mut c_void,
            );
            if result != 0 {
                dlclose(library);
                panic!("Failed to get link map");
            }
            let lib_path = CStr::from_ptr((*link_map.assume_init()).l_name)
                .to_str()
                .unwrap()
                .to_string();
            if dlclose(library) != 0 {
                panic!("Failed to close library");
            }
            lib_path.parse::<PathBuf>().unwrap()
        }
    }

    inkwell::support::load_library_permanently(&find_library("libstdc++.so")).unwrap();
    inkwell::support::load_library_permanently(&find_library("libvulkan.so")).unwrap();

    let dso_module = compilation_output
        .module
        .get_context()
        .create_module_from_ir(MemoryBuffer::create_from_memory_range(
            include_bytes!("../../../fl_runtime/dso_handle.bc"),
            "dso_module",
        ))
        .unwrap();
    compilation_output
        .module
        .link_in_module(dso_module)
        .unwrap();

    let execution_engine = compilation_output
        .module
        .create_jit_execution_engine(OptimizationLevel::None)
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
    let (_tokenizer_output, _parser_output, _analysis_output, llvmcompilation_output) =
        compile_or_panic(&context, src);

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

    llvmcompilation_output
        .run_default_optimization_passes(&target_machine)
        .unwrap();

    let object_file = dir.path().join("test.o");
    let binary_file = dir.path().join("test");

    target_machine
        .write_to_file(
            &llvmcompilation_output.module,
            FileType::Object,
            object_file.as_path(),
        )
        .unwrap();

    println!("Calling clang to link {object_file:?} to {binary_file:?}");
    let clang_out = Command::new("clang++")
        .arg("-fdiagnostics-color=always")
        .arg(object_file.to_str().unwrap())
        .args(["-o", binary_file.to_str().unwrap()])
        .arg("-lvulkan")
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
    let (_tokenizer_output, _parser_output, analysis_output, _llvmcompilation_output) =
        compile_or_panic(&context, src);

    let mut errors = vec![];
    let c_code = analysis_output.compile_c(&mut errors);
    assert_no_fatal_errors(&errors);

    let c_file = dir.path().join("test.c");
    let obj_file = dir.path().join("test.o");
    let binary_file = dir.path().join("test");

    println!("Writing C code to {c_file:?}");
    std::fs::write(&c_file, c_code).unwrap();

    let fl_runtime_header = dir.path().join("fl_runtime.h");
    std::fs::write(
        &fl_runtime_header,
        include_bytes!("../../../fl_runtime/fl_runtime.h"),
    )
    .unwrap();
    let fl_runtime_obj = dir.path().join("fl_runtime.o");
    std::fs::write(
        &fl_runtime_obj,
        include_bytes!("../../../fl_runtime/fl_runtime.o"),
    )
    .unwrap();

    println!("Calling clang to compile {c_file:?} to {obj_file:?}");
    let clang_out = Command::new("clang++")
        .arg("-fdiagnostics-color=always")
        .arg("-c")
        .args(["-x", "c"]) // important for compound literals to have the correct semantics
        .arg(c_file.to_str().unwrap())
        .args(["-o", obj_file.to_str().unwrap()])
        .arg(format!(
            "-I{}",
            fl_runtime_header.parent().unwrap().display()
        ))
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

    println!("Calling clang to link {obj_file:?}, {fl_runtime_obj:?} to {binary_file:?}");
    let clang_out = Command::new("clang++")
        .arg("-fdiagnostics-color=always")
        .arg(obj_file.to_str().unwrap())
        .arg(fl_runtime_obj)
        .arg("-lvulkan")
        .args(["-o", binary_file.to_str().unwrap()])
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
