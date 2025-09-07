#[cfg(test)]
use std::fmt::Debug;
use std::{
    ffi::{CStr, CString, c_char, c_void},
    fs::File,
    io::Read,
    mem::MaybeUninit,
    path::PathBuf,
    process::{Command, Stdio},
};

use inkwell::{
    OptimizationLevel,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetTriple},
};

use fleet::{
    ast::Program,
    infra::{
        ErrorSeverity, FleetError, format, insert_c_passes, insert_compile_passes,
        insert_fix_passes, insert_minimal_pipeline,
    },
    ir_generator::{IrGenerator, LLVMOptimizerPass},
    passes::{
        pass_manager::{Errors, InputSource, PassError, PassManager, StatData},
        save_artifact_pass::{ArtifactType, SaveArtifactPass},
    },
    tokenizer::SourceLocation,
};
use itertools::Itertools;
use libc::{RTLD_DI_LINKMAP, RTLD_LAZY, dlclose, dlinfo, dlopen, mkfifo};
use tempfile::{TempDir, tempdir};

pub trait SubprocessTestableReturnType {
    fn expected_bytes(self) -> Vec<u8>;
    fn type_signifier(&self) -> String;
}
impl SubprocessTestableReturnType for i64 {
    fn expected_bytes(self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn type_signifier(&self) -> String {
        "i64".to_string()
    }
}
impl SubprocessTestableReturnType for i32 {
    fn expected_bytes(self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn type_signifier(&self) -> String {
        "i32".to_string()
    }
}
impl SubprocessTestableReturnType for i16 {
    fn expected_bytes(self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn type_signifier(&self) -> String {
        "i16".to_string()
    }
}
impl SubprocessTestableReturnType for i8 {
    fn expected_bytes(self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn type_signifier(&self) -> String {
        "i8".to_string()
    }
}
impl SubprocessTestableReturnType for bool {
    fn expected_bytes(self) -> Vec<u8> {
        if self { vec![0x01] } else { vec![0x00] }
    }
    fn type_signifier(&self) -> String {
        "bool".to_string()
    }
}
impl SubprocessTestableReturnType for f32 {
    fn expected_bytes(self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn type_signifier(&self) -> String {
        "f32".to_string()
    }
}
impl SubprocessTestableReturnType for f64 {
    fn expected_bytes(self) -> Vec<u8> {
        self.to_ne_bytes().to_vec()
    }
    fn type_signifier(&self) -> String {
        "f64".to_string()
    }
}

pub fn assert_parser_or_tokenizer_error(src: &str, error_start: SourceLocation) {
    let mut pm = PassManager::default();
    let errors = pm.state.insert_default::<Errors>();
    pm.state.insert(InputSource(src.to_string()));

    insert_minimal_pipeline(&mut pm);

    pm.run().unwrap();

    let errors = errors.get(&pm.state);

    assert_error_at_position(&errors.0, error_start);
}

fn assert_error_at_position(errors: &Vec<FleetError>, error_start: SourceLocation) {
    assert!(
        errors
            .iter()
            .flat_map(|err| err
                .highlight_groups
                .iter()
                .map(|range| (range, err.severity)))
            .any(|(range, severity)| range.start == error_start
                && severity == ErrorSeverity::Error),
        "Expected an error at {error_start:?}. Got {errors:#?}"
    );
}

fn assert_warning_at_position(errors: &Vec<FleetError>, warning_start: SourceLocation) {
    assert!(
        errors
            .iter()
            .flat_map(|warn| warn
                .highlight_groups
                .iter()
                .map(|range| (range, warn.severity)))
            .any(|(range, severity)| range.start == warning_start
                && severity == ErrorSeverity::Warning),
        "Expected a warning at {warning_start:?}. Got {errors:#?}"
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
    let mut pm = PassManager::default();
    let errors = pm.state.insert_default::<Errors>();
    pm.state.insert(InputSource(src.to_string()));

    insert_minimal_pipeline(&mut pm);
    insert_fix_passes(&mut pm);
    insert_compile_passes(&mut pm);
    pm.insert::<IrGenerator>();

    match pm.run() {
        Err(PassError::InvalidInput { .. }) => {}
        Err(err @ (PassError::CompilerError { .. } | PassError::PassManagerStall { .. })) => {
            panic!("Should have produced an InvalidInput error. Got: {err}")
        }
        Ok(()) => panic!("Should have produced an InvalidInput error. Got: Ok(())"),
    }

    let errors = errors.get(&pm.state);

    assert_error_at_position(&errors.0, error_start);
}
pub fn assert_compile_and_warning(src: &str, warning_start: SourceLocation) {
    let mut pm = PassManager::default();
    let errors = pm.state.insert_default::<Errors>();
    pm.state.insert(InputSource(src.to_string()));

    insert_minimal_pipeline(&mut pm);
    insert_fix_passes(&mut pm);
    insert_compile_passes(&mut pm);
    pm.insert::<IrGenerator>();

    pm.run().unwrap();

    let errors = errors.get(&pm.state);

    assert_no_fatal_errors(&errors.0);
    assert_warning_at_position(&errors, warning_start);
}

pub fn assert_compile_and_return_value<ReturnType>(
    src: &str,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq + SubprocessTestableReturnType + Clone,
{
    assert_eq!(
        function_name, "main",
        "Only the main function can be tested in a subprocess"
    );
    assert_is_formatted(src.trim());
    println!("Source is formatted");
    assert_compile_and_return_value_unformatted::<ReturnType>(
        src,
        function_name,
        expected_return_value.clone(),
    );
    println!("LLVM execution succeeded");
    assert_compile_and_output_subprocess(src, expected_return_value, "", "");
    println!("Subprocess execution succeeded");
}

pub fn assert_compile_and_return_value_unformatted<ReturnType>(
    src: &str,
    function_name: &str,
    expected_return_value: ReturnType,
) where
    ReturnType: Debug + PartialEq,
{
    let pm = compile_or_panic(src);

    let needs_runtime = pm
        .state
        .get::<StatData>()
        .expect("Compilation failed before inserting stats")
        .get(&pm.state.get::<Program>().unwrap().id)
        .expect("No stats available for function to test")
        .uses_gpu
        .at_least_maybe();

    assert_eq!(function_name, "main");
    let function_name = "fleet_main";

    let actual_return_value = execute_function::<ReturnType>(
        &pm.state.get::<Module>().unwrap(),
        function_name,
        needs_runtime,
    );
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
    assert_eq!(function_name, "main");
    let function_name = "fleet_main";

    let formatted_src = assert_formatting(src, expected_fmt);

    let unformatted_retvalue = execute_or_panic::<ReturnType>(src, function_name);
    let formatted_retvalue = execute_or_panic::<ReturnType>(formatted_src, function_name);

    assert_eq!(
        unformatted_retvalue, formatted_retvalue,
        "formatting changed behaviour from returning left to right"
    );
}

fn compile_or_panic(src: &str) -> PassManager {
    let mut pm = PassManager::default();
    let errors = pm.state.insert_default::<Errors>();
    pm.state.insert(InputSource(src.to_string()));

    insert_minimal_pipeline(&mut pm);
    insert_fix_passes(&mut pm);
    insert_compile_passes(&mut pm);

    insert_c_passes(&mut pm);
    pm.insert::<IrGenerator>();

    pm.run().unwrap();

    assert_no_fatal_errors(&errors.get(&pm.state).0);
    pm.state.get::<Module>().unwrap().verify().unwrap();

    pm
}

fn format_or_panic(src: &str) -> String {
    format(src.to_string()).unwrap()
}

fn execute_or_panic<ReturnType>(src: &str, function_name: &str) -> ReturnType {
    let pm = compile_or_panic(src);

    let needs_runtime = pm
        .state
        .get::<StatData>()
        .unwrap()
        .get(&pm.state.get::<Program>().unwrap().id)
        .expect("No stats available for function to test")
        .uses_gpu
        .at_least_maybe();

    execute_function::<ReturnType>(
        &pm.state.get::<Module>().unwrap(),
        function_name,
        needs_runtime,
    )
}

fn execute_function<ReturnType>(
    module: &Module<'_>,
    function_name: &str,
    needs_runtime: bool,
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

    if needs_runtime {
        eprintln!("Linking in vulkan and runtime");
        let dir = TempDir::new().unwrap();

        let fl_runtime_so = dir.path().join("fl_runtime.so");
        std::fs::write(
            &fl_runtime_so,
            include_bytes!("../../../fl_runtime/fl_runtime.so"),
        )
        .unwrap();

        inkwell::support::load_library_permanently(&find_library("libvulkan.so")).unwrap();
        inkwell::support::load_library_permanently(fl_runtime_so.as_path()).unwrap();
        inkwell::support::load_library_permanently(&find_library("libstdc++.so")).unwrap();
    }

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    let initialize_fleet = unsafe {
        execution_engine
            .get_function::<unsafe extern "C" fn() -> ()>("initialize_fleet")
            .unwrap()
    };
    let deinitialize_fleet = unsafe {
        execution_engine
            .get_function::<unsafe extern "C" fn() -> ()>("deinitialize_fleet")
            .unwrap()
    };
    let main_function = unsafe {
        execution_engine
            .get_function::<unsafe extern "C" fn() -> ReturnType>(function_name)
            .unwrap()
    };

    unsafe {
        initialize_fleet.call();
        eprintln!("initialized");
        let retvalue = main_function.call();
        eprintln!("executed");
        deinitialize_fleet.call();
        eprintln!("deinitialized");
        retvalue
    }
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
    let mut pm = compile_or_panic(src);

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

    let object_file = dir.path().join("test.o");
    let binary_file = dir.path().join("test");

    let target_machine = pm.state.insert(target_machine);
    pm.insert::<LLVMOptimizerPass>();
    pm.insert_params::<SaveArtifactPass>((object_file.clone(), ArtifactType::Object));
    pm.run().unwrap();

    let _target_machine = target_machine.get(&pm.state);

    let needs_runtime = pm
        .state
        .get::<StatData>()
        .unwrap()
        .get(&pm.state.get::<Program>().unwrap().id)
        .expect("No stats-rdynamic available for function to test")
        .uses_gpu
        .at_least_maybe();

    let mut clang_cmd = Command::new("clang++");
    clang_cmd
        .arg("-fdiagnostics-color=always")
        .arg("-rdynamic")
        .args([
            "-Wreturn-stack-address",
            "-fsanitize=undefined",
            "-fsanitize=address",
            "-fstack-protector-all",
            "-fsanitize-address-use-after-return=always",
            "-fno-omit-frame-pointer",
            "-g",
        ])
        .arg(object_file.to_str().unwrap())
        .args(["-o", binary_file.to_str().unwrap()]);

    if needs_runtime {
        let fl_runtime_obj = dir.path().join("fl_runtime.o");
        std::fs::write(
            &fl_runtime_obj,
            include_bytes!("../../../fl_runtime/fl_runtime.o"),
        )
        .unwrap();

        clang_cmd.arg(fl_runtime_obj.to_str().unwrap());
        clang_cmd.arg("-lvulkan");

        println!("Calling clang to link {object_file:?} and {fl_runtime_obj:?} to {binary_file:?}");
    } else {
        println!("Calling clang to link {object_file:?} to {binary_file:?}");
    }

    let clang_out = clang_cmd.output().unwrap();

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
    let c_file = dir.path().join("test.c");
    let obj_file = dir.path().join("test.o");
    let binary_file = dir.path().join("test");

    let mut pm = compile_or_panic(src);
    pm.insert_params::<SaveArtifactPass>((c_file.clone(), ArtifactType::CCode));
    pm.run().unwrap();

    let needs_runtime = pm
        .state
        .get::<StatData>()
        .unwrap()
        .get(&pm.state.get::<Program>().unwrap().id)
        .expect("No stats available for function to test")
        .uses_gpu
        .at_least_maybe();

    let mut clang_compile = Command::new("clang++");
    clang_compile
        .arg("-fdiagnostics-color=always")
        .args([
            "-Wreturn-stack-address",
            "-fsanitize=undefined",
            "-fsanitize=address",
            "-fstack-protector-all",
            "-fsanitize-address-use-after-return=always",
            "-fno-omit-frame-pointer",
            "-g",
        ])
        .arg("-c")
        .args(["-x", "c"]) // important for compound literals to have the correct semantics
        .arg(c_file.to_str().unwrap())
        .args(["-o", obj_file.to_str().unwrap()]);

    let mut clang_link = Command::new("clang++");
    clang_link
        .arg("-fdiagnostics-color=always")
        .arg("-rdynamic")
        .args([
            "-Wreturn-stack-address",
            "-fsanitize=undefined",
            "-fsanitize=address",
            "-fstack-protector-all",
            "-fsanitize-address-use-after-return=always",
            "-fno-omit-frame-pointer",
            "-g",
        ])
        .arg(obj_file.to_str().unwrap())
        .args(["-o", binary_file.to_str().unwrap()]);

    if needs_runtime {
        eprintln!("Linking in vulkan and runtime");
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
        clang_compile.arg(format!(
            "-I{}",
            fl_runtime_header.parent().unwrap().display()
        ));

        clang_link.arg(fl_runtime_obj);
        clang_link.arg("-lvulkan");
    }

    println!("Calling clang to compile {c_file:?} to {obj_file:?}");
    let clang_compile_out = clang_compile.output().unwrap();
    print!(
        "Clang stdout:\n{}",
        String::from_utf8(clang_compile_out.stdout).unwrap()
    );
    print!(
        "Clang stderr:\n{}",
        String::from_utf8(clang_compile_out.stderr).unwrap()
    );
    assert!(clang_compile_out.status.success());

    println!("Calling clang to link {obj_file:?} to {binary_file:?}");
    let clang_link_out = clang_link.output().unwrap();
    print!(
        "Clang stdout:\n{}",
        String::from_utf8(clang_link_out.stdout).unwrap()
    );
    print!(
        "Clang stderr:\n{}",
        String::from_utf8(clang_link_out.stderr).unwrap()
    );
    assert!(clang_link_out.status.success());

    binary_file.to_str().unwrap().to_string()
}

fn clean_stderr(stderr: String) -> String {
    // HACK: filter out lines from mesa that look like this
    //     pci id for fd 9: 10de:2684, driver (null)
    // TODO: remove this whenever mesa doesn't complain anymore
    stderr
        .lines()
        .skip_while(|line| line.starts_with("pci id for fd ") && line.ends_with(", driver (null)"))
        .join("\n")
}

fn run_and_check_output(
    binary: String,
    expected_result: impl SubprocessTestableReturnType,
    expected_stdout: impl AsRef<str>,
    expected_stderr: impl AsRef<str>,
    dir: &TempDir,
) {
    let testhook_lib = dir.path().join("testhook.so");
    std::fs::write(
        &testhook_lib,
        include_bytes!("../../../fl_runtime/testhook.so"),
    )
    .unwrap();

    let child_pipe_path = dir.path().join("child_pipe");

    if unsafe {
        mkfifo(
            CString::new(child_pipe_path.to_str().unwrap())
                .unwrap()
                .as_c_str()
                .as_ptr(),
            0o666,
        )
    } != 0
    {
        panic!("Failed to create named pipe");
    };

    println!("Starting testee");
    let cmd = Command::new(binary)
        .env("FLEETC_TEST_PIPE", &child_pipe_path)
        .env("FLEETC_TEST_TYPE", expected_result.type_signifier())
        .env("LD_PRELOAD", &testhook_lib)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    println!("opening file");
    let mut child_pipe = File::open(child_pipe_path).unwrap();
    println!("opened file");

    let expected = expected_result.expected_bytes();

    let result = {
        let mut result = vec![0u8; expected.len()];
        child_pipe.read_exact(&mut result).map(|_| result)
    };

    let output = cmd.wait_with_output().unwrap();

    println!("Testee finished");

    println!(
        "stdout:\n{}",
        String::from_utf8(output.stdout.clone()).unwrap()
    );
    println!(
        "stderr:\n{}",
        String::from_utf8(output.stderr.clone()).unwrap()
    );

    let result = result.unwrap();

    assert_eq!(
        result, expected,
        "exit code doesn't match (left should be right)"
    );
    assert_eq!(
        String::from_utf8(output.stdout).unwrap(),
        expected_stdout.as_ref(),
        "stdout doesn't match (left should be right)"
    );
    assert_eq!(
        clean_stderr(String::from_utf8(output.stderr).unwrap()),
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
            &llvm_tmpdir,
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
            &c_tmpdir,
        );
        println!("C subprocess execution succeeded");
    }
}
