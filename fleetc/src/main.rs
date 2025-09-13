use std::io::Write;
use std::path::PathBuf;
use std::{fs::read_to_string, process::exit};

use clap::{Parser, ValueEnum};

use fleet::NewtypeDerefNoDefault;
use fleet::ast::Program;
use fleet::ast_to_dm::AstToDocumentModelConverter;
use fleet::document_model::{DocumentElement, fully_flatten_document, stringify_document};
use fleet::infra::{
    ErrorSeverity, FleetError, insert_c_passes, insert_compile_passes, insert_fix_passes,
    insert_minimal_pipeline,
};
use fleet::inkwell::module::Module;
use fleet::inkwell::targets::{CodeModel, RelocMode, Target, TargetTriple};
use fleet::inkwell::{self, OptimizationLevel};
use fleet::ir_generator::{IrGenerator, LLVMOptimizerPass};
use fleet::passes::pass_manager::{Errors, InputSource, PassError, PassManager, StatData};
use fleet::passes::save_artifact_pass::{ArtifactType, SaveArtifactPass};
use fleet::passes::simple_function_pass::SingleFunctionPass;
use fleet::passes::store_pass::StorePass;
use fleet::passes::swap_pass::SwapPass;
use fleet::tokenizer::Token;

use crate::ast_json_dump::{AstJsonDumpPass, AstJsonOutput};

mod ast_json_dump;

#[allow(unused)]
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
NewtypeDerefNoDefault!(pub OptimizedModule, Module<'static>);

#[derive(Parser)]
#[command(version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    /// The file to compile
    input_file: PathBuf,

    /// The file to write to
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Dump intermediate representations instead of compiling to a binary. Can be repeated
    #[arg(short, long, group = "output_type")]
    dump: Vec<DumpOption>,

    /// Format the file instead of compiling it
    #[arg(short, long, group = "output_type")]
    format: bool,

    #[command(flatten)]
    verbosity: clap_verbosity_flag::Verbosity,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum DumpOption {
    /// Dump the AST as JSON arrays
    AstJson,
    /// Dump the generated C code
    CCode,
    /// Dump the document model before flattening
    DocumentModel,
    /// Dump the document model after flattening
    DocumentModelFlat,
    /// Dump the parsed AST before any modifications
    AstRaw,
    /// Dump the fully transformed AST
    AstFull,
    /// Dump the unoptimized LLVM IR
    LlvmIr,
    /// Dump the optimized LLVM IR
    LlvmIrOptimized,
    /// Dump the parsed tokens
    Tokens,
    /// Dump statistics on AST nodes
    NodeStats,
}

fn main() {
    let cli = Cli::parse();
    env_logger::Builder::new()
        .format(|fmt, record| {
            let level = record.level();
            let level_style = fmt.default_level_style(level);

            writeln!(
                fmt,
                "{level_style}[{level}]{level_style:#} {}",
                record.args()
            )
        })
        .filter_level(cli.verbosity.into())
        .parse_default_env()
        .init();

    let src = read_to_string(&cli.input_file).unwrap_or_else(|_| {
        eprintln!(
            "Input file {:?} doesn't exist or isn't readable",
            cli.input_file
        );
        exit(1);
    });

    let print_all_errors_and_message = |msg, mut errors: Vec<FleetError>| {
        // error -> warning -> node, then sort by file location
        errors.sort_by_key(|err| err.highlight_groups().first().unwrap().start);
        errors.sort_by_key(|err| err.severity);

        if let Some(worst_severity) = errors.iter().map(|err| err.severity).max() {
            for error in &errors {
                // double newline
                eprintln!("{}\n", error.to_string_ansi(&src));
            }
            let ansi_color = ansi_color_for_severity(worst_severity);
            eprintln!("\x1B[{ansi_color}m{msg}\x1B[0m");
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

    // insert output passes first so partial results still get output
    {
        // TODO: this can't be the best option
        let output_file_name = cli.output.clone().unwrap_or("/dev/stdout".parse().unwrap());

        if cli.dump.is_empty() {
            if cli.format {
                let output_file_name = output_file_name.clone();
                pm.insert_params::<SingleFunctionPass<_, _>>(|dm: &DocumentElement| {
                    std::fs::write(
                        output_file_name,
                        stringify_document(fully_flatten_document(dm.clone())),
                    )
                    .unwrap();
                    Ok(())
                });
            } else {
                pm.insert_params::<SaveArtifactPass>((
                    cli.output.unwrap_or(cli.input_file.with_extension("o")),
                    ArtifactType::Object,
                ));
            }
        }

        for dump_type in &cli.dump {
            let output_file_name = output_file_name.clone();
            match dump_type {
                DumpOption::CCode => {
                    pm.insert_params::<SaveArtifactPass>((output_file_name, ArtifactType::CCode));
                }
                DumpOption::AstRaw => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|rp: &RawProgram| {
                        std::fs::write(output_file_name, format!("{rp:#?}\n")).unwrap();
                        Ok(())
                    });
                }
                DumpOption::LlvmIr => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|module: &Module| {
                        std::fs::write(output_file_name, module.to_string() + "\n").unwrap();
                        Ok(())
                    });
                }
                DumpOption::Tokens => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|tokens: &Vec<Token>| {
                        std::fs::write(output_file_name, format!("{tokens:#?}\n")).unwrap();
                        Ok(())
                    });
                }
                DumpOption::AstFull => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|program: &Program| {
                        std::fs::write(output_file_name, format!("{program:#?}\n")).unwrap();
                        Ok(())
                    });
                }
                DumpOption::AstJson => {
                    pm.insert::<AstJsonDumpPass>();
                    pm.insert_params::<SingleFunctionPass<_, _>>(|out: &AstJsonOutput| {
                        std::fs::write(output_file_name, out.0.clone()).unwrap();
                        Ok(())
                    });
                }
                DumpOption::DocumentModel => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|dm: &DocumentElement| {
                        std::fs::write(output_file_name, format!("{dm:#?}\n")).unwrap();
                        Ok(())
                    });
                }
                DumpOption::LlvmIrOptimized => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|module: &OptimizedModule| {
                        std::fs::write(output_file_name, module.to_string() + "\n").unwrap();
                        Ok(())
                    });
                }
                DumpOption::DocumentModelFlat => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|dm: &DocumentElement| {
                        std::fs::write(
                            output_file_name,
                            format!("{:#?}\n", fully_flatten_document(dm.clone())),
                        )
                        .unwrap();
                        Ok(())
                    });
                }
                DumpOption::NodeStats => {
                    pm.insert_params::<SingleFunctionPass<_, _>>(|stats: &StatData| {
                        std::fs::write(output_file_name, format!("{stats:#?}\n")).unwrap();
                        Ok(())
                    });
                }
            }
        }
    }

    insert_minimal_pipeline(&mut pm);
    pm.insert::<StorePass<Program, RawProgram>>();
    insert_fix_passes(&mut pm);
    pm.insert::<StorePass<Program, FixedProgram>>();
    insert_compile_passes(&mut pm);
    insert_c_passes(&mut pm);
    pm.insert_params::<SaveArtifactPass>(("./out.c".into(), ArtifactType::CCode));

    pm.insert::<IrGenerator>();
    pm.insert_params::<SaveArtifactPass>(("./out_unopt.ll".into(), ArtifactType::LlvmIr));
    pm.insert::<LLVMOptimizerPass>();
    pm.insert::<StorePass<Module, OptimizedModule>>();
    pm.insert_params::<SaveArtifactPass>(("./out_opt.ll".into(), ArtifactType::LlvmIr));

    pm.insert::<SwapPass<Program, FixedProgram>>();
    pm.insert::<AstToDocumentModelConverter>();
    pm.insert::<SwapPass<Program, FixedProgram>>();

    let errors = pm.state.insert_default::<Errors>();
    pm.state.insert(InputSource {
        source: src.to_string(),
        file_name: cli.input_file.to_string_lossy().to_string().into(),
    });
    let _target_machine = pm.state.insert(target_machine);

    match pm.run() {
        Err(err @ (PassError::CompilerError { .. } | PassError::PassManagerStall { .. })) => {
            let errors = errors.get(&pm.state).clone();
            print_all_errors_and_message("Compilation failed internally", errors.into());
            eprintln!("{err}");
            exit(1);
        }
        Err(PassError::InvalidInput { .. }) => {
            let errors = errors.get(&pm.state).clone();
            print_all_errors_and_message("Program has errors", errors.into());
            exit(1);
        }
        Ok(()) => (),
    };

    let errors = errors.get(&pm.state).clone();
    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        print_all_errors_and_message("Compilation has errors", errors.into());
        panic!("these errors should have resulted in a PassError::InvalidInput");
    }

    print_all_errors_and_message("There are warnings", errors.into());
}

#[cfg(test)]
mod test {
    use crate::Cli;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Cli::command().debug_assert();
    }
}
