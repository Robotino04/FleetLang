use std::io::Write;
use std::path::PathBuf;
use std::{fs::read_to_string, process::exit};

use clap::{Parser, ValueEnum};

use fleet::NewtypeDerefNoDefault;
use fleet::ast::Program;
use fleet::ast_to_dm::AstToDocumentModelConverter;
use fleet::document_model::{DocumentElement, fully_flatten_document, stringify_document};
use fleet::error_reporting::{ErrorSeverity, Errors};
use fleet::infra::{
    insert_c_passes, insert_compile_passes, insert_fix_passes, insert_minimal_pipeline,
};
use fleet::inkwell::module::Module;
use fleet::inkwell::targets::{CodeModel, RelocMode, Target, TargetTriple};
use fleet::inkwell::{self, OptimizationLevel};
use fleet::ir_generator::{IrGenerator, LLVMOptimizerPass};
use fleet::passes::ast_json_dump::{AstJsonDumpPass, AstJsonOutput};
use fleet::passes::pass_manager::{InputSource, PassError, PassManager, StatData};
use fleet::passes::save_artifact_pass::{ArtifactType, SaveArtifactPass};
use fleet::passes::simple_function_pass::SingleFunctionPass;
use fleet::passes::store_pass::StorePass;
use fleet::passes::swap_pass::SwapPass;
use fleet::tokenizer::{FileName, Token};

#[allow(unused)]
fn generate_header(text: impl AsRef<str>, length: usize) -> String {
    format!("{:-^length$}", "|".to_string() + text.as_ref() + "|")
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
    /*
    let cli = Cli {
        input_file: "./test.fl".parse().unwrap(),
        output: None,
        dump: vec![],
        format: false,
        verbosity: Default::default()
    };
    */
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

    let source = src.to_string();
    let file_name: FileName = cli.input_file.to_string_lossy().to_string().into();

    let documents = vec![(file_name.clone(), source.clone())]
        .into_iter()
        .collect();

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
                let src = src.clone();
                pm.insert_params::<SingleFunctionPass<_, _>>(move |dm: &DocumentElement| {
                    let formatted = stringify_document(fully_flatten_document(dm.clone()));
                    std::fs::write(output_file_name, &formatted).unwrap();
                    if formatted != src {
                        exit(1)
                    }
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
    pm.insert_params::<SaveArtifactPass>((cli.input_file.with_extension("c"), ArtifactType::CCode));

    pm.insert::<IrGenerator>();
    pm.insert_params::<SaveArtifactPass>((
        cli.input_file.with_extension("unopt.ll"),
        ArtifactType::LlvmIr,
    ));
    pm.insert::<LLVMOptimizerPass>();
    pm.insert::<StorePass<Module, OptimizedModule>>();
    pm.insert_params::<SaveArtifactPass>((
        cli.input_file.with_extension("opt.ll"),
        ArtifactType::LlvmIr,
    ));

    pm.insert::<SwapPass<Program, FixedProgram>>();
    pm.insert::<AstToDocumentModelConverter>();
    pm.insert::<SwapPass<Program, FixedProgram>>();

    let errors = pm.state.insert_default::<Errors>();
    pm.state.insert(InputSource { source, file_name });
    let _target_machine = pm.state.insert(target_machine);

    match pm.run() {
        Err(err @ (PassError::CompilerError { .. } | PassError::PassManagerStall { .. })) => {
            let mut errors = errors.get(&pm.state).clone();
            eprintln!(
                "{}",
                errors.format_all_errors_and_message(
                    "Compilation failed internally",
                    &documents,
                    true,
                )
            );
            eprintln!("{err}");
            exit(1);
        }
        Err(PassError::InvalidInput { .. }) => {
            let mut errors = errors.get(&pm.state).clone();
            eprintln!(
                "{}",
                errors.format_all_errors_and_message("Program has errors", &documents, true),
            );
            exit(1);
        }
        Ok(()) => (),
    };

    let mut errors = errors.get(&pm.state).clone();
    if errors
        .iter()
        .any(|err| err.severity() == ErrorSeverity::Error)
    {
        eprintln!(
            "{}",
            errors.format_all_errors_and_message("Compilation has errors", &documents, true),
        );
        panic!("these errors should have resulted in a PassError::InvalidInput");
    }

    if !(errors.is_empty()) {
        eprintln!(
            "{}",
            errors.format_all_errors_and_message("There are warnings", &documents, true),
        );
    }
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
