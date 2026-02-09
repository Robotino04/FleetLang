use std::collections::HashMap;
use std::io::Write;
use std::path::PathBuf;
use std::{fs::read_to_string, process::exit};

use clap::{Parser, ValueEnum};

use fleet::NewtypeDerefNoDefault;
use fleet::ast::Program;
use fleet::ast_to_dm::AstToDocumentModelConverter;
use fleet::document_model::{DocumentElement, fully_flatten_document, stringify_document};
use fleet::infra::{
    ErrorKind, ErrorSeverity, HighlightGroup, RenderedError, insert_c_passes,
    insert_compile_passes, insert_fix_passes, insert_minimal_pipeline,
};
use fleet::inkwell::module::Module;
use fleet::inkwell::targets::{CodeModel, RelocMode, Target, TargetTriple};
use fleet::inkwell::{self, OptimizationLevel};
use fleet::ir_generator::{IrGenerator, LLVMOptimizerPass};
use fleet::passes::ast_json_dump::{AstJsonDumpPass, AstJsonOutput};
use fleet::passes::pass_manager::{Errors, InputSource, PassError, PassManager, StatData};
use fleet::passes::save_artifact_pass::{ArtifactType, SaveArtifactPass};
use fleet::passes::simple_function_pass::SingleFunctionPass;
use fleet::passes::store_pass::StorePass;
use fleet::passes::swap_pass::SwapPass;
use fleet::tokenizer::{FileName, SourceLocation, Token};
use itertools::Itertools;

#[allow(unused)]
fn generate_header(text: impl AsRef<str>, length: usize) -> String {
    format!("{:-^length$}", "|".to_string() + text.as_ref() + "|")
}
fn ansi_color_for_severity(severity: ErrorSeverity) -> String {
    format!(
        "\x1B[{}m",
        match severity {
            ErrorSeverity::Error => "31",
            ErrorSeverity::Warning => "33",
            ErrorSeverity::Note => "34",
        }
    )
}

const ANSI_RESET: &str = "\x1B[0m";

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

fn format_error(
    RenderedError {
        highlight_groups,
        main_message,
        severity: main_severity,
    }: &RenderedError,
    ansi: bool,
    documents: &HashMap<FileName, String>,
) -> String {
    let mut highlighted_sources = Vec::new();
    for HighlightGroup {
        severity,
        range,
        message,
        tags: _,
    } in highlight_groups
    {
        let color = if ansi {
            ansi_color_for_severity(*severity)
        } else {
            "".to_string()
        };

        let Some(source) = documents.get(&range.name) else {
            let red = if ansi {
                ansi_color_for_severity(ErrorSeverity::Error)
            } else {
                "".to_string()
            };

            let filename = &range.name.0;
            let SourceLocation {
                index: _,
                line,
                column,
            } = range.range.start;

            highlighted_sources.push(format!(
                "{filename}:{line}:{column}: {color}{message}{ANSI_RESET}\n\
                {red}[INTERNAL ERROR]{ANSI_RESET}: Ha, no fancy error for you! (I couldn't find the source code, sorry)"
            ));

            continue;
        };

        let prefix_lines = 2;
        let postfix_lines = 2;

        let start_line = range.start().line().saturating_sub(prefix_lines);
        let end_line = range.end().line() + postfix_lines;

        let filename = &range.name.0;
        let SourceLocation {
            index: _,
            line,
            column,
        } = range.range.start;

        let mut min_column = range
            .start()
            .column()
            .min(range.end().column().saturating_sub(1));
        let mut max_column = (range.start().column() + 1).max(range.end().column());

        let structure_color = if ansi {
            ansi_color_for_severity(ErrorSeverity::Note)
        } else {
            "".to_string()
        };

        let displayed_lines = source
            .lines()
            .enumerate()
            .map(|(nr, line)| (nr + 1, line))
            .skip(start_line.saturating_sub(1))
            .take(end_line - start_line)
            .filter(|(nr, line)| {
                if *nr >= range.start().line() && *nr <= range.end().line() {
                    true
                } else {
                    !line.trim().is_empty()
                }
            });

        let max_line_number_length =
            displayed_lines.clone().last().unwrap().0.to_string().len() + 1;

        let snippet = displayed_lines
            .map(|(nr, line)| {
                if nr > range.start().line() && nr < range.end().line() {
                    if let Some((local_min_column, _)) =
                        line.chars().find_position(|c| !c.is_whitespace())
                    {
                        min_column = min_column.min(local_min_column);
                    }
                    if let Some((local_max_column, _)) = line
                        .chars()
                        .enumerate()
                        .collect_vec()
                        .into_iter()
                        .rev()
                        .find(|(_, c)| !c.is_whitespace())
                    {
                        max_column = max_column.max(local_max_column + 1);
                    }
                }

                let line_colored = {
                    let mut out = String::new();
                    if nr > range.start().line() && nr <= range.end().line() {
                        out += &ansi_color_for_severity(*severity);
                    }
                    for (col, c) in line.chars().enumerate() {
                        if col == range.start().column() && nr == range.start().line() {
                            out += &ansi_color_for_severity(*severity);
                        }
                        if col == range.end().column() && nr == range.end().line() {
                            out += ANSI_RESET;
                        }
                        out.push(c);
                    }
                    if nr >= range.start().line() && nr < range.end().line() {
                        out += ANSI_RESET;
                    }
                    out
                };

                let mut out = format!(
                    "{structure_color}{nr:<max_line_number_length$}│{ANSI_RESET} {line_colored}"
                );
                if nr == range.end().line() {
                    let length = max_column - min_column;
                    out += &format!(
                        "\n{structure_color}{empty:<max_line_number_length$}│{ANSI_RESET} \
                        {empty:<min_column$}{color}{empty:^<length$} {message}{ANSI_RESET}",
                        empty = "",
                    );
                }

                out
            })
            .join("\n");

        highlighted_sources.push(format!(
            "{structure_color}{empty:─<max_line_number_length$}╭─ {filename}:{line}:{column}{ANSI_RESET}\n\
            {snippet}\n\
            {structure_color}{empty:─<max_line_number_length$}╯{ANSI_RESET}",
            empty = "",
        ));
    }

    let color = if ansi {
        ansi_color_for_severity(*main_severity)
    } else {
        "".to_string()
    };

    let severity = match *main_severity {
        ErrorSeverity::Note => "note",
        ErrorSeverity::Warning => "warning",
        ErrorSeverity::Error => "error",
    };

    let highlighted_sources = highlighted_sources.join("\n");

    format!(
        "{color}{severity}:{ANSI_RESET} {main_message}\n\n\
        {highlighted_sources}",
    )
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

    let print_all_errors_and_message = |msg, mut errors: Vec<ErrorKind>| {
        // error -> warning -> node, then sort by file location
        errors.sort_by_key(|err| {
            err.render()
                .highlight_groups
                .first()
                .unwrap()
                .range
                .range
                .start
        });
        errors.sort_by_key(|err| err.severity());

        if let Some(worst_severity) = errors.iter().map(|err| err.severity()).max() {
            for error in &errors {
                // double newline
                eprintln!("{}\n\n", format_error(&error.render(), true, &documents));
            }
            let ansi_color = ansi_color_for_severity(worst_severity);
            eprintln!("\n{ansi_color}{msg}{ANSI_RESET}");
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
        .any(|err| err.severity() == ErrorSeverity::Error)
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
