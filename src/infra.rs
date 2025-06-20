use std::error::Error;

use inkwell::{
    context::Context, module::Module, passes::PassBuilderOptions, targets::TargetMachine,
};
use itertools::Itertools;

use crate::{
    ast::{AstNode, AstVisitor, PerNodeData, Program},
    ast_to_dm::AstToDocumentModelConverter,
    document_model::{fully_flatten_document, stringify_document},
    ir_generator::IrGenerator,
    parser::{IdGenerator, Parser},
    passes::{
        err_missing_type_in_parameter::ErrMissingTypeInParam,
        find_node_bonds::find_node_bounds,
        fix_non_block_statements::FixNonBlockStatements,
        fix_trailing_comma::FixTrailingComma,
        function_termination_analysis::{FunctionTermination, FunctionTerminationAnalyzer},
        remove_parens::RemoveParensPass,
        type_propagation::{TypeAnalysisData, TypePropagator},
    },
    tokenizer::{SourceLocation, Token, Tokenizer},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ErrorSeverity {
    Error,
    Warning,
    Note,
}

#[derive(Clone, Debug)]
pub struct FleetError {
    pub start: SourceLocation,
    pub end: SourceLocation,
    pub message: String,
    pub severity: ErrorSeverity,
}

fn run_fix_passes(
    errors: &mut Vec<FleetError>,
    program: &mut Program,
    id_generator: &mut IdGenerator,
) {
    RemoveParensPass::default().visit_program(program);
    FixNonBlockStatements::new(errors, id_generator).visit_program(program);
    FixTrailingComma::new(errors, id_generator).visit_program(program);
    ErrMissingTypeInParam::new(errors).visit_program(program);
}

impl FleetError {
    pub fn from_token(token: &Token, msg: impl ToString, severity: ErrorSeverity) -> Self {
        Self {
            start: token.start,
            end: token.end,
            message: msg.to_string(),
            severity,
        }
    }
    pub fn from_node(
        node: impl Into<AstNode>,
        msg: impl ToString,
        severity: ErrorSeverity,
    ) -> Self {
        let (start, end) = find_node_bounds(node);
        Self {
            start,
            end,
            message: msg.to_string(),
            severity,
        }
    }
}

pub fn print_error_message(source: &str, error: &FleetError) {
    let ansi_color = match error.severity {
        ErrorSeverity::Error => "31",
        ErrorSeverity::Warning => "33",
        ErrorSeverity::Note => "34",
    };

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
            let start_col = if line == error.start.line {
                error.start.column
            } else {
                0
            };
            let end_col = if line == error.end.line {
                error.end.column.min(text.len())
            } else {
                text.len()
            };

            pad_with_line_number((
                line,
                format!(
                    "{}\x1B[{ansi_color}m{}\x1B[0m{}",
                    &text[..start_col],
                    &text[start_col..end_col],
                    &text[end_col..]
                )
                .as_str(),
            ))
        })
        .join("\n");
    let after_err = source_lines.skip(error.end.line);

    let before_err_trunc = before_err
        .skip(error.start.line.saturating_sub(num_before_error_lines + 1))
        .skip_while(|(_line, text)| text.trim() == "")
        .map(pad_with_line_number)
        .join("\n");
    let after_err_trunc = after_err
        .take(num_after_error_lines)
        .map(pad_with_line_number)
        .join("\n");

    println!(
        "\x1B[{ansi_color}m[FLEETC: {}] {}\x1B[0m",
        match error.severity {
            ErrorSeverity::Error => "ERROR",
            ErrorSeverity::Warning => "WARNING",
            ErrorSeverity::Note => "NOTE",
        },
        error.message
    );
    println!("{}", before_err_trunc);
    println!("{}", err);
    println!("{}\n", after_err_trunc);
}

#[derive(Clone, Debug)]
pub enum CompileStatus<'a> {
    ParserFailure {
        tokens: Vec<Token>,
    },
    TokenizerOrParserErrors {
        tokens: Vec<Token>,
        partial_parsed_program: Program,
        id_generator: IdGenerator,
    },
    AnalysisErrors {
        tokens: Vec<Token>,
        parsed_program: Program,
        parsed_id_generator: IdGenerator,
        program: Program,
        id_generator: IdGenerator,
        function_terminations: PerNodeData<FunctionTermination>,
        type_analysis_data: TypeAnalysisData,
    },
    IrGeneratorFailure {
        tokens: Vec<Token>,
        parsed_program: Program,
        parsed_id_generator: IdGenerator,
        program: Program,
        id_generator: IdGenerator,
        function_terminations: PerNodeData<FunctionTermination>,
        type_analysis_data: TypeAnalysisData,
    },
    IrGeneratorErrors {
        tokens: Vec<Token>,
        parsed_program: Program,
        parsed_id_generator: IdGenerator,
        program: Program,
        id_generator: IdGenerator,
        function_terminations: PerNodeData<FunctionTermination>,
        type_analysis_data: TypeAnalysisData,
        partial_module: Option<Module<'a>>,
    },
    Success {
        tokens: Vec<Token>,
        parsed_program: Program,
        parsed_id_generator: IdGenerator,
        program: Program,
        id_generator: IdGenerator,
        function_terminations: PerNodeData<FunctionTermination>,
        type_analysis_data: TypeAnalysisData,
        module: Module<'a>,
    },
}

impl<'a> CompileStatus<'a> {
    pub fn module(&self) -> Option<&Module<'a>> {
        match &self {
            CompileStatus::ParserFailure { .. } => None,
            CompileStatus::TokenizerOrParserErrors { .. } => None,
            CompileStatus::AnalysisErrors { .. } => None,
            CompileStatus::IrGeneratorFailure { .. } => None,
            CompileStatus::IrGeneratorErrors { partial_module, .. } => partial_module.as_ref(),
            CompileStatus::Success { module, .. } => Some(module),
        }
    }
    pub fn tokens(&self) -> Option<&Vec<Token>> {
        match &self {
            CompileStatus::ParserFailure { tokens } => Some(tokens),
            CompileStatus::TokenizerOrParserErrors { tokens, .. } => Some(tokens),
            CompileStatus::AnalysisErrors { tokens, .. } => Some(tokens),
            CompileStatus::IrGeneratorFailure { tokens, .. } => Some(tokens),
            CompileStatus::IrGeneratorErrors { tokens, .. } => Some(tokens),
            CompileStatus::Success { tokens, .. } => Some(tokens),
        }
    }
    pub fn program(&self) -> Option<&Program> {
        match &self {
            CompileStatus::ParserFailure { .. } => None,
            CompileStatus::TokenizerOrParserErrors {
                partial_parsed_program,
                ..
            } => Some(partial_parsed_program),
            CompileStatus::AnalysisErrors { program, .. } => Some(program),
            CompileStatus::IrGeneratorFailure { program, .. } => Some(program),
            CompileStatus::IrGeneratorErrors { program, .. } => Some(program),
            CompileStatus::Success { program, .. } => Some(program),
        }
    }
    // the program as it was parsed without any modifications
    pub fn parsed_program(&self) -> Option<&Program> {
        match &self {
            CompileStatus::ParserFailure { .. } => None,
            CompileStatus::TokenizerOrParserErrors {
                partial_parsed_program,
                ..
            } => Some(partial_parsed_program),
            CompileStatus::AnalysisErrors { parsed_program, .. } => Some(parsed_program),
            CompileStatus::IrGeneratorFailure { parsed_program, .. } => Some(parsed_program),
            CompileStatus::IrGeneratorErrors { parsed_program, .. } => Some(parsed_program),
            CompileStatus::Success { parsed_program, .. } => Some(parsed_program),
        }
    }
    pub fn id_generator(&self) -> Option<&IdGenerator> {
        match &self {
            CompileStatus::ParserFailure { .. } => None,
            CompileStatus::TokenizerOrParserErrors { id_generator, .. } => Some(id_generator),
            CompileStatus::AnalysisErrors { id_generator, .. } => Some(id_generator),
            CompileStatus::IrGeneratorFailure { id_generator, .. } => Some(id_generator),
            CompileStatus::IrGeneratorErrors { id_generator, .. } => Some(id_generator),
            CompileStatus::Success { id_generator, .. } => Some(id_generator),
        }
    }
    pub fn type_analysis_data(&self) -> Option<&TypeAnalysisData> {
        match &self {
            CompileStatus::ParserFailure { .. } => None,
            CompileStatus::TokenizerOrParserErrors { .. } => None,
            CompileStatus::AnalysisErrors {
                type_analysis_data, ..
            } => Some(type_analysis_data),
            CompileStatus::IrGeneratorFailure {
                type_analysis_data, ..
            } => Some(type_analysis_data),
            CompileStatus::IrGeneratorErrors {
                type_analysis_data, ..
            } => Some(type_analysis_data),
            CompileStatus::Success {
                type_analysis_data, ..
            } => Some(type_analysis_data),
        }
    }
    pub fn function_terminations(&self) -> Option<&PerNodeData<FunctionTermination>> {
        match &self {
            CompileStatus::ParserFailure { .. } => None,
            CompileStatus::TokenizerOrParserErrors { .. } => None,
            CompileStatus::AnalysisErrors {
                function_terminations,
                ..
            } => Some(function_terminations),
            CompileStatus::IrGeneratorFailure {
                function_terminations,
                ..
            } => Some(function_terminations),
            CompileStatus::IrGeneratorErrors {
                function_terminations,
                ..
            } => Some(function_terminations),
            CompileStatus::Success {
                function_terminations,
                ..
            } => Some(function_terminations),
        }
    }
    pub fn parsed_id_generator(&self) -> Option<&IdGenerator> {
        match &self {
            CompileStatus::ParserFailure { .. } => None,
            CompileStatus::TokenizerOrParserErrors { id_generator, .. } => Some(id_generator),
            CompileStatus::AnalysisErrors {
                parsed_id_generator,
                ..
            } => Some(parsed_id_generator),
            CompileStatus::IrGeneratorFailure {
                parsed_id_generator,
                ..
            } => Some(parsed_id_generator),
            CompileStatus::IrGeneratorErrors {
                parsed_id_generator,
                ..
            } => Some(parsed_id_generator),
            CompileStatus::Success {
                parsed_id_generator,
                ..
            } => Some(parsed_id_generator),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompileResult<'a> {
    pub status: CompileStatus<'a>,
    pub errors: Vec<FleetError>,
}

pub fn compile_program<'a>(context: &'a Context, src: &str) -> CompileResult<'a> {
    let mut errors = vec![];

    let tokens = Tokenizer::new(src.to_string(), &mut errors).tokenize();

    let Ok((mut program, mut id_generator)) =
        Parser::new(tokens.clone(), &mut errors).parse_program()
    else {
        return CompileResult {
            status: CompileStatus::ParserFailure { tokens },
            errors,
        };
    };

    let parsed_program = program.clone();
    let parsed_id_generator = id_generator.clone();

    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        return CompileResult {
            status: CompileStatus::TokenizerOrParserErrors {
                tokens,
                partial_parsed_program: parsed_program,
                id_generator: parsed_id_generator,
            },
            errors,
        };
    }

    // Analysis
    let term_analyzer = FunctionTerminationAnalyzer::new(&mut errors);
    let function_terminations = term_analyzer.visit_program(&mut program);
    let analysis_data =
        TypePropagator::new(&mut errors, &mut id_generator).visit_program(&mut program);

    run_fix_passes(&mut errors, &mut program, &mut id_generator);

    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        return CompileResult {
            status: CompileStatus::AnalysisErrors {
                tokens,
                parsed_program,
                program,
                type_analysis_data: analysis_data,
                function_terminations,
                parsed_id_generator,
                id_generator,
            },
            errors,
        };
    }

    let ir_generator =
        IrGenerator::new(context, &mut errors, &function_terminations, &analysis_data);
    let module = match ir_generator.visit_program(&mut program) {
        Ok(module) => module,
        Err(error) => {
            errors.push(FleetError {
                start: SourceLocation::start(),
                end: program
                    .functions
                    .first()
                    .map_or(SourceLocation::start(), |f| f.close_paren_token.end),
                message: match error.source() {
                    Some(source) => format!("{} ({:?})", error, source),
                    None => error.to_string(),
                },
                severity: ErrorSeverity::Error,
            });
            return CompileResult {
                status: CompileStatus::IrGeneratorFailure {
                    tokens,
                    parsed_program,
                    program,
                    type_analysis_data: analysis_data,
                    function_terminations,
                    parsed_id_generator,
                    id_generator,
                },
                errors,
            };
        }
    };

    if errors
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        return CompileResult {
            status: CompileStatus::IrGeneratorErrors {
                tokens,
                parsed_program,
                program,
                type_analysis_data: analysis_data,
                function_terminations,
                partial_module: if module.verify().is_ok() {
                    Some(module)
                } else {
                    None
                },
                parsed_id_generator,
                id_generator,
            },
            errors,
        };
    }

    CompileResult {
        status: CompileStatus::Success {
            tokens,
            parsed_program,
            program,
            type_analysis_data: analysis_data,
            function_terminations,
            module: module.clone(),
            parsed_id_generator,
            id_generator,
        },
        errors,
    }
}

pub fn format_program(mut program: Program, mut id_generator: IdGenerator) -> String {
    let mut errors = vec![];

    run_fix_passes(&mut errors, &mut program, &mut id_generator);

    let document = AstToDocumentModelConverter::default().visit_program(&mut program);
    stringify_document(&fully_flatten_document(document))
}

pub fn run_default_optimization_passes(
    module: &Module<'_>,
    target_machine: &TargetMachine,
) -> Result<(), Box<dyn Error>> {
    module.run_passes("default<O1>", target_machine, PassBuilderOptions::create())?;
    Ok(())
}
