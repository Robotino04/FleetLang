use std::error::Error;

#[cfg(feature = "llvm_backend")]
use inkwell::{context::Context, module::Module, targets::TargetMachine};
use itertools::Itertools;

use crate::{
    ast::{AstNode, AstVisitor, PerNodeData, Program},
    ast_to_dm::AstToDocumentModelConverter,
    document_model::{fully_flatten_document, stringify_document},
    generate_c::CCodeGenerator,
    parser::{IdGenerator, Parser, ParserError},
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
pub struct TokenizerOutput {
    pub tokens: Vec<Token>,
}

impl TokenizerOutput {
    pub fn new(src: &str, errors: &mut Vec<FleetError>) -> Option<Self> {
        let tokens = Tokenizer::new(src.to_string(), errors).tokenize();

        Some(Self { tokens })
    }

    pub fn parse(&self, errors: &mut Vec<FleetError>) -> Result<ParserOutput, ParserError> {
        Parser::new(self.tokens.clone(), errors)
            .parse_program()
            .map(|(program, id_generator)| ParserOutput {
                program,
                id_generator,
            })
    }
}

#[derive(Clone, Debug)]
pub struct ParserOutput {
    pub program: Program,
    pub id_generator: IdGenerator,
}

impl ParserOutput {
    pub fn analyze(&self, errors: &mut Vec<FleetError>) -> Option<AnalysisOutput> {
        let mut id_generator = self.id_generator.clone();
        let mut program = self.program.clone();
        let term_analyzer = FunctionTerminationAnalyzer::new(errors);
        let function_terminations = term_analyzer.visit_program(&mut program);
        let type_analysis_data =
            TypePropagator::new(errors, &mut id_generator).visit_program(&mut program);

        run_fix_passes(errors, &mut program, &mut id_generator);

        Some(AnalysisOutput {
            program,
            id_generator,
            function_terminations,
            type_analysis_data,
        })
    }

    pub fn format(&self) -> String {
        let mut errors = vec![];
        run_fix_passes(
            &mut errors,
            &mut self.program.clone(),
            &mut self.id_generator.clone(),
        );

        let document =
            AstToDocumentModelConverter::default().visit_program(&mut self.program.clone());
        stringify_document(&fully_flatten_document(document))
    }
}

#[derive(Clone, Debug)]
pub struct AnalysisOutput {
    pub program: Program,
    pub id_generator: IdGenerator,

    pub function_terminations: PerNodeData<FunctionTermination>,
    pub type_analysis_data: TypeAnalysisData,
}

impl AnalysisOutput {
    #[cfg(feature = "llvm_backend")]
    pub fn compile_llvm<'a>(
        &self,
        errors: &mut Vec<FleetError>,
        context: &'a Context,
    ) -> Option<LLVMCompilationOutput<'a>> {
        use crate::ir_generator::IrGenerator;

        if errors
            .iter()
            .any(|err| err.severity == ErrorSeverity::Error)
        {
            return None;
        }
        let ir_generator = IrGenerator::new(
            context,
            errors,
            &self.function_terminations,
            &self.type_analysis_data,
        );
        match ir_generator.visit_program(&mut self.program.clone()) {
            Ok(module) => Some(LLVMCompilationOutput { module }),
            Err(error) => {
                errors.push(FleetError {
                    start: SourceLocation::start(),
                    end: self
                        .program
                        .functions
                        .first()
                        .map_or(SourceLocation::start(), |f| f.close_paren_token.end),
                    message: match error.source() {
                        Some(source) => format!("{} ({:?})", error, source),
                        None => error.to_string(),
                    },
                    severity: ErrorSeverity::Error,
                });

                None
            }
        }
    }

    pub fn compile_c(&self) -> String {
        CCodeGenerator::new(&self.type_analysis_data).visit_program(&mut self.program.clone())
    }
}

#[cfg(feature = "llvm_backend")]
#[derive(Clone, Debug)]
pub struct LLVMCompilationOutput<'a> {
    pub module: Module<'a>,
}

#[cfg(feature = "llvm_backend")]
impl LLVMCompilationOutput<'_> {
    pub fn run_default_optimization_passes(
        &self,
        target_machine: &TargetMachine,
    ) -> Result<(), Box<dyn Error>> {
        use inkwell::passes::PassBuilderOptions;

        self.module
            .run_passes("default<O1>", target_machine, PassBuilderOptions::create())?;
        Ok(())
    }
}
