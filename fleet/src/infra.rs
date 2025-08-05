use std::collections::HashMap;
#[cfg(feature = "llvm_backend")]
use std::error::Error;

#[cfg(feature = "llvm_backend")]
use inkwell::{context::Context, module::Module, targets::TargetMachine};
use itertools::Itertools;

use crate::{
    ast::{AstNode, AstVisitor, PerNodeData, Program},
    ast_to_dm::AstToDocumentModelConverter,
    document_model::{fully_flatten_document, stringify_document},
    generate_c::CCodeGenerator,
    generate_glsl::GLSLCodeGenerator,
    parser::{IdGenerator, Parser, ParserError},
    passes::{
        err_missing_type_in_parameter::ErrMissingTypeInParam,
        find_node_bonds::find_node_bounds,
        fix_non_block_statements::FixNonBlockStatements,
        fix_trailing_comma::FixTrailingComma,
        lvalue_reducer::LValueReducer,
        remove_parens::RemoveParensPass,
        stat_tracker::{NodeStats, StatTracker, YesNoMaybe},
        type_propagation::{FunctionID, TypeAnalysisData, TypePropagator},
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
    pub fn from_node<I: Into<AstNode> + Clone>(
        node: &I,
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

    let pad_with_line_number =
        |(line, text): (usize, &str)| format!("{line:<max_line_number_len$}| {text}");

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
    println!("{before_err_trunc}");
    println!("{err}");
    println!("{after_err_trunc}\n");
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
    pub fn analyze(&self, errors: &mut Vec<FleetError>) -> Option<TypeAnalysisOutput> {
        let mut id_generator = self.id_generator.clone();
        let mut program = self.program.clone();
        let type_analysis_data =
            TypePropagator::new(errors, &mut id_generator).visit_program(&mut program);

        Some(TypeAnalysisOutput {
            program,
            id_generator,
            type_analysis_data,
        })
    }

    pub fn format(&self) -> String {
        let mut errors = vec![];
        let mut program = self.program.clone();
        let mut id_generator = self.id_generator.clone();

        run_fix_passes(&mut errors, &mut program, &mut id_generator);

        let document = AstToDocumentModelConverter::default().visit_program(&mut program);
        stringify_document(&fully_flatten_document(document))
    }
}

#[derive(Clone, Debug)]
pub struct TypeAnalysisOutput {
    pub program: Program,
    pub id_generator: IdGenerator,
    pub type_analysis_data: TypeAnalysisData,
}

impl TypeAnalysisOutput {
    pub fn fixup(&self, errors: &mut Vec<FleetError>) -> Option<FixupOutput> {
        let mut id_generator = self.id_generator.clone();
        let mut program = self.program.clone();

        LValueReducer::new(
            errors,
            None,
            &self.type_analysis_data.type_data,
            &self.type_analysis_data.type_sets,
            &self.type_analysis_data.variable_data,
            &self.type_analysis_data.scope_data,
        )
        .visit_program(&mut program);
        let stats = StatTracker::new(errors, &self.type_analysis_data).visit_program(&mut program);

        run_fix_passes(errors, &mut program, &mut id_generator);

        Some(FixupOutput {
            program,
            id_generator,
            stats,
        })
    }
}

#[derive(Clone, Debug)]
pub struct FixupOutput {
    pub program: Program,
    pub id_generator: IdGenerator,
    pub stats: PerNodeData<NodeStats>,
}

impl FixupOutput {
    pub fn pre_compile_glsl(
        &self,
        errors: &mut Vec<FleetError>,
        type_analysis_output: &TypeAnalysisOutput,
    ) -> Option<GLSLOutput> {
        if errors
            .iter()
            .any(|err| err.severity == ErrorSeverity::Error)
        {
            return None;
        }

        let mut program = self.program.clone();

        if self
            .stats
            .get(&self.program.id)
            .expect("No stats available for program")
            .uses_gpu
            == YesNoMaybe::No
        {
            eprintln!("Skipping glsl pregen because nothing uses the gpu");

            return Some(GLSLOutput {
                program,
                glsl_functions: HashMap::default(),
            });
        }

        Some(GLSLOutput {
            glsl_functions: GLSLCodeGenerator::new(
                errors,
                &type_analysis_output.type_analysis_data.variable_data,
                &type_analysis_output.type_analysis_data.function_data,
                &type_analysis_output.type_analysis_data.type_data,
                &type_analysis_output.type_analysis_data.type_sets,
                &self.stats,
            )
            .visit_program(&mut program)
            .ok()?,
            program,
        })
    }
}

#[derive(Clone, Debug)]
pub struct GLSLOutput {
    pub program: Program,
    pub glsl_functions: HashMap<FunctionID, (String, String)>,
}

impl GLSLOutput {
    #[cfg(feature = "llvm_backend")]
    pub fn compile_llvm<'a>(
        &self,
        errors: &mut Vec<FleetError>,
        context: &'a Context,
        type_analysis_output: &TypeAnalysisOutput,
        fixup_output: &FixupOutput,
    ) -> Option<LLVMCompilationOutput<'a>> {
        use crate::ir_generator::IrGenerator;

        if errors
            .iter()
            .any(|err| err.severity == ErrorSeverity::Error)
        {
            return None;
        }
        match IrGenerator::new(
            context,
            errors,
            &fixup_output.stats,
            &mut type_analysis_output.type_analysis_data.clone(),
            &self.glsl_functions,
        )
        .visit_program(&mut self.program.clone())
        {
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
                        Some(source) => format!("{error} ({source:?})"),
                        None => error.to_string(),
                    },
                    severity: ErrorSeverity::Error,
                });

                None
            }
        }
    }

    pub fn compile_c(
        &self,
        errors: &mut Vec<FleetError>,
        type_analysis_output: &TypeAnalysisOutput,
        fixup_output: &FixupOutput,
    ) -> Option<String> {
        if errors
            .iter()
            .any(|err| err.severity == ErrorSeverity::Error)
        {
            return None;
        }

        let TypeAnalysisData {
            type_data,
            type_sets,
            variable_data,
            function_data,
            scope_data,
        } = &type_analysis_output.type_analysis_data;

        Some(
            CCodeGenerator::new(
                errors,
                variable_data,
                function_data,
                type_data,
                &mut type_sets.clone(),
                scope_data,
                &fixup_output.stats,
                &self.glsl_functions,
            )
            .visit_program(&mut self.program.clone()),
        )
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

        // tsan conflicts with asan
        // msan is annoying to link
        self.module.run_passes(
            "default<O1>,asan",
            target_machine,
            PassBuilderOptions::create(),
        )?;

        Ok(())
    }
}
