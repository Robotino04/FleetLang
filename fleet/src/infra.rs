use indoc::formatdoc;
use itertools::Itertools;

use crate::{
    NewtypeDeref,
    ast::AstNode,
    ast_to_dm::AstToDocumentModelConverter,
    document_model::{DocumentElement, stringify_document},
    generate_c::CCodeGenerator,
    generate_glsl::GLSLCodeGenerator,
    parser::Parser,
    passes::{
        err_missing_type_in_parameter::ErrMissingTypeInParam,
        err_too_few_iterators::ErrTooFewIterators,
        find_node_bounds::find_node_bounds,
        fix_non_block_statements::FixNonBlockStatements,
        fix_trailing_comma::FixTrailingComma,
        lvalue_reducer::LValueReducer,
        pass_manager::{Errors, InputSource, PassError, PassManager},
        remove_parens::RemoveParensPass,
        scope_analysis::ScopeAnalyzer,
        stat_tracker::StatTracker,
        store_pass::StorePass,
        type_propagation::TypePropagator,
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

    pub fn to_string(&self, source: &str) -> String {
        self.to_string_impl(source, false)
    }
    pub fn to_string_ansi(&self, source: &str) -> String {
        self.to_string_impl(source, true)
    }

    fn to_string_impl(&self, source: &str, use_ansi: bool) -> String {
        let (enable_color, disable_color) = {
            let nr = match self.severity {
                ErrorSeverity::Error => 31,
                ErrorSeverity::Warning => 33,
                ErrorSeverity::Note => 34,
            };

            if use_ansi {
                (format!("\x1B[{nr}m"), "\x1B[0m")
            } else {
                ("".to_string(), "")
            }
        };

        let num_before_error_lines = 3;
        let num_after_error_lines = 3;

        let max_line_number_len = (self.end.line + num_after_error_lines).to_string().len();

        let pad_with_line_number =
            |(line, text): (usize, &str)| format!("{line:<max_line_number_len$}| {text}");

        let source_lines = source
            .split("\n")
            .enumerate()
            .map(|(line, text)| (line + 1, text));

        let before_err = source_lines.clone().take(self.start.line - 1);
        let err = source_lines
            .clone()
            .skip(self.start.line - 1)
            .take(self.end.line - self.start.line + 1)
            .map(|(line, text)| {
                let start_col = if line == self.start.line {
                    self.start.column
                } else {
                    0
                };
                let end_col = if line == self.end.line {
                    self.end.column.min(text.len())
                } else {
                    text.len()
                };

                pad_with_line_number((
                    line,
                    format!(
                        "{}{enable_color}{}{disable_color}{}",
                        &text[..start_col],
                        &text[start_col..end_col],
                        &text[end_col..]
                    )
                    .as_str(),
                ))
            })
            .join("\n");
        let after_err = source_lines.skip(self.end.line);

        let before_err_trunc = before_err
            .skip(self.start.line.saturating_sub(num_before_error_lines + 1))
            .skip_while(|(_line, text)| text.trim() == "")
            .map(pad_with_line_number)
            .join("\n");
        let after_err_trunc = after_err
            .take(num_after_error_lines)
            .map(pad_with_line_number)
            .join("\n");

        formatdoc!(
            "
            {enable_color}[FLEETC: {}] {}{disable_color}
            {before_err_trunc}
            {err}
            {after_err_trunc}",
            match self.severity {
                ErrorSeverity::Error => "ERROR",
                ErrorSeverity::Warning => "WARNING",
                ErrorSeverity::Note => "NOTE",
            },
            self.message
        )
    }
}

pub fn insert_fix_passes(pm: &mut PassManager) {
    pm.insert::<RemoveParensPass>();
    pm.insert::<FixNonBlockStatements>();
    pm.insert::<FixTrailingComma>();
    pm.insert::<ErrMissingTypeInParam>();
    pm.insert::<ErrTooFewIterators>();
}

pub fn insert_minimal_pipeline(pm: &mut PassManager) {
    pm.insert::<Tokenizer>();
    pm.insert::<Parser>();
}
pub fn insert_compile_passes(pm: &mut PassManager) {
    pm.insert::<ScopeAnalyzer>();
    pm.insert::<TypePropagator>();
    pm.insert::<LValueReducer>();
    pm.insert::<StatTracker>();
    pm.insert::<GLSLCodeGenerator>();
}

pub fn insert_c_passes(pm: &mut PassManager) {
    pm.insert::<CCodeGenerator>();
}

NewtypeDeref!(ParseErrorsOnly, Errors);

pub fn format(source: String) -> Result<String, PassError> {
    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    pm.insert::<StorePass<Errors, ParseErrorsOnly>>();
    insert_fix_passes(&mut pm);
    pm.insert::<AstToDocumentModelConverter>();

    pm.state.insert(InputSource(source));
    pm.state.insert_default::<Errors>();

    pm.run()?;

    let de = pm
        .state
        .get::<DocumentElement>()
        .expect("Formatting passes failed")
        .clone();

    if pm
        .state
        .get::<ParseErrorsOnly>()
        .unwrap()
        .iter()
        .any(|err| err.severity == ErrorSeverity::Error)
    {
        return Err(PassError::InvalidInput {
            producing_pass: "Formatting function".to_string(),
            source: "Not formatting malformed input".into(),
        });
    }

    Ok(stringify_document(de))
}
