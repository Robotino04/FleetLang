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
        first_token_of_node::first_token_of_node,
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
    tokenizer::{FileName, SourceLocation, SourceRange, Token, Tokenizer},
};

#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum ErrorSeverity {
    Note,
    Warning,
    Error,
}

#[derive(Clone, Debug)]
pub struct FleetError {
    /// ranges are guaranteed to be sorted and non-overlapping
    highlight_groups: Vec<SourceRange>,
    pub message: String,
    pub severity: ErrorSeverity,
    pub file_name: FileName,
}

impl FleetError {
    /// Returns [None] if `highlight_groups` contains overlapping ranges or is empty
    pub fn try_new(
        mut highlight_groups: Vec<SourceRange>,
        message: impl ToString,
        severity: ErrorSeverity,
        file_name: FileName,
    ) -> Option<Self> {
        highlight_groups.sort_by_key(|hl| hl.start);

        if highlight_groups.is_empty() {
            return None;
        }

        for (a, b) in highlight_groups.iter().tuple_windows() {
            if a.end > b.start {
                return None;
            }
        }

        Some(Self {
            highlight_groups,
            message: message.to_string(),
            severity,
            file_name,
        })
    }
    pub fn from_range(
        range: SourceRange,
        msg: impl ToString,
        severity: ErrorSeverity,
        file_name: FileName,
    ) -> Self {
        Self {
            highlight_groups: vec![range],
            message: msg.to_string(),
            severity,
            file_name,
        }
    }
    pub fn from_token(token: &Token, msg: impl ToString, severity: ErrorSeverity) -> Self {
        Self {
            highlight_groups: vec![token.range],
            message: msg.to_string(),
            severity,
            file_name: token.file_name.clone(),
        }
    }
    pub fn from_node<I: Into<AstNode> + Clone>(
        node: &I,
        msg: impl ToString,
        severity: ErrorSeverity,
    ) -> Self {
        Self {
            highlight_groups: vec![find_node_bounds(node)],
            message: msg.to_string(),
            severity,
            file_name: first_token_of_node(node)
                .expect("Cannot create FleetError from empty node")
                .file_name,
        }
    }

    pub fn highlight_groups(&self) -> &Vec<SourceRange> {
        &self.highlight_groups
    }

    pub fn to_string(&self, source: &str) -> String {
        self.to_string_impl(source, false)
    }
    pub fn to_string_ansi(&self, source: &str) -> String {
        self.to_string_impl(source, true)
    }

    pub fn start(&self) -> SourceLocation {
        self.highlight_groups
            .iter()
            .map(|range| range.start)
            .min()
            .expect("FleetError without highlight group")
    }
    pub fn end(&self) -> SourceLocation {
        self.highlight_groups
            .iter()
            .map(|range| range.end)
            .max()
            .expect("FleetError without highlight group")
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

        let self_start = self.start();
        let self_end = self.end();

        let num_before_error_lines = 3;
        let num_after_error_lines = 3;

        let max_line_number_len = (self_end.line + num_after_error_lines).to_string().len();

        let pad_with_line_number =
            |(line, text): (usize, &str)| format!("{line:<max_line_number_len$}| {text}");

        let source_lines = source
            .split("\n")
            .enumerate()
            .map(|(line, text)| (line + 1, text));

        let before_err = source_lines.clone().take(self_start.line - 1);
        let err = source_lines
            .clone()
            .skip(self_start.line - 1)
            .take(self_end.line - self_start.line + 1)
            .map(|(line, text)| {
                let mut text = text.to_string();

                let mut offset = 0;

                for SourceRange {
                    start: hl_start,
                    end: hl_end,
                } in &self.highlight_groups
                {
                    assert!(hl_start < hl_end);
                    let start_col = if line == hl_start.line {
                        hl_start.column + offset
                    } else {
                        0
                    };
                    let end_col = if line == hl_end.line {
                        (hl_end.column + offset).min(text.len())
                    } else {
                        text.len()
                    };

                    let new_text = format!(
                        "{}{enable_color}{}{disable_color}{}",
                        &text[..start_col],
                        &text[start_col..end_col],
                        &text[end_col..]
                    );

                    offset += new_text.len() - text.len();
                    text = new_text;
                }

                pad_with_line_number((line, &text))
            })
            .join("\n");
        let after_err = source_lines.skip(self_end.line);

        let before_err_trunc = before_err
            .skip(self_start.line.saturating_sub(num_before_error_lines + 1))
            .skip_while(|(_line, text)| text.trim() == "")
            .map(pad_with_line_number)
            .join("\n");
        let after_err_trunc = after_err
            .take(num_after_error_lines)
            .map(pad_with_line_number)
            .join("\n");

        assert!(
            !self.message.ends_with("\n"),
            "Message of error ends with newline: {self:#?}"
        );

        let mut output = format!(
            "{enable_color}[{}] {}:{}:{}: {}{disable_color}",
            match self.severity {
                ErrorSeverity::Error => "ERROR",
                ErrorSeverity::Warning => "WARNING",
                ErrorSeverity::Note => "NOTE",
            },
            self.file_name.0,
            self.start().line,
            self.start().column,
            self.message
        );

        if !before_err_trunc.is_empty() {
            output.push('\n');
            output.push_str(&before_err_trunc);
        }
        if !err.is_empty() {
            output.push('\n');
            output.push_str(&err);
        }
        if !after_err_trunc.is_empty() {
            output.push('\n');
            output.push_str(&after_err_trunc);
        }

        output
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

pub fn format(source: InputSource) -> Result<String, PassError> {
    let mut pm = PassManager::default();
    insert_minimal_pipeline(&mut pm);
    pm.insert::<StorePass<Errors, ParseErrorsOnly>>();
    insert_fix_passes(&mut pm);
    pm.insert::<AstToDocumentModelConverter>();

    pm.state.insert(source);
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
