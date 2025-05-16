use inkwell::{context::Context, module::Module};

use crate::{
    ast::{AstVisitor, Program},
    ast_to_dm::convert_program_to_document_model,
    document_model::{fully_flatten_document, stringify_document},
    ir_generator::IrGenerator,
    parser::Parser,
    passes::remove_parens::RemoveParensPass,
    tokenizer::{SourceLocation, Token, Tokenizer},
};

#[derive(Clone, Debug)]
pub struct FleetError {
    pub start: SourceLocation,
    pub end: SourceLocation,
    pub message: String,
}

impl FleetError {
    pub fn from_token(token: &Token, msg: impl ToString) -> Self {
        Self {
            start: token.start,
            end: token.end,
            message: msg.to_string(),
        }
    }
}

pub fn fleet_error(msg: impl AsRef<str>) {
    println!("\x1B[31m[FLEETC: ERROR] {}.\x1B[0m", msg.as_ref());
}
pub fn print_error_message(source: &String, error: &FleetError) {
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
                    "{}\x1B[31m{}\x1B[0m{}",
                    &text[..start_col],
                    &text[start_col..end_col],
                    &text[end_col..]
                )
                .as_str(),
            ))
        })
        .collect::<Vec<_>>()
        .join("\n");
    let after_err = source_lines.skip(error.end.line);

    let before_err_trunc = before_err
        .skip(error.start.line.saturating_sub(num_before_error_lines + 1))
        .skip_while(|(_line, text)| text.trim() == "")
        .map(pad_with_line_number)
        .collect::<Vec<_>>()
        .join("\n");
    let after_err_trunc = after_err
        .take(num_after_error_lines)
        .map(pad_with_line_number)
        .collect::<Vec<_>>()
        .join("\n");

    fleet_error(format!("\x1B[31m[FLEETC: ERROR] {}\x1B[0m", error.message));
    println!("{}", before_err_trunc);
    println!("{}", err);
    println!("{}\n", after_err_trunc);
}

#[derive(Clone, Debug)]
pub enum CompileStatus<'a> {
    TokenizerFailure {},
    ParserFailure {
        tokens: Vec<Token>,
    },
    TokenizerOrParserErrors {
        tokens: Vec<Token>,
        partial_program: Program,
    },
    IrGeneratorFailure {
        tokens: Vec<Token>,
        program: Program,
    },
    IrGeneratorErrors {
        tokens: Vec<Token>,
        program: Program,
        partial_module: Option<Module<'a>>,
    },
    Success {
        tokens: Vec<Token>,
        program: Program,
        module: Module<'a>,
    },
}

impl<'a> CompileStatus<'a> {
    pub fn module(&self) -> Option<&Module<'a>> {
        match &self {
            CompileStatus::TokenizerFailure {} => None,
            CompileStatus::ParserFailure { tokens: _ } => None,
            CompileStatus::TokenizerOrParserErrors {
                tokens: _,
                partial_program: _,
            } => None,
            CompileStatus::IrGeneratorFailure {
                tokens: _,
                program: _,
            } => None,
            CompileStatus::IrGeneratorErrors {
                tokens: _,
                program: _,
                partial_module,
            } => partial_module.as_ref(),
            CompileStatus::Success {
                tokens: _,
                program: _,
                module,
            } => Some(module),
        }
    }
    pub fn tokens(&self) -> Option<&Vec<Token>> {
        match &self {
            CompileStatus::TokenizerFailure {} => None,
            CompileStatus::ParserFailure { tokens } => Some(tokens),
            CompileStatus::TokenizerOrParserErrors {
                tokens,
                partial_program: _,
            } => Some(tokens),
            CompileStatus::IrGeneratorFailure { tokens, program: _ } => Some(tokens),
            CompileStatus::IrGeneratorErrors {
                tokens,
                program: _,
                partial_module: _,
            } => Some(tokens),
            CompileStatus::Success {
                tokens,
                program: _,
                module: _,
            } => Some(tokens),
        }
    }
    pub fn program(&self) -> Option<&Program> {
        match &self {
            CompileStatus::TokenizerFailure {} => None,
            CompileStatus::ParserFailure { tokens: _ } => None,
            CompileStatus::TokenizerOrParserErrors {
                tokens: _,
                partial_program,
            } => Some(partial_program),
            CompileStatus::IrGeneratorFailure { tokens: _, program } => Some(program),
            CompileStatus::IrGeneratorErrors {
                tokens: _,
                program,
                partial_module: _,
            } => Some(program),
            CompileStatus::Success {
                tokens: _,
                program,
                module: _,
            } => Some(program),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompileResult<'a> {
    pub status: CompileStatus<'a>,
    pub errors: Vec<FleetError>,
}

pub fn compile_program<'a>(context: &'a Context, src: &str) -> CompileResult<'a> {
    let mut tokenizer = Tokenizer::new(src.to_string());

    let tokens = tokenizer.tokenize();
    if tokens.is_err() {
        return CompileResult {
            status: CompileStatus::TokenizerFailure {},
            errors: vec![],
        };
    }
    let tokens = tokens.unwrap().clone();

    let mut parser = Parser::new(tokens.clone());
    let program = parser.parse_program();

    let mut errors = tokenizer.errors().clone();
    errors.append(&mut parser.errors().clone());

    if program.is_err() {
        return CompileResult {
            status: CompileStatus::ParserFailure { tokens },
            errors,
        };
    }
    let program = program.unwrap();
    if !errors.is_empty() {
        return CompileResult {
            status: CompileStatus::TokenizerOrParserErrors {
                tokens,
                partial_program: program,
            },
            errors,
        };
    }

    let mut ir_generator = IrGenerator::new(&context);
    if let Err(error) = ir_generator.generate_program_ir(&program) {
        errors.push(FleetError {
            start: SourceLocation::start(),
            end: SourceLocation::end(src),
            message: match error.source() {
                Some(source) => format!("{} ({:?})", error.to_string(), source),
                None => error.to_string(),
            },
        });
        return CompileResult {
            status: CompileStatus::IrGeneratorFailure { tokens, program },
            errors,
        };
    }
    if !ir_generator.errors().is_empty() {
        errors.append(&mut ir_generator.errors().clone());
        return CompileResult {
            status: CompileStatus::IrGeneratorErrors {
                tokens,
                program,
                partial_module: if ir_generator.module().verify().is_ok() {
                    Some(ir_generator.module().clone())
                } else {
                    None
                },
            },
            errors,
        };
    }

    return CompileResult {
        status: CompileStatus::Success {
            tokens,
            program,
            module: ir_generator.module().clone(),
        },
        errors,
    };
}

pub fn format_program(mut program: Program) -> String {
    RemoveParensPass::new().visit_program(&mut program);
    let document = convert_program_to_document_model(&program);
    let formatted_src = stringify_document(&fully_flatten_document(document));
    return formatted_src;
}
