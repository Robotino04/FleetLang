use crate::{
    ast::{Executor, ExecutorHost, Expression, FunctionDefinition, Program, Statement, Type},
    document_model::DocumentElement,
    tokenizer::{Keyword, Token, TokenType, Trivia, TriviaKind},
};

fn trivia_to_element(trivia: &Vec<Trivia>) -> DocumentElement {
    return DocumentElement::spaced_concatentation(
        DocumentElement::CollapsableSpace,
        trivia
            .iter()
            .map(|t| match &t.kind {
                TriviaKind::LineComment(contents) => DocumentElement::Concatenation(vec![
                    DocumentElement::Text("// ".to_string()),
                    DocumentElement::Text(contents.trim().to_string()),
                    DocumentElement::CollapsableLineBreak,
                ]),
                TriviaKind::BlockComment(contents) => {
                    let min_space = contents
                        .split("\n")
                        .skip(1)
                        .map(|line| {
                            if line.trim() == "" {
                                usize::MAX
                            } else {
                                line.chars()
                                    .take_while(|c| matches!(*c, ' ' | '\t'))
                                    .count()
                            }
                        })
                        .min()
                        .unwrap_or(0);

                    let index_of_last_line = contents.split("\n").count() - 1;

                    // can't use .lines() because that removes the last newline
                    let lines = contents.split("\n").enumerate().map(|(i, mut line)| {
                        if i != index_of_last_line {
                            line = line.trim_end();
                        }
                        if i != 0 {
                            line = &line[min_space.min(line.len().saturating_sub(1))..];
                        }

                        if line.trim() == "" {
                            return "";
                        } else {
                            return line;
                        }
                    });

                    DocumentElement::Concatenation(vec![
                        DocumentElement::Text("/*".to_string()),
                        DocumentElement::spaced_concatentation(
                            DocumentElement::ForcedLineBreak,
                            lines
                                .map(|line| DocumentElement::Text(line.to_string()))
                                .collect(),
                        ),
                        DocumentElement::Text("*/".to_string()),
                    ])
                }
                TriviaKind::EmptyLine => DocumentElement::CollapsableLineBreak,
                TriviaKind::EmptyLineAtTokenSide => DocumentElement::ForcedLineBreak,
            })
            .collect(),
    );
}

fn keyword_string(keyword: Keyword) -> String {
    match keyword {
        Keyword::On => "on",
        Keyword::Self_ => "self",
        Keyword::Let => "let",
        Keyword::I32 => "i32",
        Keyword::Return => "return",
    }
    .to_string()
}

fn token_type_to_element(token: &Token) -> DocumentElement {
    #[rustfmt::skip]
    let s = match token.type_ {
        TokenType::Keyword(keyword) => &keyword_string(keyword),
        TokenType::Identifier(ref id) => id.as_str(),
        TokenType::Number(n) => &n.to_string(),
        TokenType::UnknownCharacters(ref chars) => chars,

        TokenType::OpenBrace        => "{",
        TokenType::CloseBrace       => "}",
        TokenType::OpenParen        => "(",
        TokenType::CloseParen       => ")",
        TokenType::OpenBracket      => "{",
        TokenType::CloseBracket     => "}",

        TokenType::Semicolon        => ";",
        TokenType::Dot              => ".",
        TokenType::Colon            => ":",

        TokenType::EqualSign        => "=",
        TokenType::SingleRightArrow => "->",

        TokenType::ExclamationMark  => "!",
        TokenType::Tilde            => "~",

        TokenType::Minus            => "-",
        TokenType::Plus             => "+",
        TokenType::Star             => "*",
        TokenType::Slash            => "/",
        TokenType::Percent          => "%",

        TokenType::GreaterThan      => ">",
        TokenType::GreaterThanEqual => ">=",
        TokenType::LessThan         => "<",
        TokenType::LessThanEqual    => "<=",

        TokenType::DoubleEqual      => "==",
        TokenType::NotEqual         => "!=",

        TokenType::DoubleAmpersand  => "&&",
        TokenType::DoublePipe       => "||",
    };

    return DocumentElement::Text(s.to_string());
}

fn token_to_element(token: &Token) -> DocumentElement {
    DocumentElement::spaced_concatentation(
        DocumentElement::CollapsableSpace,
        vec![
            trivia_to_element(&token.leading_trivia),
            token_type_to_element(token),
            trivia_to_element(&token.trailing_trivia),
        ],
    )
}

pub fn convert_program_to_document_model(program: &Program) -> DocumentElement {
    return DocumentElement::Concatenation(vec![
        DocumentElement::SpaceEater,
        DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableLineBreak,
            program
                .functions
                .iter()
                .map(|f| convert_function_definition(f))
                .collect(),
        ),
        DocumentElement::ReverseSpaceEater,
    ]);
}

fn convert_function_definition(
    FunctionDefinition {
        let_token,
        name: _,
        name_token,
        equal_token,
        open_paren_token,
        close_paren_token,
        right_arrow_token,
        return_type,
        body,
    }: &FunctionDefinition,
) -> DocumentElement {
    return DocumentElement::spaced_concatentation(
        DocumentElement::CollapsableSpace,
        vec![
            token_to_element(let_token),
            token_to_element(name_token),
            token_to_element(equal_token),
            DocumentElement::Concatenation(vec![
                trivia_to_element(&open_paren_token.leading_trivia),
                DocumentElement::CollapsableSpace,
                token_type_to_element(open_paren_token),
                trivia_to_element(&open_paren_token.trailing_trivia),
                //
                trivia_to_element(&close_paren_token.leading_trivia),
                token_type_to_element(close_paren_token),
                DocumentElement::CollapsableSpace,
                trivia_to_element(&close_paren_token.trailing_trivia),
            ]),
            token_to_element(right_arrow_token),
            convert_type(return_type),
            convert_statement(body),
        ],
    );
}

fn convert_type(type_: &Type) -> DocumentElement {
    match type_ {
        Type::I32 { token } => token_to_element(token),
    }
}

fn convert_statement(statement: &Statement) -> DocumentElement {
    match statement {
        Statement::Expression {
            expression,
            semicolon_token,
        } => DocumentElement::Concatenation(vec![
            convert_expression(expression),
            DocumentElement::double_space_eater(),
            token_to_element(semicolon_token),
        ]),
        Statement::On {
            on_token,
            open_paren_token,
            executor,
            close_paren_token,
            body,
        } => DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                token_to_element(on_token),
                DocumentElement::Concatenation(vec![
                    token_to_element(open_paren_token),
                    convert_executor(executor),
                    token_to_element(close_paren_token),
                ]),
                convert_statement(body),
            ],
        ),
        Statement::Block {
            open_brace_token,
            body,
            close_brace_token,
        } => DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableLineBreak,
            vec![
                DocumentElement::spaced_concatentation(
                    DocumentElement::CollapsableSpace,
                    vec![
                        trivia_to_element(&open_brace_token.leading_trivia),
                        token_type_to_element(open_brace_token),
                        trivia_to_element(&open_brace_token.trailing_trivia),
                    ],
                ),
                DocumentElement::Indentation(Box::new(DocumentElement::spaced_concatentation(
                    DocumentElement::CollapsableLineBreak,
                    vec![
                        DocumentElement::spaced_concatentation(
                            DocumentElement::CollapsableLineBreak,
                            body.iter().map(|s| convert_statement(s)).collect(),
                        ),
                        trivia_to_element(&close_brace_token.leading_trivia),
                    ],
                ))),
                DocumentElement::spaced_concatentation(
                    DocumentElement::CollapsableSpace,
                    vec![
                        token_type_to_element(close_brace_token),
                        trivia_to_element(&close_brace_token.trailing_trivia),
                    ],
                ),
            ],
        ),
        Statement::Return {
            return_token,
            value,
            semicolon_token,
        } => DocumentElement::Concatenation(vec![
            DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableSpace,
                vec![token_to_element(return_token), convert_expression(value)],
            ),
            DocumentElement::double_space_eater(),
            DocumentElement::double_space_eater(),
            token_to_element(semicolon_token),
        ]),
        Statement::VariableDefinition {
            let_token,
            name_token,
            name: _,
            colon_token,
            type_,
            equals_token,
            value,
            semicolon_token,
        } => DocumentElement::Concatenation(vec![
            DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableSpace,
                vec![
                    token_to_element(let_token),
                    token_to_element(name_token),
                    DocumentElement::double_space_eater(),
                    token_to_element(colon_token),
                    convert_type(type_),
                    token_to_element(equals_token),
                    convert_expression(value),
                ],
            ),
            DocumentElement::double_space_eater(),
            DocumentElement::double_space_eater(),
            token_to_element(semicolon_token),
        ]),
    }
}
fn convert_executor(executor: &Executor) -> DocumentElement {
    match executor {
        Executor::Thread {
            host,
            dot_token,
            thread_token,
            open_bracket_token,
            index,
            close_bracket_token,
        } => DocumentElement::Concatenation(vec![
            convert_executor_host(host),
            token_to_element(dot_token),
            token_to_element(thread_token),
            token_to_element(open_bracket_token),
            convert_expression(index),
            token_to_element(close_bracket_token),
        ]),
    }
}
fn convert_executor_host(host: &ExecutorHost) -> DocumentElement {
    match host {
        ExecutorHost::Self_ { token } => token_to_element(token),
    }
}

fn convert_expression(expression: &Expression) -> DocumentElement {
    match expression {
        Expression::Number { value: _, token } => token_to_element(token),
        Expression::FunctionCall {
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
        } => DocumentElement::Concatenation(vec![
            token_to_element(name_token),
            DocumentElement::Concatenation(vec![
                token_to_element(open_paren_token),
                DocumentElement::spaced_concatentation(
                    DocumentElement::Concatenation(vec![
                        DocumentElement::Text(",".to_string()),
                        DocumentElement::CollapsableSpace,
                    ]),
                    arguments
                        .iter()
                        .map(|arg| convert_expression(arg))
                        .collect(),
                ),
                token_to_element(close_paren_token),
            ]),
        ]),
        Expression::VariableAccess {
            name: _,
            name_token,
        } => token_to_element(name_token),
        Expression::Grouping {
            open_paren_token,
            subexpression,
            close_paren_token,
        } => DocumentElement::Concatenation(vec![
            DocumentElement::Concatenation(vec![
                trivia_to_element(&open_paren_token.leading_trivia),
                DocumentElement::CollapsableSpace,
                token_type_to_element(open_paren_token),
                DocumentElement::double_space_eater(),
                trivia_to_element(&open_paren_token.trailing_trivia),
            ]),
            convert_expression(subexpression),
            DocumentElement::Concatenation(vec![
                trivia_to_element(&close_paren_token.leading_trivia),
                DocumentElement::double_space_eater(),
                token_type_to_element(close_paren_token),
                DocumentElement::CollapsableSpace,
                trivia_to_element(&close_paren_token.trailing_trivia),
            ]),
        ]),
        Expression::Unary {
            operator_token,
            operation: _,
            operand,
        } => DocumentElement::Concatenation(vec![
            token_to_element(operator_token),
            DocumentElement::double_space_eater(),
            convert_expression(operand),
        ]),
        Expression::Binary {
            left,
            operator_token,
            operation: _,
            right,
        } => DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                convert_expression(left),
                token_to_element(operator_token),
                convert_expression(right),
            ],
        ),
        Expression::VariableAssignment {
            name: _,
            name_token,
            equal_token,
            right,
        } => DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                token_to_element(name_token),
                token_to_element(equal_token),
                convert_expression(right),
            ],
        ),
    }
}
