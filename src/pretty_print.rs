use crate::{
    ast::{AstNode, Executor, ExecutorHost, Expression, FunctionDefinition, Statement, Type},
    tokenizer::{Keyword, Token, TokenType, Trivia, TriviaKind},
};

fn pretty_print_trivia(trivia: &Vec<Trivia>) -> String {
    if !trivia.iter().any(|t| {
        matches!(
            t.kind,
            TriviaKind::LineComment(_) | TriviaKind::BlockComment(_)
        )
    }) {
        return "".to_string();
    }

    let mut result: Vec<String> = vec![];
    let mut prev: Option<TriviaKind> = None;
    for t in trivia {
        if matches!(prev, Some(TriviaKind::BlockComment(_)))
            && matches!(
                t.kind,
                TriviaKind::LineComment(_) | TriviaKind::BlockComment(_)
            )
        {
            result.push(" ".to_string());
        }

        result.push(match t.kind.clone() {
            TriviaKind::LineComment(contents) => {
                format!("//{}\n", " ".to_string() + contents.trim())
            }
            TriviaKind::BlockComment(contents) => {
                let last_line_index = contents.split("\n").count() - 1;
                format!(
                    "/*{}*/",
                    contents
                        .split("\n")
                        .enumerate()
                        .map(|(i, line)| if i == 0 {
                            line
                        } else if i == last_line_index {
                            line.trim_start()
                        } else {
                            line.trim()
                        })
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
            TriviaKind::EmptyLine => "\n".to_string(),
        });
        prev = Some(t.kind.clone());
    }

    return result.concat().to_string();
}

fn pretty_print_keyword(keyword: Keyword) -> String {
    match keyword {
        Keyword::On => "on",
        Keyword::Self_ => "self",
        Keyword::Let => "let",
        Keyword::I32 => "i32",
        Keyword::Return => "return",
    }
    .into()
}

enum IndentationChange {
    TokenNewlineIndent,
    IndentNewlineToken,
    SpaceTokenSpace,
    NoneTokenSpace,
    SpaceTokenNone,
    SpaceTokenNoneTriviaSpace,
}

fn pretty_print_token_full(token: &Token, indentation_change: IndentationChange) -> String {
    let lead = pretty_print_trivia(&token.leading_trivia);
    let trail = pretty_print_trivia(&token.trailing_trivia);
    let actual_token = match token.type_ {
        TokenType::Keyword(keyword) => pretty_print_keyword(keyword),
        TokenType::Identifier(ref id) => id.clone(),
        TokenType::OpenBrace => "{".to_string(),
        TokenType::CloseBrace => "}".to_string(),
        TokenType::OpenParen => "(".to_string(),
        TokenType::CloseParen => ")".to_string(),
        TokenType::OpenBracket => "{".to_string(),
        TokenType::CloseBracket => "}".to_string(),
        TokenType::Semicolon => ";".to_string(),
        TokenType::Dot => ".".to_string(),
        TokenType::EqualSign => "=".to_string(),
        TokenType::SingleRightArrow => "->".to_string(),
        TokenType::Number(n) => n.to_string(),
        TokenType::ExclamationMark => "!".to_string(),
        TokenType::Tilde => "~".to_string(),
        TokenType::Minus => "-".to_string(),
        TokenType::Plus => "+".to_string(),
        TokenType::Star => "*".to_string(),
        TokenType::Slash => "/".to_string(),
        TokenType::Percent => "%".to_string(),
        TokenType::UnknownCharacters(ref chars) => chars.clone(),
    };

    let mut result = "".to_string();
    /*
     * Do not look at this match statement! It is the result of pure trial and error and will most
     * likely break for new syntax. All this for some stupid comments...
     */
    match indentation_change {
        IndentationChange::TokenNewlineIndent => {
            result += lead.as_str();
            if !lead.is_empty() && !lead.ends_with("\n") {
                result += " ";
            }
            result += actual_token.as_str();
            if !trail.is_empty() && !trail.starts_with("\n") {
                result += " ";
            }
            if !trail.is_empty() {
                result += indent::indent_by(4, trail.as_str()).as_str();
            }
            if !trail.ends_with("\n") {
                result += "\n";
            }
        }
        IndentationChange::IndentNewlineToken => {
            if !lead.is_empty() && !lead.starts_with("\n") {
                result += "\n";
            }
            if !lead.is_empty() {
                result += indent::indent_all_by(4, lead.as_str()).as_str();
            }
            if !lead.ends_with("\n") {
                result += "\n";
            }
            result += actual_token.as_str();
            if !trail.is_empty() && !trail.starts_with("\n") {
                result += " "
            }
            result += trail.as_str();
        }
        IndentationChange::SpaceTokenSpace => {
            result += lead.as_str();
            if !lead.is_empty() && !lead.ends_with("\n") {
                result += " ";
            }
            result += actual_token.as_str();
            if !trail.is_empty() && !trail.starts_with("\n") {
                result += " "
            }
            result += trail.as_str();
        }
        IndentationChange::SpaceTokenNone => {
            result += lead.as_str();
            if !lead.is_empty() && !lead.ends_with("\n") {
                result += " ";
            }
            result += actual_token.as_str();
            result += trail.as_str();
        }
        IndentationChange::SpaceTokenNoneTriviaSpace => {
            result += lead.as_str();
            if !lead.is_empty() && !lead.ends_with("\n") {
                result += " ";
            }
            result += actual_token.as_str();
            result += trail.as_str();
            if !trail.is_empty() {
                result += " ";
            }
        }
        IndentationChange::NoneTokenSpace => {
            result += lead.as_str();
            result += actual_token.as_str();
            if !trail.is_empty() && !trail.starts_with("\n") {
                result += " "
            }
            result += trail.as_str();
        }
    }

    return result;
}

fn pretty_print_token(token: &Token) -> String {
    pretty_print_token_full(token, IndentationChange::SpaceTokenSpace)
}

pub fn pretty_print(node: AstNode) -> String {
    match node {
        AstNode::Program(program) => {
            return program
                .functions
                .iter()
                .map(|f| pretty_print(f.clone().into()))
                .collect::<Vec<_>>()
                .join("\n");
        }
        AstNode::FunctionDefinition(FunctionDefinition {
            let_token,
            name: _,
            name_token,
            equal_token,
            open_paren_token,
            close_paren_token,
            right_arrow_token,
            return_type,
            body,
        }) => {
            return format!(
                "{} {} {} {}{} {} {} {}",
                pretty_print_token(&let_token),
                pretty_print_token(&name_token),
                pretty_print_token(&equal_token),
                pretty_print_token_full(&open_paren_token, IndentationChange::SpaceTokenNone),
                pretty_print_token_full(&close_paren_token, IndentationChange::NoneTokenSpace),
                pretty_print_token(&right_arrow_token),
                pretty_print(return_type.into()),
                pretty_print(body.into())
            );
        }
        AstNode::Type(Type::I32 { token }) => pretty_print_token(&token),
        AstNode::Statement(Statement::Expression {
            expression,
            semicolon_token,
        }) => {
            return pretty_print(expression.into()) + pretty_print_token(&semicolon_token).as_str();
        }
        AstNode::Statement(Statement::On {
            on_token,
            open_paren_token,
            executor,
            close_paren_token,
            body,
        }) => {
            return format!(
                "{} {}{}{} {}",
                pretty_print_token(&on_token),
                pretty_print_token_full(&open_paren_token, IndentationChange::SpaceTokenNone),
                pretty_print(executor.into()),
                pretty_print_token_full(&close_paren_token, IndentationChange::NoneTokenSpace),
                pretty_print((*body).into())
            );
        }
        AstNode::Statement(Statement::Block {
            open_brace_token,
            body,
            close_brace_token,
        }) => {
            return format!(
                "{}{}{}",
                pretty_print_token_full(&open_brace_token, IndentationChange::TokenNewlineIndent),
                indent::indent_all_by(
                    4,
                    body.iter()
                        .map(|tls| pretty_print(tls.clone().into()))
                        .collect::<Vec<_>>()
                        .join("\n")
                        .as_str()
                        .trim(),
                ),
                pretty_print_token_full(&close_brace_token, IndentationChange::IndentNewlineToken),
            );
        }
        AstNode::Statement(Statement::Return {
            return_token,
            value,
            semicolon_token,
        }) => {
            return format!(
                "{} {}{}",
                pretty_print_token(&return_token),
                pretty_print(value.into()),
                pretty_print_token(&semicolon_token),
            );
        }
        AstNode::ExecutorHost(ExecutorHost::Self_ { token }) => pretty_print_token(&token),
        AstNode::Executor(Executor::Thread {
            host,
            dot_token,
            thread_token,
            open_bracket_token,
            index,
            close_bracket_token,
        }) => {
            return format!(
                "{}{}{}{}{}{}",
                pretty_print(host.into()),
                pretty_print_token(&dot_token),
                pretty_print_token(&thread_token),
                pretty_print_token(&open_bracket_token),
                pretty_print(index.into()),
                pretty_print_token(&close_bracket_token),
            );
        }
        AstNode::Expression(Expression::Number { value: _, token }) => pretty_print_token(&token),
        AstNode::Expression(Expression::FunctionCall {
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
        }) => {
            return format!(
                "{}{}{}{}",
                pretty_print_token(&name_token),
                pretty_print_token(&open_paren_token),
                arguments
                    .iter()
                    .map(|e| pretty_print(AstNode::Expression(e.clone())))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str(),
                pretty_print_token(&close_paren_token),
            );
        }
        AstNode::Expression(Expression::Unary {
            operator_token,
            operation: _,
            operand,
        }) => {
            return format!(
                "{}{}",
                pretty_print_token(&operator_token),
                pretty_print((*operand).into())
            );
        }
        AstNode::Expression(Expression::Binary {
            left,
            operator_token,
            operation: _,
            right,
        }) => {
            return format!(
                "{} {} {}",
                pretty_print(AstNode::Expression(*left)),
                pretty_print_token(&operator_token),
                pretty_print(AstNode::Expression(*right))
            );
        }
        AstNode::Expression(Expression::Grouping {
            open_paren_token,
            subexpression,
            close_paren_token,
        }) => {
            return format!(
                "{}{}{}",
                pretty_print_token_full(
                    &open_paren_token,
                    IndentationChange::SpaceTokenNoneTriviaSpace
                ),
                pretty_print(AstNode::Expression(*subexpression)),
                pretty_print_token_full(&close_paren_token, IndentationChange::NoneTokenSpace),
            );
        }
    }
}
