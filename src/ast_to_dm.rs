use crate::{
    ast::{
        AstVisitor, BinaryExpression, BlockStatement, ExpressionStatement, FunctionCallExpression,
        FunctionDefinition, GroupingExpression, IfStatement, NumberExpression, OnStatement,
        Program, ReturnStatement, SelfExecutorHost, ThreadExecutor, Type, UnaryExpression,
        VariableAccessExpression, VariableAssignmentExpression, VariableDefinitionStatement,
    },
    document_model::DocumentElement,
    tokenizer::{Keyword, Token, TokenType, Trivia, TriviaKind},
};

pub struct AstToDocumentModelConverter {}

impl AstToDocumentModelConverter {
    pub fn new() -> Self {
        Self {}
    }

    fn trivia_to_element(&self, trivia: &Vec<Trivia>) -> DocumentElement {
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

    fn keyword_string(&self, keyword: Keyword) -> String {
        match keyword {
            Keyword::On => "on",
            Keyword::Self_ => "self",
            Keyword::Let => "let",
            Keyword::I32 => "i32",
            Keyword::Return => "return",
            Keyword::If => "if",
            Keyword::Elif => "elif",
            Keyword::Else => "else",
        }
        .to_string()
    }

    fn token_type_to_element(&self, token: &Token) -> DocumentElement {
        #[rustfmt::skip]
        let s = match token.type_ {
            TokenType::Keyword(keyword) => &self.keyword_string(keyword),
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

    fn token_to_element(&self, token: &Token) -> DocumentElement {
        DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                self.trivia_to_element(&token.leading_trivia),
                self.token_type_to_element(token),
                self.trivia_to_element(&token.trailing_trivia),
            ],
        )
    }
}

impl AstVisitor for AstToDocumentModelConverter {
    type ProgramOutput = DocumentElement;
    type FunctionDefinitionOutput = DocumentElement;
    type StatementOutput = DocumentElement;
    type ExecutorHostOutput = DocumentElement;
    type ExecutorOutput = DocumentElement;
    type ExpressionOutput = DocumentElement;
    type TypeOutput = DocumentElement;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        DocumentElement::Concatenation(vec![
            DocumentElement::SpaceEater,
            DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableLineBreak,
                program
                    .functions
                    .iter_mut()
                    .map(|f| self.visit_function_definition(f))
                    .collect(),
            ),
            DocumentElement::ReverseSpaceEater,
        ])
    }

    fn visit_function_definition(
        &mut self,
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
            id: _,
        }: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                self.token_to_element(let_token),
                self.token_to_element(name_token),
                self.token_to_element(equal_token),
                DocumentElement::Concatenation(vec![
                    self.trivia_to_element(&open_paren_token.leading_trivia),
                    DocumentElement::CollapsableSpace,
                    self.token_type_to_element(open_paren_token),
                    self.trivia_to_element(&open_paren_token.trailing_trivia),
                    //
                    self.trivia_to_element(&close_paren_token.leading_trivia),
                    self.token_type_to_element(close_paren_token),
                    DocumentElement::CollapsableSpace,
                    self.trivia_to_element(&close_paren_token.trailing_trivia),
                ]),
                self.token_to_element(right_arrow_token),
                self.visit_type(return_type),
                self.visit_statement(body),
            ],
        )
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        DocumentElement::Concatenation(vec![
            self.visit_expression(expression),
            DocumentElement::double_space_eater(),
            self.token_to_element(semicolon_token),
        ])
    }

    fn visit_on_statement(
        &mut self,
        OnStatement {
            on_token,
            open_paren_token,
            executor,
            close_paren_token,
            body,
            id: _,
        }: &mut OnStatement,
    ) -> Self::StatementOutput {
        DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                self.token_to_element(on_token),
                DocumentElement::Concatenation(vec![
                    self.token_to_element(open_paren_token),
                    self.visit_executor(executor),
                    self.token_to_element(close_paren_token),
                ]),
                self.visit_statement(body),
            ],
        )
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token,
            body,
            close_brace_token,
            id: _,
        }: &mut BlockStatement,
    ) -> Self::StatementOutput {
        DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableLineBreak,
            vec![
                DocumentElement::spaced_concatentation(
                    DocumentElement::CollapsableSpace,
                    vec![
                        self.trivia_to_element(&open_brace_token.leading_trivia),
                        self.token_type_to_element(open_brace_token),
                        self.trivia_to_element(&open_brace_token.trailing_trivia),
                    ],
                ),
                DocumentElement::Indentation(Box::new(DocumentElement::spaced_concatentation(
                    DocumentElement::CollapsableLineBreak,
                    vec![
                        DocumentElement::spaced_concatentation(
                            DocumentElement::CollapsableLineBreak,
                            body.iter_mut().map(|s| self.visit_statement(s)).collect(),
                        ),
                        self.trivia_to_element(&close_brace_token.leading_trivia),
                    ],
                ))),
                DocumentElement::spaced_concatentation(
                    DocumentElement::CollapsableSpace,
                    vec![
                        self.token_type_to_element(close_brace_token),
                        self.trivia_to_element(&close_brace_token.trailing_trivia),
                    ],
                ),
            ],
        )
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement {
            return_token,
            value,
            semicolon_token,
            id: _,
        }: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        DocumentElement::Concatenation(vec![
            DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableSpace,
                vec![
                    self.token_to_element(return_token),
                    self.visit_expression(value),
                ],
            ),
            DocumentElement::double_space_eater(),
            DocumentElement::double_space_eater(),
            self.token_to_element(semicolon_token),
        ])
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            let_token,
            name_token,
            name: _,
            colon_token,
            type_,
            equals_token,
            value,
            semicolon_token,
            id: _,
        }: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        DocumentElement::Concatenation(vec![
            DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableSpace,
                vec![
                    self.token_to_element(let_token),
                    self.token_to_element(name_token),
                    DocumentElement::double_space_eater(),
                    self.token_to_element(colon_token),
                    self.visit_type(type_),
                    self.token_to_element(equals_token),
                    self.visit_expression(value),
                ],
            ),
            DocumentElement::double_space_eater(),
            DocumentElement::double_space_eater(),
            self.token_to_element(semicolon_token),
        ])
    }

    fn visit_if_statement(
        &mut self,
        IfStatement {
            if_token,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        }: &mut IfStatement,
    ) -> Self::StatementOutput {
        let mut elements = vec![DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                self.token_to_element(if_token),
                self.visit_expression(condition),
                self.visit_statement(if_body),
            ],
        )];

        for (token, condition, body) in elifs {
            elements.push(DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableSpace,
                vec![
                    self.token_to_element(token),
                    self.visit_expression(condition),
                    self.visit_statement(body),
                ],
            ));
        }

        if let Some((else_token, else_body)) = else_ {
            elements.push(DocumentElement::spaced_concatentation(
                DocumentElement::CollapsableSpace,
                vec![
                    self.token_to_element(else_token),
                    self.visit_statement(else_body),
                ],
            ));
        }

        return DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableLineBreak,
            elements,
        );
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.token_to_element(&executor_host.token)
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        DocumentElement::Concatenation(vec![
            self.visit_executor_host(&mut executor.host),
            self.token_to_element(&mut executor.dot_token),
            self.token_to_element(&mut executor.thread_token),
            self.token_to_element(&mut executor.open_bracket_token),
            self.visit_expression(&mut executor.index),
            self.token_to_element(&mut executor.close_bracket_token),
        ])
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        self.token_to_element(&expression.token)
    }

    fn visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        }: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::Concatenation(vec![
            self.token_to_element(name_token),
            DocumentElement::Concatenation(vec![
                self.token_to_element(open_paren_token),
                DocumentElement::spaced_concatentation(
                    DocumentElement::Concatenation(vec![
                        DocumentElement::Text(",".to_string()),
                        DocumentElement::CollapsableSpace,
                    ]),
                    arguments
                        .iter_mut()
                        .map(|arg| self.visit_expression(arg))
                        .collect(),
                ),
                self.token_to_element(close_paren_token),
            ]),
        ])
    }

    fn visit_grouping_expression(
        &mut self,
        GroupingExpression {
            open_paren_token,
            subexpression,
            close_paren_token,
            id: _,
        }: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::Concatenation(vec![
            DocumentElement::Concatenation(vec![
                self.trivia_to_element(&open_paren_token.leading_trivia),
                DocumentElement::CollapsableSpace,
                self.token_type_to_element(open_paren_token),
                DocumentElement::double_space_eater(),
                self.trivia_to_element(&open_paren_token.trailing_trivia),
            ]),
            self.visit_expression(subexpression),
            DocumentElement::Concatenation(vec![
                self.trivia_to_element(&close_paren_token.leading_trivia),
                DocumentElement::double_space_eater(),
                self.token_type_to_element(close_paren_token),
                DocumentElement::CollapsableSpace,
                self.trivia_to_element(&close_paren_token.trailing_trivia),
            ]),
        ])
    }

    fn visit_variable_access_expression(
        &mut self,
        VariableAccessExpression {
            name: _,
            name_token,
            id: _,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.token_to_element(name_token)
    }

    fn visit_unary_expression(
        &mut self,
        UnaryExpression {
            operator_token,
            operation: _,
            operand,
            id: _,
        }: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::Concatenation(vec![
            self.token_to_element(operator_token),
            DocumentElement::double_space_eater(),
            self.visit_expression(operand),
        ])
    }

    fn visit_binary_expression(
        &mut self,
        BinaryExpression {
            left,
            operator_token,
            operation: _,
            right,
            id: _,
        }: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                self.visit_expression(left),
                self.token_to_element(operator_token),
                self.visit_expression(right),
            ],
        )
    }

    fn visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            name: _,
            name_token,
            equal_token,
            right,
            id: _,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::spaced_concatentation(
            DocumentElement::CollapsableSpace,
            vec![
                self.token_to_element(name_token),
                self.token_to_element(equal_token),
                self.visit_expression(right),
            ],
        )
    }

    fn visit_type(&mut self, type_: &mut Type) -> Self::TypeOutput {
        match type_ {
            Type::I32 { token, id: _ } => self.token_to_element(token),
        }
    }
}
