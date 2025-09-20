use std::cell::RefMut;

use itertools::Itertools;

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LiteralExpression, OnStatement, OnStatementIterator, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody,
        StructExpression, StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor,
        UnaryExpression, UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    document_model::DocumentElement,
    escape::{QuoteType, escape},
    passes::pass_manager::{GlobalState, Pass, PassFactory, PassResult},
    tokenizer::{Keyword, Token, TokenType, Trivia, TriviaKind},
};

pub struct AstToDocumentModelConverter<'state> {
    program: Option<RefMut<'state, Program>>,
    output_de: Option<RefMut<'state, DocumentElement>>,

    /// Stores if newlines in the trivia are currently allowed at the start
    is_first_in_statement: bool,
}

impl PassFactory for AstToDocumentModelConverter<'_> {
    type Output<'state> = AstToDocumentModelConverter<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        let program = state.check_named()?;

        let output_de = state.insert(DocumentElement::empty());

        Ok(Self::Output {
            program: Some(program.get_mut(state)),
            output_de: Some(output_de.get_mut(state)),
            is_first_in_statement: false,
        })
    }
}
impl Pass for AstToDocumentModelConverter<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        let mut output_de = self.output_de.take().unwrap();
        *output_de = self.visit_program(&mut program);

        Ok(())
    }
}
struct TokenToElementConfig {
    leading_pre_spacer: DocumentElement,
    leading_post_spacer: DocumentElement,
    trailing_pre_spacer: DocumentElement,
    trailing_post_spacer: DocumentElement,
    allow_leading_newlines: bool,
    allow_trailing_newlines: bool,
}

impl TokenToElementConfig {
    fn semicolon() -> Self {
        Self {
            allow_leading_newlines: false,
            allow_trailing_newlines: true,
            ..Default::default()
        }
    }
    fn open_brace() -> Self {
        Self {
            allow_leading_newlines: true,
            allow_trailing_newlines: true,
            ..Default::default()
        }
    }

    fn open_paren() -> Self {
        Self {
            leading_pre_spacer: DocumentElement::empty(),
            leading_post_spacer: DocumentElement::single_collapsable_space(),
            trailing_pre_spacer: DocumentElement::empty(),
            trailing_post_spacer: DocumentElement::single_collapsable_space(),
            allow_leading_newlines: false,
            allow_trailing_newlines: false,
        }
    }
    fn close_paren() -> Self {
        Self {
            leading_pre_spacer: DocumentElement::single_collapsable_space(),
            leading_post_spacer: DocumentElement::empty(),
            trailing_pre_spacer: DocumentElement::single_collapsable_space(),
            trailing_post_spacer: DocumentElement::empty(),
            allow_leading_newlines: false,
            allow_trailing_newlines: false,
        }
    }
}

impl Default for TokenToElementConfig {
    fn default() -> Self {
        Self {
            leading_pre_spacer: DocumentElement::empty(),
            leading_post_spacer: DocumentElement::single_collapsable_space(),
            trailing_pre_spacer: DocumentElement::single_collapsable_space(),
            trailing_post_spacer: DocumentElement::empty(),
            allow_leading_newlines: false,
            allow_trailing_newlines: false,
        }
    }
}

impl AstToDocumentModelConverter<'_> {
    fn trivia_to_element(
        &mut self,
        trivia: &[Trivia],
        mut allow_newlines: bool,
    ) -> DocumentElement {
        allow_newlines |= self.is_first_in_statement;
        self.is_first_in_statement = false;

        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            trivia
                .iter()
                .filter_map(|t| match &t.kind {
                    TriviaKind::LineComment(contents) => {
                        Some(DocumentElement::Concatenation(vec![
                            DocumentElement::Text("// ".to_string()),
                            DocumentElement::Text(contents.trim().to_string()),
                            DocumentElement::single_collapsable_linebreak(),
                        ]))
                    }
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

                            if line.trim() == "" { "" } else { line }
                        });

                        Some(DocumentElement::Concatenation(vec![
                            DocumentElement::Text("/*".to_string()),
                            DocumentElement::spaced_concatentation(
                                DocumentElement::ForcedLineBreak,
                                lines
                                    .map(|line| DocumentElement::Text(line.to_string()))
                                    .collect(),
                            ),
                            DocumentElement::Text("*/".to_string()),
                        ]))
                    }
                    TriviaKind::EmptyLine => {
                        if allow_newlines {
                            Some(DocumentElement::CollapsableLineBreak { max: 2, count: 1 })
                        } else {
                            None
                        }
                    }
                })
                .collect(),
        )
    }

    fn keyword_string(&self, keyword: Keyword) -> String {
        match keyword {
            Keyword::On => "on",
            Keyword::Self_ => "self",
            Keyword::Let => "let",
            Keyword::I8 => "i8",
            Keyword::I16 => "i16",
            Keyword::I32 => "i32",
            Keyword::I64 => "i64",
            Keyword::U8 => "u8",
            Keyword::U16 => "u16",
            Keyword::U32 => "u32",
            Keyword::U64 => "u64",
            Keyword::F32 => "f32",
            Keyword::F64 => "f64",
            Keyword::Bool => "bool",
            Keyword::Idk => "idk",
            Keyword::As => "as",
            Keyword::Struct => "struct",
            Keyword::True => "true",
            Keyword::False => "false",
            Keyword::Return => "return",
            Keyword::If => "if",
            Keyword::Elif => "elif",
            Keyword::Else => "else",
            Keyword::While => "while",
            Keyword::For => "for",
            Keyword::Break => "break",
            Keyword::Skip => "skip",
            Keyword::Extern => "extern",
        }
        .to_string()
    }

    fn token_type_to_element(&self, token: &Token) -> DocumentElement {
        #[rustfmt::skip]
        let s = match token.type_ {
            TokenType::Keyword(keyword) => &self.keyword_string(keyword),
            TokenType::Identifier(ref id) => id.as_str(),

            TokenType::Integer(_, ref str) => str,
            TokenType::Float(_, ref str) => str,
            TokenType::StringLiteral(ref str) => &('"'.to_string() + &escape(str, QuoteType::Double) + "\""),
            TokenType::CharLiteral(ref str) => &("'".to_string() + &escape(str, QuoteType::Single) + "'"),

            TokenType::UnknownCharacters(ref chars) => chars,

            TokenType::OpenBrace        => "{",
            TokenType::CloseBrace       => "}",
            TokenType::OpenParen        => "(",
            TokenType::CloseParen       => ")",
            TokenType::OpenBracket      => "[",
            TokenType::CloseBracket     => "]",

            TokenType::Semicolon        => ";",
            TokenType::Comma            => ",",
            TokenType::Dot              => ".",
            TokenType::At               => "@",
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

        DocumentElement::Text(s.to_string())
    }

    fn token_to_element(&mut self, token: &Token) -> DocumentElement {
        self.token_to_element_config(token, Default::default())
    }
    fn token_to_element_config(
        &mut self,
        token: &Token,
        TokenToElementConfig {
            leading_pre_spacer,
            leading_post_spacer,
            trailing_pre_spacer,
            trailing_post_spacer,
            allow_leading_newlines,
            allow_trailing_newlines,
        }: TokenToElementConfig,
    ) -> DocumentElement {
        let mut elements = vec![];
        let leading_trivia_element =
            self.trivia_to_element(&token.leading_trivia, allow_leading_newlines);
        if leading_trivia_element != DocumentElement::Concatenation(vec![]) {
            elements.push(leading_pre_spacer);
            elements.push(leading_trivia_element);
            elements.push(leading_post_spacer);
        }

        elements.push(self.token_type_to_element(token));

        let trailing_trivia_element =
            self.trivia_to_element(&token.trailing_trivia, allow_trailing_newlines);
        if trailing_trivia_element != DocumentElement::Concatenation(vec![]) {
            elements.push(trailing_pre_spacer);
            elements.push(trailing_trivia_element);
            elements.push(trailing_post_spacer);
        }

        DocumentElement::Concatenation(elements)
    }

    fn braced_broken_block(
        &mut self,
        open_paren: &Token,
        body: Vec<DocumentElement>,
        close_paren: &Token,
    ) -> DocumentElement {
        self.is_first_in_statement = true;
        if body.is_empty() {
            DocumentElement::Concatenation(vec![
                self.token_to_element(open_paren),
                DocumentElement::ReverseLineBreakEater,
                DocumentElement::LineBreakEater,
                self.token_to_element(close_paren),
            ])
        } else {
            DocumentElement::spaced_concatentation(
                DocumentElement::single_collapsable_linebreak(),
                vec![
                    self.token_to_element_config(open_paren, TokenToElementConfig::open_brace()),
                    DocumentElement::LineBreakEater,
                    DocumentElement::Indentation(Box::new(DocumentElement::spaced_concatentation(
                        DocumentElement::single_collapsable_linebreak(),
                        vec![
                            DocumentElement::spaced_concatentation(
                                DocumentElement::single_collapsable_linebreak(),
                                body,
                            ),
                            self.trivia_to_element(&close_paren.leading_trivia, true),
                        ],
                    ))),
                    DocumentElement::ReverseLineBreakEater,
                    DocumentElement::spaced_concatentation(
                        DocumentElement::single_collapsable_space(),
                        vec![
                            // valid use of token_type_to_element because we break the trivia across indentation boundaries
                            self.token_type_to_element(close_paren),
                            self.trivia_to_element(&close_paren.trailing_trivia, true),
                        ],
                    ),
                ],
            )
        }
    }
}

impl AstVisitor for AstToDocumentModelConverter<'_> {
    type ProgramOutput = DocumentElement;
    type TopLevelOutput = DocumentElement;
    type FunctionBodyOutput = DocumentElement;
    type SimpleBindingOutput = DocumentElement;
    type StatementOutput = DocumentElement;
    type ExecutorHostOutput = DocumentElement;
    type ExecutorOutput = DocumentElement;
    type ExpressionOutput = DocumentElement;
    type LValueOutput = DocumentElement;
    type TypeOutput = DocumentElement;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        DocumentElement::Concatenation(vec![
            DocumentElement::LineBreakEater,
            DocumentElement::spaced_concatentation(
                DocumentElement::single_collapsable_linebreak(),
                program
                    .top_level_statements
                    .iter_mut()
                    .map(|tls| self.visit_top_level_statement(tls))
                    .collect(),
            ),
            DocumentElement::ReverseLineBreakEater,
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
            parameters,
            close_paren_token,
            right_arrow_token,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) -> Self::TopLevelOutput {
        self.is_first_in_statement = true;
        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            vec![
                self.token_to_element(let_token),
                self.token_to_element(name_token),
                self.token_to_element(equal_token),
                DocumentElement::Concatenation(vec![
                    self.token_to_element_config(
                        open_paren_token,
                        TokenToElementConfig::open_paren(),
                    ),
                    DocumentElement::spaced_concatentation(
                        DocumentElement::single_collapsable_space(),
                        parameters
                            .iter_mut()
                            .map(|(param, comma)| {
                                if let Some(comma) = comma {
                                    DocumentElement::Concatenation(vec![
                                        self.visit_simple_binding(param),
                                        self.token_to_element(comma),
                                    ])
                                } else {
                                    self.visit_simple_binding(param)
                                }
                            })
                            .collect(),
                    ),
                    DocumentElement::ReverseSpaceEater, // consumes the open_brace space if there is a comment in the parens
                    self.token_to_element_config(
                        close_paren_token,
                        TokenToElementConfig::close_paren(),
                    ),
                ]),
                self.token_to_element(right_arrow_token),
                if let Some(return_type) = return_type {
                    self.visit_type(return_type)
                } else {
                    DocumentElement::empty()
                },
                DocumentElement::LineBreakEater,
                self.visit_function_body(body),
            ],
        )
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_statement(statement)
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token,
            extern_token,
            symbol: _,
            symbol_token,
            semicolon_token,
            id: _,
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        DocumentElement::Concatenation(vec![
            self.token_to_element(at_token),
            self.token_to_element(extern_token),
            DocumentElement::single_collapsable_space(),
            self.token_to_element(symbol_token),
            self.token_to_element_config(semicolon_token, TokenToElementConfig::semicolon()),
        ])
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token,
            name: _,
            type_,
            id: _,
        }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        if let Some((colon_token, type_)) = type_ {
            DocumentElement::Concatenation(vec![
                self.token_to_element(name_token),
                self.token_to_element(colon_token),
                DocumentElement::single_collapsable_space(),
                self.visit_type(type_),
            ])
        } else {
            self.token_to_element(name_token)
        }
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.is_first_in_statement = true;
        DocumentElement::Concatenation(vec![
            self.visit_expression(expression),
            self.token_to_element_config(semicolon_token, TokenToElementConfig::semicolon()),
        ])
    }

    fn visit_on_statement(
        &mut self,
        OnStatement {
            on_token,
            executor,
            iterators,
            open_paren_token,
            bindings,
            close_paren_token,
            body,
            id: _,
        }: &mut OnStatement,
    ) -> Self::StatementOutput {
        self.is_first_in_statement = true;
        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            vec![
                self.token_to_element(on_token),
                DocumentElement::Concatenation(vec![
                    self.visit_executor(executor),
                    DocumentElement::Concatenation(
                        iterators
                            .iter_mut()
                            .map(
                                |OnStatementIterator {
                                     open_bracket_token,
                                     binding,
                                     equal_token,
                                     max_value,
                                     close_bracket_token,
                                 }| {
                                    vec![
                                        self.token_to_element(open_bracket_token),
                                        self.visit_simple_binding(binding),
                                        DocumentElement::single_collapsable_space(),
                                        self.token_to_element(equal_token),
                                        DocumentElement::single_collapsable_space(),
                                        self.visit_expression(max_value),
                                        self.token_to_element(close_bracket_token),
                                    ]
                                },
                            )
                            .concat(),
                    ),
                ]),
                DocumentElement::Concatenation(vec![
                    self.token_to_element(open_paren_token),
                    DocumentElement::spaced_concatentation(
                        DocumentElement::single_collapsable_space(),
                        bindings
                            .iter_mut()
                            .map(|(binding, comma)| {
                                if let Some(comma) = comma {
                                    DocumentElement::Concatenation(vec![
                                        self.visit_lvalue(binding),
                                        self.token_to_element(comma),
                                    ])
                                } else {
                                    self.visit_lvalue(binding)
                                }
                            })
                            .collect(),
                    ),
                    self.token_to_element(close_paren_token),
                ]),
                DocumentElement::LineBreakEater,
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
        let body = body
            .iter_mut()
            .map(|stmt| self.visit_statement(stmt))
            .collect();
        self.braced_broken_block(open_brace_token, body, close_brace_token)
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
        self.is_first_in_statement = true;
        DocumentElement::Concatenation(vec![
            if let Some(value) = value {
                DocumentElement::Concatenation(vec![
                    self.token_to_element(return_token),
                    DocumentElement::single_collapsable_space(),
                    self.visit_expression(value),
                ])
            } else {
                self.token_to_element(return_token)
            },
            self.token_to_element_config(semicolon_token, TokenToElementConfig::semicolon()),
        ])
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            let_token,
            binding,
            equals_token,
            value,
            semicolon_token,
            id: _,
        }: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.is_first_in_statement = true;
        DocumentElement::Concatenation(vec![
            DocumentElement::spaced_concatentation(
                DocumentElement::single_collapsable_space(),
                vec![
                    self.token_to_element(let_token),
                    self.visit_simple_binding(binding),
                    self.token_to_element(equals_token),
                    self.visit_expression(value),
                ],
            ),
            self.token_to_element_config(semicolon_token, TokenToElementConfig::semicolon()),
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
        self.is_first_in_statement = true;
        let mut elements = vec![DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            vec![
                self.token_to_element(if_token),
                self.visit_expression(condition),
                DocumentElement::LineBreakEater,
                self.visit_statement(if_body),
            ],
        )];

        for (token, condition, body) in elifs {
            elements.push(DocumentElement::spaced_concatentation(
                DocumentElement::single_collapsable_space(),
                vec![
                    self.token_to_element(token),
                    self.visit_expression(condition),
                    DocumentElement::LineBreakEater,
                    self.visit_statement(body),
                ],
            ));
        }

        if let Some((else_token, else_body)) = else_ {
            elements.push(DocumentElement::spaced_concatentation(
                DocumentElement::single_collapsable_space(),
                vec![
                    self.token_to_element(else_token),
                    DocumentElement::LineBreakEater,
                    self.visit_statement(else_body),
                ],
            ));
        }

        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_linebreak(),
            elements,
        )
    }

    fn visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token,
            condition,
            body,
            id: _,
        }: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.is_first_in_statement = true;
        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            vec![
                self.token_to_element(while_token),
                self.visit_expression(condition),
                DocumentElement::LineBreakEater,
                self.visit_statement(body),
            ],
        )
    }

    fn visit_for_loop_statement(
        &mut self,
        ForLoopStatement {
            for_token,
            open_paren_token,
            initializer,
            condition,
            second_semicolon_token,
            incrementer,
            close_paren_token,
            body,
            id: _,
        }: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.is_first_in_statement = true;
        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            vec![
                self.token_to_element(for_token),
                DocumentElement::Concatenation(vec![
                    self.token_to_element(open_paren_token),
                    DocumentElement::Concatenation(vec![
                        self.visit_statement(initializer),
                        condition
                            .as_mut()
                            .map(|con| {
                                DocumentElement::Concatenation(vec![
                                    DocumentElement::single_collapsable_space(),
                                    self.visit_expression(con),
                                    self.token_to_element(second_semicolon_token),
                                ])
                            })
                            .unwrap_or(self.token_to_element(second_semicolon_token)),
                        incrementer
                            .as_mut()
                            .map(|inc| {
                                DocumentElement::Concatenation(vec![
                                    DocumentElement::single_collapsable_space(),
                                    self.visit_expression(inc),
                                ])
                            })
                            .unwrap_or(DocumentElement::empty()),
                    ]),
                    self.token_to_element(close_paren_token),
                ]),
                DocumentElement::LineBreakEater,
                self.visit_statement(body),
            ],
        )
    }

    fn visit_break_statement(
        &mut self,
        BreakStatement {
            break_token,
            semicolon_token,
            id: _,
        }: &mut BreakStatement,
    ) -> Self::StatementOutput {
        self.is_first_in_statement = true;
        DocumentElement::Concatenation(vec![
            self.token_to_element(break_token),
            self.token_to_element_config(semicolon_token, TokenToElementConfig::semicolon()),
        ])
    }

    fn visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token,
            semicolon_token,
            id: _,
        }: &mut SkipStatement,
    ) -> Self::StatementOutput {
        self.is_first_in_statement = true;
        DocumentElement::Concatenation(vec![
            self.token_to_element(skip_token),
            self.token_to_element_config(semicolon_token, TokenToElementConfig::semicolon()),
        ])
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.token_to_element(&executor_host.token)
    }

    fn visit_thread_executor(
        &mut self,
        ThreadExecutor {
            host,
            dot_token,
            thread_token,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &mut ThreadExecutor,
    ) -> Self::ExecutorOutput {
        DocumentElement::Concatenation(vec![
            self.visit_executor_host(host),
            self.token_to_element(dot_token),
            self.token_to_element(thread_token),
            self.token_to_element(open_bracket_token),
            self.visit_expression(index),
            self.token_to_element(close_bracket_token),
        ])
    }

    fn visit_gpu_executor(
        &mut self,
        GPUExecutor {
            host,
            dot_token,
            gpus_token,
            open_bracket_token: open_bracket_token_1,
            gpu_index,
            close_bracket_token: close_bracket_token_1,
            id: _,
        }: &mut GPUExecutor,
    ) -> Self::ExecutorOutput {
        DocumentElement::Concatenation(vec![
            self.visit_executor_host(host),
            self.token_to_element(dot_token),
            self.token_to_element(gpus_token),
            self.token_to_element(open_bracket_token_1),
            self.visit_expression(gpu_index),
            self.token_to_element(close_bracket_token_1),
        ])
    }

    fn visit_literal_expression(
        &mut self,
        expression: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        self.token_to_element(&expression.token)
    }

    fn visit_array_expression(
        &mut self,
        ArrayExpression {
            open_bracket_token,
            elements,
            close_bracket_token,
            id: _,
        }: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::Concatenation(vec![
            self.token_to_element(open_bracket_token),
            DocumentElement::spaced_concatentation(
                DocumentElement::single_collapsable_space(),
                elements
                    .iter_mut()
                    .map(|(item, comma)| match comma {
                        Some(comma) => DocumentElement::Concatenation(vec![
                            self.visit_expression(item),
                            self.token_to_element(comma),
                        ]),
                        None => self.visit_expression(item),
                    })
                    .collect(),
            ),
            self.token_to_element(close_bracket_token),
        ])
    }

    fn visit_struct_expression(
        &mut self,
        StructExpression {
            type_,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        }: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        let members = members
            .iter_mut()
            .map(
                |(
                    StructMemberValue {
                        name: _,
                        name_token,
                        colon_token,
                        value,
                    },
                    comma,
                )| {
                    DocumentElement::Concatenation(vec![
                        self.token_to_element(name_token),
                        self.token_to_element(colon_token),
                        DocumentElement::single_collapsable_space(),
                        self.visit_expression(value),
                        comma
                            .as_mut()
                            .map(|comma| self.token_to_element(comma))
                            .unwrap_or_else(DocumentElement::empty),
                    ])
                },
            )
            .collect();

        DocumentElement::Concatenation(vec![
            self.visit_type(type_),
            DocumentElement::single_collapsable_space(),
            self.braced_broken_block(open_brace_token, members, close_brace_token),
        ])
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
                    DocumentElement::single_collapsable_space(),
                    arguments
                        .iter_mut()
                        .map(|(arg, comma)| match comma {
                            Some(comma) => DocumentElement::Concatenation(vec![
                                self.visit_expression(arg),
                                self.token_to_element(comma),
                            ]),
                            None => self.visit_expression(arg),
                        })
                        .collect(),
                ),
                self.token_to_element(close_paren_token),
            ]),
        ])
    }

    fn visit_compiler_expression(
        &mut self,
        CompilerExpression {
            at_token,
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        }: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::Concatenation(vec![
            self.token_to_element(at_token),
            self.token_to_element(name_token),
            DocumentElement::Concatenation(vec![
                self.token_to_element(open_paren_token),
                DocumentElement::spaced_concatentation(
                    DocumentElement::single_collapsable_space(),
                    arguments
                        .iter_mut()
                        .map(|(arg, comma)| match comma {
                            Some(comma) => DocumentElement::Concatenation(vec![
                                self.visit_expression(arg),
                                self.token_to_element(comma),
                            ]),
                            None => self.visit_expression(arg),
                        })
                        .collect(),
                ),
                self.token_to_element(close_paren_token),
            ]),
        ])
    }

    fn visit_array_index_expression(
        &mut self,
        ArrayIndexExpression {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::Concatenation(vec![
            self.visit_expression(array),
            self.token_to_element(open_bracket_token),
            self.visit_expression(index),
            self.token_to_element(close_bracket_token),
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
            self.token_to_element_config(open_paren_token, TokenToElementConfig::open_paren()),
            self.visit_expression(subexpression),
            self.token_to_element_config(close_paren_token, TokenToElementConfig::close_paren()),
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
            self.visit_expression(operand),
        ])
    }

    fn visit_cast_expression(
        &mut self,
        CastExpression {
            operand,
            as_token,
            type_,
            id: _,
        }: &mut CastExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            vec![
                self.visit_expression(operand),
                self.token_to_element(as_token),
                self.visit_type(type_),
            ],
        )
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
            DocumentElement::single_collapsable_space(),
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
            lvalue,
            equal_token,
            right,
            id: _,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        DocumentElement::spaced_concatentation(
            DocumentElement::single_collapsable_space(),
            vec![
                self.visit_lvalue(lvalue),
                self.token_to_element(equal_token),
                self.visit_expression(right),
            ],
        )
    }

    fn visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name: _,
            name_token,
            id: _,
        }: &mut VariableLValue,
    ) -> Self::LValueOutput {
        self.token_to_element(name_token)
    }

    fn visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &mut ArrayIndexLValue,
    ) -> Self::LValueOutput {
        DocumentElement::Concatenation(vec![
            self.visit_lvalue(array),
            self.token_to_element(open_bracket_token),
            self.visit_expression(index),
            self.token_to_element(close_bracket_token),
        ])
    }

    fn visit_grouping_lvalue(
        &mut self,
        GroupingLValue {
            open_paren_token,
            sublvalue,
            close_paren_token,
            id: _,
        }: &mut GroupingLValue,
    ) -> Self::LValueOutput {
        DocumentElement::Concatenation(vec![
            self.token_to_element(open_paren_token),
            self.visit_lvalue(sublvalue),
            self.token_to_element(close_paren_token),
        ])
    }

    fn visit_simple_type(&mut self, type_: &mut SimpleType) -> Self::TypeOutput {
        self.token_to_element(&type_.token)
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        DocumentElement::Concatenation(vec![
            self.token_to_element(&unit_type.open_paren_token),
            self.token_to_element(&unit_type.close_paren_token),
        ])
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.token_to_element(&idk_type.token)
    }

    fn visit_array_type(
        &mut self,
        ArrayType {
            subtype,
            open_bracket_token,
            size,
            close_bracket_token,
            id: _,
        }: &mut ArrayType,
    ) -> Self::TypeOutput {
        DocumentElement::Concatenation(vec![
            self.visit_type(subtype),
            self.token_to_element(open_bracket_token),
            if let Some(size) = size {
                self.visit_expression(size)
            } else {
                DocumentElement::empty()
            },
            self.token_to_element(close_bracket_token),
        ])
    }

    fn visit_struct_type(
        &mut self,
        StructType {
            struct_token,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        }: &mut StructType,
    ) -> Self::TypeOutput {
        let members = members
            .iter_mut()
            .map(
                |(
                    StructMemberDefinition {
                        name: _,
                        name_token,
                        colon_token,
                        type_,
                    },
                    comma,
                )| {
                    DocumentElement::Concatenation(vec![
                        self.token_to_element(name_token),
                        self.token_to_element(colon_token),
                        DocumentElement::single_collapsable_space(),
                        self.visit_type(type_),
                        comma
                            .as_mut()
                            .map(|comma| self.token_to_element(comma))
                            .unwrap_or_else(DocumentElement::empty),
                    ])
                },
            )
            .collect();

        DocumentElement::Concatenation(vec![
            self.token_to_element(struct_token),
            DocumentElement::single_collapsable_space(),
            self.braced_broken_block(open_brace_token, members, close_brace_token),
        ])
    }
}
