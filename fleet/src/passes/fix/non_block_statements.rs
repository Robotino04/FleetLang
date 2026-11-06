use std::ops::ControlFlow;

use crate::{
    ast::{
        BlockStatement, ForLoopStatement, FunctionBody, FunctionDefinition, HasSourceRange,
        IfStatement, Program, Statement, WhileLoopStatement,
        partial_visitor::{AstWalker, PartialAstVisitor},
    },
    error_reporting::{BlockRequiredKind, ErrorKind, Errors},
    passes::{
        prelude::*,
        proc::{
            parser::IdGenerator,
            tokenizer::{Token, TokenType},
        },
    },
};

pub struct FixNonBlockStatements<'state> {
    errors: &'state mut Errors,
    program: Option<&'state mut Program>,
    id_generator: &'state mut IdGenerator,
}

impl PassFactory for FixNonBlockStatements<'_> {
    type Output<'state> = FixNonBlockStatements<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, FetchError> {
        let Fetched {
            read: (),
            write: (errors, program, id_generator),
            empty: (),
        } = state.fetch()?;

        Ok(Self::Output {
            errors,
            program: Some(program),
            id_generator,
        })
    }
}
impl Pass for FixNonBlockStatements<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = self.program.take().unwrap();

        let _ = self.visit_program(program);

        Ok(())
    }
}

impl FixNonBlockStatements<'_> {
    fn create_fake_block_around(&mut self, node: &mut Statement) -> Statement {
        let range = node.get_source_range();

        Statement::Block(BlockStatement {
            open_brace_token: Token {
                type_: TokenType::OpenBrace,
                range: range.start().until(range.start()),
                leading_trivia: vec![],
                trailing_trivia: vec![],
            },
            body: vec![node.clone()],
            close_brace_token: Token {
                type_: TokenType::CloseBrace,
                range: range.end().until(range.end()),
                leading_trivia: vec![],
                trailing_trivia: vec![],
            },
            id: self.id_generator.next_node_id(),
        })
    }
}

impl PartialAstVisitor for FixNonBlockStatements<'_> {
    type Break<'a> = ();

    fn visit_function_definition(&mut self, fdef: &mut FunctionDefinition) -> ControlFlow<()> {
        if let FunctionBody::Statement(stmt_body) = &mut *fdef.body
            && !matches!(stmt_body.statement, Statement::Block { .. })
        {
            self.errors.push(ErrorKind::BlockRequired {
                kind: BlockRequiredKind::Function,
                stmt_range: stmt_body.statement.get_source_range(),
            });

            stmt_body.statement = self.create_fake_block_around(&mut stmt_body.statement);
        }

        self.walk_function_definition(fdef)
    }

    fn visit_if_statement(&mut self, stmt: &mut IfStatement) -> ControlFlow<()> {
        let IfStatement {
            if_token: _,
            condition: _,
            if_body,
            elifs,
            else_,
            id: _,
        } = stmt;

        if !matches!(**if_body, Statement::Block { .. }) {
            self.errors.push(ErrorKind::BlockRequired {
                kind: BlockRequiredKind::If,
                stmt_range: if_body.get_source_range(),
            });

            **if_body = self.create_fake_block_around(&mut *if_body);
        }

        for (_token, _condition, elif_body) in elifs {
            if !matches!(elif_body, Statement::Block { .. }) {
                self.errors.push(ErrorKind::BlockRequired {
                    kind: BlockRequiredKind::Elif,
                    stmt_range: elif_body.get_source_range(),
                });

                *elif_body = self.create_fake_block_around(elif_body);
            }
        }

        if let Some((_token, else_body)) = else_
            && !matches!(**else_body, Statement::Block { .. })
        {
            self.errors.push(ErrorKind::BlockRequired {
                kind: BlockRequiredKind::Else,
                stmt_range: else_body.get_source_range(),
            });

            **else_body = self.create_fake_block_around(&mut *else_body);
        }

        self.walk_if_statement(stmt)
    }

    fn visit_while_loop_statement(&mut self, stmt: &mut WhileLoopStatement) -> ControlFlow<()> {
        if !matches!(*stmt.body, Statement::Block { .. }) {
            self.errors.push(ErrorKind::BlockRequired {
                kind: BlockRequiredKind::While,
                stmt_range: stmt.body.get_source_range(),
            });

            *stmt.body = self.create_fake_block_around(&mut stmt.body);
        }

        self.walk_while_loop_statement(stmt)
    }

    fn visit_for_loop_statement(&mut self, stmt: &mut ForLoopStatement) -> ControlFlow<()> {
        if !matches!(*stmt.body, Statement::Block { .. }) {
            self.errors.push(ErrorKind::BlockRequired {
                kind: BlockRequiredKind::For,
                stmt_range: stmt.body.get_source_range(),
            });

            *stmt.body = self.create_fake_block_around(&mut stmt.body);
        }

        self.walk_for_loop_statement(stmt)
    }
}
