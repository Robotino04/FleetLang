use std::cell::RefMut;

use crate::{
    ast::{
        AstVisitor, BlockStatement, ForLoopStatement, FunctionBody, FunctionDefinition,
        IfStatement, Program, Statement, WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    parser::IdGenerator,
    passes::pass_manager::{Errors, GlobalState, Pass, PassFactory, PassResult},
    tokenizer::{Token, TokenType},
};

use super::{find_node_bounds::find_node_bounds, partial_visitor::PartialAstVisitor};

pub struct FixNonBlockStatements<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,
    id_generator: RefMut<'state, IdGenerator>,
}

impl PassFactory for FixNonBlockStatements<'_> {
    type Output<'state> = FixNonBlockStatements<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        Ok(Self::Output {
            errors: state.get_mut_named::<Errors>()?,
            program: Some(state.get_mut_named::<Program>()?),
            id_generator: state.get_mut_named::<IdGenerator>()?,
        })
    }
}
impl Pass for FixNonBlockStatements<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        self.visit_program(&mut program);

        Ok(())
    }
}

impl FixNonBlockStatements<'_> {
    fn create_fake_block_arround(&mut self, node: &mut Statement) -> Statement {
        let range = find_node_bounds(node);

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
    fn partial_visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) {
        for (param, _comma) in parameters {
            self.visit_simple_binding(param);
        }

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }

        let body_clone = body.clone();
        if let FunctionBody::Statement(stmt_body) = &mut **body
            && !matches!(stmt_body.statement, Statement::Block { .. })
        {
            self.errors.push(FleetError::from_node(
                &*body_clone,
                "Functions must have a block as the body",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                &*body_clone,
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            stmt_body.statement = self.create_fake_block_arround(&mut stmt_body.statement);
        }

        self.visit_function_body(body);
    }

    fn partial_visit_if_statement(
        &mut self,
        IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        }: &mut IfStatement,
    ) {
        if !matches!(**if_body, Statement::Block { .. }) {
            self.errors.push(FleetError::from_node(
                &**if_body,
                "If statements must always have a block as the body.",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                &**if_body,
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            **if_body = self.create_fake_block_arround(&mut *if_body);
        }

        self.visit_expression(condition);
        self.visit_statement(if_body);

        for (_token, elif_condition, elif_body) in elifs {
            if !matches!(elif_body, Statement::Block { .. }) {
                self.errors.push(FleetError::from_node(
                    elif_body,
                    "Elif statements must always have a block as the body.",
                    ErrorSeverity::Error,
                ));
                self.errors.push(FleetError::from_node(
                    elif_body,
                    "Formatting will fix this",
                    ErrorSeverity::Note,
                ));

                *elif_body = self.create_fake_block_arround(elif_body);
            }
            self.visit_expression(elif_condition);
            self.visit_statement(elif_body);
        }

        if let Some((_token, else_body)) = else_ {
            if !matches!(**else_body, Statement::Block { .. }) {
                self.errors.push(FleetError::from_node(
                    &**else_body,
                    "Else statements must always have a block as the body.",
                    ErrorSeverity::Error,
                ));
                self.errors.push(FleetError::from_node(
                    &**else_body,
                    "Formatting will fix this",
                    ErrorSeverity::Note,
                ));

                **else_body = self.create_fake_block_arround(&mut *else_body);
            }

            self.visit_statement(else_body);
        }
    }

    fn partial_visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        }: &mut WhileLoopStatement,
    ) {
        if !matches!(**body, Statement::Block { .. }) {
            self.errors.push(FleetError::from_node(
                &**body,
                "While loops must always have a block as the body.",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                &**body,
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            **body = self.create_fake_block_arround(&mut *body);
        }

        self.visit_expression(condition);
        self.visit_statement(body);
    }

    fn partial_visit_for_loop_statement(
        &mut self,
        ForLoopStatement {
            for_token: _,
            open_paren_token: _,
            initializer,
            condition,
            second_semicolon_token: _,
            incrementer,
            close_paren_token: _,
            body,
            id: _,
        }: &mut ForLoopStatement,
    ) {
        if !matches!(**body, Statement::Block { .. }) {
            self.errors.push(FleetError::from_node(
                &**body,
                "For loops must always have a block as the body.",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                &**body,
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            **body = self.create_fake_block_arround(&mut *body);
        }

        self.visit_statement(initializer);
        if let Some(cond) = condition {
            self.visit_expression(cond);
        }
        if let Some(inc) = incrementer {
            self.visit_expression(inc);
        }
        self.visit_statement(body);
    }
}
