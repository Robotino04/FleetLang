use crate::{
    ast::{
        AstVisitor, BlockStatement, ForLoopStatement, FunctionDefinition, IfStatement, Statement,
        WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    parser::IdGenerator,
    tokenizer::{Token, TokenType},
};

use super::{find_node_bonds::find_node_bounds, partial_visitor::PartialAstVisitor};

pub struct FixNonBlockStatements<'a> {
    errors: &'a mut Vec<FleetError>,
    id_generator: &'a mut IdGenerator,
}

impl<'a> FixNonBlockStatements<'a> {
    pub fn new(errors: &'a mut Vec<FleetError>, id_generator: &'a mut IdGenerator) -> Self {
        Self {
            errors,
            id_generator,
        }
    }

    fn create_fake_block_arround(&mut self, node: &Statement) -> Statement {
        let (start, end) = find_node_bounds(node.clone());

        return Statement::Block(BlockStatement {
            open_brace_token: Token {
                type_: TokenType::OpenBrace,
                start,
                end: start,
                leading_trivia: vec![],
                trailing_trivia: vec![],
            },
            body: vec![node.clone()],
            close_brace_token: Token {
                type_: TokenType::CloseBrace,
                start: end,
                end,
                leading_trivia: vec![],
                trailing_trivia: vec![],
            },
            id: self.id_generator.next_id(),
        });
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
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) {
        if !matches!(body, Statement::Block { .. }) {
            self.errors.push(FleetError::from_node(
                body.clone(),
                "Functions must have a block as the body",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                body.clone(),
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            *body = self.create_fake_block_arround(body);
        }
        self.visit_statement(body);
        self.visit_type(return_type);
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
                *if_body.clone(),
                "If statements must always have a block as the body.",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                *if_body.clone(),
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            **if_body = self.create_fake_block_arround(&**if_body);
        }

        self.visit_expression(condition);
        self.visit_statement(if_body);

        for (_token, elif_condition, elif_body) in elifs {
            if !matches!(elif_body, Statement::Block { .. }) {
                self.errors.push(FleetError::from_node(
                    elif_body.clone(),
                    "Elif statements must always have a block as the body.",
                    ErrorSeverity::Error,
                ));
                self.errors.push(FleetError::from_node(
                    elif_body.clone(),
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
                    *else_body.clone(),
                    "Else statements must always have a block as the body.",
                    ErrorSeverity::Error,
                ));
                self.errors.push(FleetError::from_node(
                    *else_body.clone(),
                    "Formatting will fix this",
                    ErrorSeverity::Note,
                ));

                **else_body = self.create_fake_block_arround(&**else_body);
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
                *body.clone(),
                "While loops must always have a block as the body.",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                *body.clone(),
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            **body = self.create_fake_block_arround(&**body);
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
                *body.clone(),
                "For loops must always have a block as the body.",
                ErrorSeverity::Error,
            ));
            self.errors.push(FleetError::from_node(
                *body.clone(),
                "Formatting will fix this",
                ErrorSeverity::Note,
            ));

            **body = self.create_fake_block_arround(&**body);
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
