use crate::{
    ast::{
        AstNode, AstVisitor, Executor, ExecutorHost, Expression, FunctionDefinition, Program,
        Statement, Type,
    },
    tokenizer::{SourceLocation, Token},
};

pub struct FindContainingNodePass {
    pub node_hierarchy: Vec<AstNode>,
    pub token: Option<Token>,
    search_position: SourceLocation,
}

impl FindContainingNodePass {
    pub fn new(search_position: SourceLocation) -> Self {
        Self {
            node_hierarchy: vec![],
            token: None,
            search_position,
        }
    }

    fn visit_token(&mut self, token: &Token) -> Result<(), ()> {
        if (token.start.line..=token.end.line).contains(&self.search_position.line)
            && (token.start.column..token.end.column).contains(&self.search_position.column)
        {
            self.token = Some(token.clone());
            return Err(());
        }
        return Ok(());
    }
}

impl AstVisitor for FindContainingNodePass {
    type Output = Result<(), ()>;

    fn visit_program(&mut self, program: &mut Program) -> Self::Output {
        self.node_hierarchy.push(program.clone().into());

        for f in &mut program.functions {
            if let Err(()) = self.visit_function_definition(f) {
                return Ok(());
            }
        }
        return Err(());
    }

    fn visit_function_definition(&mut self, function: &mut FunctionDefinition) -> Self::Output {
        self.node_hierarchy.push(function.clone().into());

        let FunctionDefinition {
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
        } = function;

        self.visit_statement(body)?;
        self.visit_token(let_token)?;
        self.visit_token(name_token)?;
        self.visit_token(equal_token)?;
        self.visit_token(open_paren_token)?;
        self.visit_token(close_paren_token)?;
        self.visit_token(right_arrow_token)?;
        self.visit_type(return_type)?;
        self.visit_statement(body)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_statement(&mut self, statement: &mut Statement) -> Self::Output {
        self.node_hierarchy.push(statement.clone().into());

        match statement {
            Statement::Expression {
                expression,
                semicolon_token,
                id: _,
            } => {
                self.visit_expression(expression)?;
                self.visit_token(semicolon_token)?;
            }
            Statement::On {
                on_token,
                open_paren_token,
                executor,
                close_paren_token,
                body,
                id: _,
            } => {
                self.visit_token(on_token)?;
                self.visit_token(open_paren_token)?;
                self.visit_executor(executor)?;
                self.visit_token(close_paren_token)?;
                self.visit_statement(body)?;
            }
            Statement::Block {
                open_brace_token,
                body,
                close_brace_token,
                id: _,
            } => {
                self.visit_token(open_brace_token)?;
                for stmt in body {
                    self.visit_statement(stmt)?;
                }
                self.visit_token(close_brace_token)?;
            }
            Statement::Return {
                return_token,
                value,
                semicolon_token,
                id: _,
            } => {
                self.visit_token(return_token)?;
                self.visit_expression(value)?;
                self.visit_token(semicolon_token)?;
            }
            Statement::VariableDefinition {
                let_token,
                name_token,
                name: _,
                colon_token,
                type_,
                equals_token,
                value,
                semicolon_token,
                id: _,
            } => {
                self.visit_token(let_token)?;
                self.visit_token(name_token)?;
                self.visit_token(colon_token)?;
                self.visit_type(type_)?;
                self.visit_token(equals_token)?;
                self.visit_expression(value)?;
                self.visit_token(semicolon_token)?;
            }
            Statement::If {
                if_token,
                condition,
                if_body,
                elifs,
                else_,
                id: _,
            } => {
                self.visit_token(if_token)?;
                self.visit_expression(condition)?;
                self.visit_statement(if_body)?;
                for (elif_token, elif_condition, elif_body) in elifs {
                    self.visit_token(elif_token)?;
                    self.visit_expression(elif_condition)?;
                    self.visit_statement(elif_body)?;
                }
                if let Some((else_token, else_body)) = else_ {
                    self.visit_token(else_token)?;
                    self.visit_statement(else_body)?;
                }
            }
        }

        self.node_hierarchy.pop();

        return Ok(());
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) -> Self::Output {
        self.node_hierarchy.push(executor_host.clone().into());

        match executor_host {
            ExecutorHost::Self_ { token, id: _ } => {
                self.visit_token(token)?;
            }
        }
        self.node_hierarchy.pop();

        return Ok(());
    }

    fn visit_executor(&mut self, executor: &mut Executor) -> Self::Output {
        self.node_hierarchy.push(executor.clone().into());

        match executor {
            Executor::Thread {
                host,
                dot_token,
                thread_token,
                open_bracket_token,
                index,
                close_bracket_token,
                id: _,
            } => {
                self.visit_executor_host(host)?;
                self.visit_token(dot_token)?;
                self.visit_token(thread_token)?;
                self.visit_token(open_bracket_token)?;
                self.visit_expression(index)?;
                self.visit_token(close_bracket_token)?;
            }
        }

        self.node_hierarchy.pop();

        return Ok(());
    }

    fn visit_expression(&mut self, expression: &mut Expression) -> Self::Output {
        self.node_hierarchy.push(expression.clone().into());

        match expression {
            Expression::Number {
                value: _,
                token,
                id: _,
            } => {
                self.visit_token(token)?;
            }
            Expression::VariableAccess {
                name: _,
                name_token,
                id: _,
            } => {
                self.visit_token(name_token)?;
            }
            Expression::FunctionCall {
                name: _,
                name_token,
                open_paren_token,
                arguments,
                close_paren_token,
                id: _,
            } => {
                self.visit_token(name_token)?;
                self.visit_token(open_paren_token)?;
                for arg in arguments {
                    self.visit_expression(arg)?;
                }
                self.visit_token(close_paren_token)?;
            }
            Expression::Grouping {
                open_paren_token,
                subexpression,
                close_paren_token,
                id: _,
            } => {
                self.visit_token(open_paren_token)?;
                self.visit_expression(subexpression)?;
                self.visit_token(close_paren_token)?;
            }
            Expression::Unary {
                operator_token,
                operation: _,
                operand,
                id: _,
            } => {
                self.visit_token(operator_token)?;
                self.visit_expression(operand)?;
            }
            Expression::Binary {
                left,
                operator_token,
                operation: _,
                right,
                id: _,
            } => {
                self.visit_expression(&mut *left)?;
                self.visit_token(operator_token)?;
                self.visit_expression(&mut *right)?;
            }
            Expression::VariableAssignment {
                name: _,
                name_token,
                equal_token,
                right,
                id: _,
            } => {
                self.visit_token(name_token)?;
                self.visit_token(&equal_token)?;
                self.visit_expression(&mut *right)?;
            }
        }

        self.node_hierarchy.pop();

        return Ok(());
    }

    fn visit_type(&mut self, type_: &mut Type) -> Self::Output {
        self.node_hierarchy.push(type_.clone().into());

        match type_ {
            Type::I32 { token, id: _ } => {
                self.visit_token(token)?;
            }
        }

        self.node_hierarchy.pop();

        return Ok(());
    }
}
