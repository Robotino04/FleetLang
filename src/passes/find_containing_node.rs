use crate::{
    ast::{
        AstNode, AstVisitor, BlockStatement, Expression, ExpressionStatement, FunctionDefinition,
        IfStatement, OnStatement, Program, ReturnStatement, SelfExecutorHost, ThreadExecutor, Type,
        VariableDefinitionStatement,
    },
    tokenizer::{SourceLocation, Token},
};

pub struct FindContainingNodePass {
    node_hierarchy: Vec<AstNode>,
    token: Option<Token>,
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
    type SubOutput = Result<(), ()>;
    type Output = Result<(Vec<AstNode>, Token), ()>;

    fn visit_program(mut self, program: &mut Program) -> Self::Output {
        self.node_hierarchy.push(program.clone().into());

        for f in &mut program.functions {
            if let Err(()) = self.visit_function_definition(f) {
                return Ok((self.node_hierarchy, self.token.expect("a token should have been found if visit_function_definition returns Err(())")));
            }
        }
        return Err(());
    }

    fn visit_function_definition(&mut self, function: &mut FunctionDefinition) -> Self::SubOutput {
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

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::SubOutput {
        self.node_hierarchy.push(expr_stmt.clone().into());

        self.visit_expression(&mut expr_stmt.expression)?;
        self.visit_token(&mut expr_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::SubOutput {
        self.node_hierarchy.push(on_stmt.clone().into());

        self.visit_token(&mut on_stmt.on_token)?;
        self.visit_token(&mut on_stmt.open_paren_token)?;
        self.visit_executor(&mut on_stmt.executor)?;
        self.visit_token(&mut on_stmt.close_paren_token)?;
        self.visit_statement(&mut on_stmt.body)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_block_statement(&mut self, block_stmt: &mut BlockStatement) -> Self::SubOutput {
        self.node_hierarchy.push(block_stmt.clone().into());

        self.visit_token(&mut block_stmt.open_brace_token)?;
        for stmt in &mut block_stmt.body {
            self.visit_statement(stmt)?;
        }
        self.visit_token(&mut block_stmt.close_brace_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) -> Self::SubOutput {
        self.node_hierarchy.push(return_stmt.clone().into());

        self.visit_token(&mut return_stmt.return_token)?;
        self.visit_expression(&mut return_stmt.value)?;
        self.visit_token(&mut return_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::SubOutput {
        self.node_hierarchy.push(vardef_stmt.clone().into());

        self.visit_token(&mut vardef_stmt.let_token)?;
        self.visit_token(&mut vardef_stmt.name_token)?;
        self.visit_token(&mut vardef_stmt.colon_token)?;
        self.visit_type(&mut vardef_stmt.type_)?;
        self.visit_token(&mut vardef_stmt.equals_token)?;
        self.visit_expression(&mut vardef_stmt.value)?;
        self.visit_token(&mut vardef_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::SubOutput {
        self.node_hierarchy.push(if_stmt.clone().into());

        self.visit_token(&mut if_stmt.if_token)?;
        self.visit_expression(&mut if_stmt.condition)?;
        self.visit_statement(&mut if_stmt.if_body)?;
        for (elif_token, elif_condition, elif_body) in &mut if_stmt.elifs {
            self.visit_token(elif_token)?;
            self.visit_expression(elif_condition)?;
            self.visit_statement(elif_body)?;
        }
        if let Some((else_token, else_body)) = &mut if_stmt.else_ {
            self.visit_token(else_token)?;
            self.visit_statement(else_body)?;
        }

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::SubOutput {
        self.node_hierarchy.push(executor_host.clone().into());

        self.visit_token(&executor_host.token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::SubOutput {
        self.node_hierarchy.push(executor.clone().into());

        self.visit_executor_host(&mut executor.host)?;
        self.visit_token(&mut executor.dot_token)?;
        self.visit_token(&mut executor.thread_token)?;
        self.visit_token(&mut executor.open_bracket_token)?;
        self.visit_expression(&mut executor.index)?;
        self.visit_token(&mut executor.close_bracket_token)?;

        self.node_hierarchy.pop();

        return Ok(());
    }

    fn visit_expression(&mut self, expression: &mut Expression) -> Self::SubOutput {
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

    fn visit_type(&mut self, type_: &mut Type) -> Self::SubOutput {
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
