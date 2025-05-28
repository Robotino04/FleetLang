use crate::ast::{
    AstVisitor, BlockStatement, Executor, ExecutorHost, Expression, FunctionDefinition,
    IfStatement, OnStatement, Program, ReturnStatement, SelfExecutorHost, Statement,
    ThreadExecutor, Type, VariableDefinitionStatement,
};

pub trait PartialAstVisitor {
    fn partial_visit_program(mut self, program: &mut Program)
    where
        Self: Sized,
    {
        for f in &mut program.functions {
            self.partial_visit_function_definition(f);
        }
    }

    // statements
    fn partial_visit_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Expression(expression_statement) => {
                self.partial_visit_expression_statement(expression_statement)
            }
            Statement::On(on_statement) => self.partial_visit_on_statement(on_statement),
            Statement::Block(block_statement) => {
                self.partial_visit_block_statement(block_statement)
            }
            Statement::Return(return_statement) => {
                self.partial_visit_return_statement(return_statement)
            }
            Statement::VariableDefinition(variable_definition_statement) => {
                self.partial_visit_variable_definition_statement(variable_definition_statement)
            }
            Statement::If(if_statement) => self.partial_visit_if_statement(if_statement),
        }
    }
    fn partial_visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.partial_visit_statement(&mut function_definition.body);
    }

    fn partial_visit_expression_statement(
        &mut self,
        expr_stmt: &mut crate::ast::ExpressionStatement,
    ) {
        self.partial_visit_expression(&mut expr_stmt.expression);
    }

    fn partial_visit_on_statement(
        &mut self,
        OnStatement {
            on_token: _,
            open_paren_token: _,
            executor,
            close_paren_token: _,
            body,
            id: _,
        }: &mut OnStatement,
    ) {
        self.partial_visit_executor(executor);
        self.partial_visit_statement(body);
    }

    fn partial_visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        }: &mut BlockStatement,
    ) {
        for stmt in body {
            self.partial_visit_statement(stmt);
        }
    }

    fn partial_visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) {
        self.partial_visit_expression(&mut return_stmt.value);
    }

    fn partial_visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) {
        self.partial_visit_type(&mut vardef_stmt.type_);
        self.partial_visit_expression(&mut vardef_stmt.value);
    }

    fn partial_visit_if_statement(&mut self, if_stmt: &mut IfStatement) {
        self.partial_visit_expression(&mut if_stmt.condition);
        self.partial_visit_statement(&mut if_stmt.if_body);

        for (_token, condition, body) in &mut if_stmt.elifs {
            self.partial_visit_expression(condition);
            self.partial_visit_statement(&mut *body);
        }

        if let Some((_, else_body)) = &mut if_stmt.else_ {
            self.partial_visit_statement(&mut *else_body);
        }
    }

    // executor hosts
    fn partial_visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_(self_executor_host) => {
                self.partial_visit_self_executor_host(self_executor_host)
            }
        }
    }
    fn partial_visit_self_executor_host(&mut self, _executor_host: &mut SelfExecutorHost) {}

    fn partial_visit_executor(&mut self, executor: &mut Executor) {
        match executor {
            Executor::Thread(thread_executor) => {
                self.partial_visit_thread_executor(thread_executor)
            }
        }
    }
    fn partial_visit_thread_executor(&mut self, executor: &mut ThreadExecutor) {
        self.partial_visit_executor_host(&mut executor.host);
        self.partial_visit_expression(&mut executor.index);
    }

    fn partial_visit_expression(&mut self, expression: &mut Expression) {
        match expression {
            Expression::Number {
                value: _,
                token: _,
                id: _,
            } => {}
            Expression::VariableAccess {
                name: _,
                name_token: _,
                id: _,
            } => {}
            Expression::FunctionCall {
                name: _,
                name_token: _,
                open_paren_token: _,
                arguments,
                close_paren_token: _,
                id: _,
            } => {
                for arg in arguments {
                    self.partial_visit_expression(arg);
                }
            }
            Expression::Grouping {
                open_paren_token: _,
                subexpression,
                close_paren_token: _,
                id: _,
            } => {
                self.partial_visit_expression(&mut *subexpression);
            }
            Expression::Unary {
                operator_token: _,
                operation: _,
                operand,
                id: _,
            } => {
                self.partial_visit_expression(&mut *operand);
            }
            Expression::Binary {
                left,
                operator_token: _,
                operation: _,
                right,
                id: _,
            } => {
                self.partial_visit_expression(&mut *left);
                self.partial_visit_expression(&mut *right);
            }
            Expression::VariableAssignment {
                name: _,
                name_token: _,
                equal_token: _,
                right,
                id: _,
            } => {
                self.partial_visit_expression(&mut *right);
            }
        }
    }

    fn partial_visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { .. } => {}
        }
    }
}

impl<T> AstVisitor for T
where
    T: PartialAstVisitor,
{
    type SubOutput = ();
    type Output = ();

    fn visit_program(self, program: &mut Program) -> Self::Output {
        self.partial_visit_program(program);
    }

    fn visit_function_definition(
        &mut self,
        function_definition: &mut FunctionDefinition,
    ) -> Self::SubOutput {
        self.partial_visit_function_definition(function_definition);
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut crate::ast::ExpressionStatement,
    ) -> Self::SubOutput {
        self.partial_visit_expression_statement(expr_stmt);
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::SubOutput {
        self.partial_visit_on_statement(on_stmt);
    }

    fn visit_block_statement(&mut self, block: &mut BlockStatement) -> Self::SubOutput {
        self.partial_visit_block_statement(block);
    }

    fn visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) -> Self::SubOutput {
        self.partial_visit_return_statement(return_stmt);
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::SubOutput {
        self.partial_visit_variable_definition_statement(vardef_stmt);
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::SubOutput {
        self.partial_visit_if_statement(if_stmt);
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::SubOutput {
        self.partial_visit_self_executor_host(executor_host);
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::SubOutput {
        self.partial_visit_thread_executor(executor);
    }

    fn visit_expression(&mut self, expression: &mut Expression) -> Self::SubOutput {
        self.partial_visit_expression(expression);
    }

    fn visit_type(&mut self, type_: &mut Type) -> Self::SubOutput {
        self.partial_visit_type(type_);
    }
}
