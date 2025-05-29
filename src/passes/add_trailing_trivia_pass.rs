use crate::{
    ast::{
        AstVisitor, BlockStatement, ExpressionStatement, FunctionDefinition, IfStatement,
        OnStatement, Program, ReturnStatement, SelfExecutorHost, ThreadExecutor, Type,
        VariableDefinitionStatement,
    },
    tokenizer::{Token, Trivia},
};

pub struct AddTrailingTriviaPass {
    new_trivia: Vec<Trivia>,
}

impl AddTrailingTriviaPass {
    pub fn new(new_trivia: Vec<Trivia>) -> Self {
        Self { new_trivia }
    }

    fn add_trailing_trivia_to_token(&mut self, token: &mut Token) {
        token.trailing_trivia.extend(self.new_trivia.clone());
        self.new_trivia.clear();
    }
}

impl AstVisitor for AddTrailingTriviaPass {
    type ProgramOutput = ();
    type FunctionDefinitionOutput = ();
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = ();
    type TypeOutput = ();

    fn visit_program(mut self, program: &mut Program) {
        if let Some(f) = program.functions.last_mut() {
            self.visit_function_definition(f);
        }
    }

    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.visit_statement(&mut function_definition.body);
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            semicolon_token, ..
        }: &mut ExpressionStatement,
    ) {
        self.add_trailing_trivia_to_token(semicolon_token);
    }

    fn visit_on_statement(&mut self, OnStatement { body, .. }: &mut OnStatement) {
        self.visit_statement(body);
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            close_brace_token, ..
        }: &mut BlockStatement,
    ) {
        self.add_trailing_trivia_to_token(close_brace_token);
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement {
            semicolon_token, ..
        }: &mut ReturnStatement,
    ) {
        self.add_trailing_trivia_to_token(semicolon_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            semicolon_token, ..
        }: &mut VariableDefinitionStatement,
    ) {
        self.add_trailing_trivia_to_token(semicolon_token);
    }

    fn visit_if_statement(
        &mut self,
        IfStatement {
            if_body,
            elifs,
            else_,
            ..
        }: &mut IfStatement,
    ) {
        if let Some((_, else_body)) = else_ {
            self.visit_statement(else_body);
        } else if let Some((_, _, body)) = elifs.last_mut() {
            self.visit_statement(body);
        } else {
            self.visit_statement(if_body);
        }
    }

    fn visit_self_executor_host(&mut self, executor_host: &mut SelfExecutorHost) {
        self.add_trailing_trivia_to_token(&mut executor_host.token);
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) {
        self.add_trailing_trivia_to_token(&mut executor.close_bracket_token);
    }

    fn visit_number_expression(&mut self, expression: &mut crate::ast::NumberExpression) {
        self.add_trailing_trivia_to_token(&mut expression.token);
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut crate::ast::FunctionCallExpression,
    ) {
        self.add_trailing_trivia_to_token(&mut expression.close_paren_token);
    }

    fn visit_grouping_expression(&mut self, expression: &mut crate::ast::GroupingExpression) {
        self.add_trailing_trivia_to_token(&mut expression.close_paren_token);
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut crate::ast::VariableAccessExpression,
    ) {
        self.add_trailing_trivia_to_token(&mut expression.name_token);
    }

    fn visit_unary_expression(&mut self, expression: &mut crate::ast::UnaryExpression) {
        self.visit_expression(&mut expression.operand);
    }

    fn visit_binary_expression(&mut self, expression: &mut crate::ast::BinaryExpression) {
        self.visit_expression(&mut expression.right);
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut crate::ast::VariableAssignmentExpression,
    ) {
        self.visit_expression(&mut expression.right);
    }

    fn visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { token, id: _ } => {
                self.add_trailing_trivia_to_token(token);
            }
        }
    }
}
