use crate::{
    ast::{
        AstVisitor, BinaryExpression, BlockStatement, BreakStatement, ExpressionStatement,
        ForLoopStatement, FunctionCallExpression, FunctionDefinition, GroupingExpression, I32Type,
        IfStatement, NumberExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost,
        SkipStatement, ThreadExecutor, UnaryExpression, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, WhileLoopStatement,
    },
    tokenizer::{Token, Trivia},
};

pub struct AddLeadingTriviaPass {
    new_trivia: Vec<Trivia>,
}

impl AddLeadingTriviaPass {
    pub fn new(new_trivia: Vec<Trivia>) -> Self {
        Self { new_trivia }
    }

    fn add_leading_trivia_to_token(&mut self, token: &mut Token) {
        self.new_trivia.extend(token.leading_trivia.clone());
        token.leading_trivia = self.new_trivia.clone();
        self.new_trivia.clear();
    }
}

impl AstVisitor for AddLeadingTriviaPass {
    type ProgramOutput = ();
    type FunctionDefinitionOutput = ();
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = ();
    type TypeOutput = ();

    fn visit_program(mut self, program: &mut Program) {
        program.functions.first_mut().map(|f| {
            self.visit_function_definition(f);
        });
    }

    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.add_leading_trivia_to_token(&mut function_definition.name_token);
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement { expression, .. }: &mut ExpressionStatement,
    ) {
        self.visit_expression(expression);
    }

    fn visit_on_statement(&mut self, OnStatement { on_token, .. }: &mut OnStatement) {
        self.add_leading_trivia_to_token(on_token);
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token, ..
        }: &mut BlockStatement,
    ) {
        self.add_leading_trivia_to_token(open_brace_token);
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement { return_token, .. }: &mut ReturnStatement,
    ) {
        self.add_leading_trivia_to_token(return_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement { let_token, .. }: &mut VariableDefinitionStatement,
    ) {
        self.add_leading_trivia_to_token(let_token);
    }

    fn visit_if_statement(&mut self, IfStatement { if_token, .. }: &mut IfStatement) {
        self.add_leading_trivia_to_token(if_token);
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.add_leading_trivia_to_token(&mut while_stmt.while_token);
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.add_leading_trivia_to_token(&mut for_stmt.for_token);
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        self.add_leading_trivia_to_token(&mut break_stmt.break_token);
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        self.add_leading_trivia_to_token(&mut skip_stmt.skip_token);
    }

    fn visit_self_executor_host(&mut self, executor_host: &mut SelfExecutorHost) {
        self.add_leading_trivia_to_token(&mut executor_host.token);
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) {
        self.visit_executor_host(&mut executor.host);
    }

    fn visit_number_expression(&mut self, expression: &mut NumberExpression) {
        self.add_leading_trivia_to_token(&mut expression.token);
    }

    fn visit_function_call_expression(&mut self, expression: &mut FunctionCallExpression) {
        self.add_leading_trivia_to_token(&mut expression.name_token);
    }

    fn visit_grouping_expression(&mut self, expression: &mut GroupingExpression) {
        self.add_leading_trivia_to_token(&mut expression.open_paren_token);
    }

    fn visit_variable_access_expression(&mut self, expression: &mut VariableAccessExpression) {
        self.add_leading_trivia_to_token(&mut expression.name_token);
    }

    fn visit_unary_expression(&mut self, expression: &mut UnaryExpression) {
        self.add_leading_trivia_to_token(&mut expression.operator_token);
    }

    fn visit_binary_expression(&mut self, expression: &mut BinaryExpression) {
        self.visit_expression(&mut expression.left);
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) {
        self.add_leading_trivia_to_token(&mut expression.name_token);
    }

    fn visit_i32_type(&mut self, i32_type: &mut I32Type) {
        self.add_leading_trivia_to_token(&mut i32_type.token);
    }
}
