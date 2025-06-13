use crate::{
    ast::{
        AstVisitor, BinaryExpression, BlockStatement, BoolExpression, BoolType, BreakStatement,
        CastExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionCallExpression, FunctionDefinition, GroupingExpression, IdkType, IfStatement,
        IntType, NumberExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost,
        SimpleBinding, SkipStatement, StatementFunctionBody, ThreadExecutor, UnaryExpression,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, WhileLoopStatement,
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

    pub fn visit_token(&mut self, token: &mut Token) {
        self.new_trivia.extend(token.leading_trivia.clone());
        token.leading_trivia = self.new_trivia.clone();
        self.new_trivia.clear();
    }
}

impl AstVisitor for AddLeadingTriviaPass {
    type ProgramOutput = ();
    type FunctionDefinitionOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = ();
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
        self.visit_token(&mut function_definition.let_token);
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, .. }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_statement(statement);
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody { at_token, .. }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_token(at_token);
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding { name_token, .. }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        self.visit_token(name_token);
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement { expression, .. }: &mut ExpressionStatement,
    ) {
        self.visit_expression(expression);
    }

    fn visit_on_statement(&mut self, OnStatement { on_token, .. }: &mut OnStatement) {
        self.visit_token(on_token);
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token, ..
        }: &mut BlockStatement,
    ) {
        self.visit_token(open_brace_token);
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement { return_token, .. }: &mut ReturnStatement,
    ) {
        self.visit_token(return_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement { binding, .. }: &mut VariableDefinitionStatement,
    ) {
        self.visit_simple_binding(binding);
    }

    fn visit_if_statement(&mut self, IfStatement { if_token, .. }: &mut IfStatement) {
        self.visit_token(if_token);
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.visit_token(&mut while_stmt.while_token);
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.visit_token(&mut for_stmt.for_token);
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        self.visit_token(&mut break_stmt.break_token);
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        self.visit_token(&mut skip_stmt.skip_token);
    }

    fn visit_self_executor_host(&mut self, executor_host: &mut SelfExecutorHost) {
        self.visit_token(&mut executor_host.token);
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) {
        self.visit_executor_host(&mut executor.host);
    }

    fn visit_number_expression(&mut self, expression: &mut NumberExpression) {
        self.visit_token(&mut expression.token);
    }

    fn visit_bool_expression(&mut self, expression: &mut BoolExpression) -> Self::ExpressionOutput {
        self.visit_token(&mut expression.token);
    }

    fn visit_function_call_expression(&mut self, expression: &mut FunctionCallExpression) {
        self.visit_token(&mut expression.name_token);
    }

    fn visit_grouping_expression(&mut self, expression: &mut GroupingExpression) {
        self.visit_token(&mut expression.open_paren_token);
    }

    fn visit_variable_access_expression(&mut self, expression: &mut VariableAccessExpression) {
        self.visit_token(&mut expression.name_token);
    }

    fn visit_unary_expression(&mut self, expression: &mut UnaryExpression) {
        self.visit_token(&mut expression.operator_token);
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        self.visit_expression(&mut expression.operand);
    }

    fn visit_binary_expression(&mut self, expression: &mut BinaryExpression) {
        self.visit_expression(&mut expression.left);
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) {
        self.visit_token(&mut expression.name_token);
    }

    fn visit_int_type(&mut self, int_type: &mut IntType) {
        self.visit_token(&mut int_type.token);
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        self.visit_token(&mut unit_type.open_paren_token);
    }

    fn visit_bool_type(&mut self, bool_type: &mut BoolType) -> Self::TypeOutput {
        self.visit_token(&mut bool_type.token);
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.visit_token(&mut idk_type.token);
    }
}
