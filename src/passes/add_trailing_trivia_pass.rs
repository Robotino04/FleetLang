use crate::{
    ast::{
        AstVisitor, BinaryExpression, BlockStatement, BoolExpression, BoolType, BreakStatement,
        CastExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionBody,
        FunctionCallExpression, FunctionDefinition, GroupingExpression, IdkType, IfStatement,
        IntType, NumberExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost,
        SimpleBinding, SkipStatement, StatementFunctionBody, ThreadExecutor, UnaryExpression,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, WhileLoopStatement,
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

    pub fn visit_token(&mut self, token: &mut Token) {
        token.trailing_trivia.extend(self.new_trivia.clone());
        self.new_trivia.clear();
    }
}

impl AstVisitor for AddTrailingTriviaPass {
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
        program.functions.last_mut().map(|f| {
            self.visit_function_definition(f);
        });
    }
    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        match &mut function_definition.body {
            FunctionBody::Statement(statement_function_body) => {
                self.visit_statement_function_body(statement_function_body)
            }
            FunctionBody::Extern(extern_function_body) => {
                self.visit_extern_function_body(extern_function_body)
            }
        }
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, .. }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_statement(statement);
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            semicolon_token, ..
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_token(semicolon_token);
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding {
            type_, name_token, ..
        }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        if let Some((_colon, type_)) = type_ {
            self.visit_type(type_);
        } else {
            self.visit_token(name_token);
        }
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            semicolon_token, ..
        }: &mut ExpressionStatement,
    ) {
        self.visit_token(semicolon_token);
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
        self.visit_token(close_brace_token);
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement {
            semicolon_token, ..
        }: &mut ReturnStatement,
    ) {
        self.visit_token(semicolon_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            semicolon_token, ..
        }: &mut VariableDefinitionStatement,
    ) {
        self.visit_token(semicolon_token);
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

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.visit_statement(&mut while_stmt.body);
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.visit_statement(&mut for_stmt.body);
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
        self.visit_token(&mut executor.close_bracket_token);
    }

    fn visit_number_expression(&mut self, expression: &mut NumberExpression) {
        self.visit_token(&mut expression.token);
    }

    fn visit_bool_expression(&mut self, expression: &mut BoolExpression) -> Self::ExpressionOutput {
        self.visit_token(&mut expression.token);
    }

    fn visit_function_call_expression(&mut self, expression: &mut FunctionCallExpression) {
        self.visit_token(&mut expression.close_paren_token);
    }

    fn visit_grouping_expression(&mut self, expression: &mut GroupingExpression) {
        self.visit_token(&mut expression.close_paren_token);
    }

    fn visit_variable_access_expression(&mut self, expression: &mut VariableAccessExpression) {
        self.visit_token(&mut expression.name_token);
    }

    fn visit_unary_expression(&mut self, expression: &mut UnaryExpression) {
        self.visit_expression(&mut expression.operand);
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        self.visit_type(&mut expression.type_);
    }

    fn visit_binary_expression(&mut self, expression: &mut BinaryExpression) {
        self.visit_expression(&mut expression.right);
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) {
        self.visit_expression(&mut expression.right);
    }

    fn visit_int_type(&mut self, int_type: &mut IntType) {
        self.visit_token(&mut int_type.token);
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        self.visit_token(&mut unit_type.close_paren_token);
    }

    fn visit_bool_type(&mut self, bool_type: &mut BoolType) -> Self::TypeOutput {
        self.visit_token(&mut bool_type.token);
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.visit_token(&mut idk_type.token);
    }
}
