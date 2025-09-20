use either::Either;

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LiteralExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost, SimpleBinding,
        SimpleType, SkipStatement, StatementFunctionBody, StructExpression, StructType,
        ThreadExecutor, UnaryExpression, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    tokenizer::Token,
};

type Callback<'a, R> = Box<dyn FnOnce(&mut Token) -> R + 'a>;

pub struct FirstTokenMapper<'slf, R> {
    callback: Either<Callback<'slf, R>, R>,
}

impl<'slf, R> FirstTokenMapper<'slf, R> {
    pub fn new(callback: impl FnOnce(&mut Token) -> R + 'slf) -> Self {
        Self {
            callback: Either::Left(Box::new(callback)),
        }
    }

    pub fn visit_token(&mut self, token: &mut Token) {
        let callback = std::mem::replace(
            &mut self.callback,
            Either::Left(Box::new(|_| {
                unreachable!("Callback result wasn't placed back after visiting")
            })),
        );

        self.callback = match callback {
            Either::Left(callback_fn) => {
                let result = callback_fn(token);
                Either::Right(result)
            }
            Either::Right(value) => Either::Right(value),
        };
    }

    pub fn result(self) -> Option<R> {
        self.callback.right()
    }
}

impl<R> AstVisitor for FirstTokenMapper<'_, R> {
    type ProgramOutput = Option<R>;
    type TopLevelOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = ();
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = ();
    type LValueOutput = ();
    type TypeOutput = ();

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        if let Some(tls) = program.top_level_statements.first_mut() {
            self.visit_top_level_statement(tls);
        }

        self.callback.right()
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

    fn visit_gpu_executor(&mut self, executor: &mut GPUExecutor) -> Self::ExecutorOutput {
        self.visit_executor_host(&mut executor.host);
    }

    fn visit_literal_expression(&mut self, expression: &mut LiteralExpression) {
        self.visit_token(&mut expression.token);
    }

    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        self.visit_token(&mut expression.open_bracket_token);
    }

    fn visit_struct_expression(
        &mut self,
        expression: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        self.visit_type(&mut expression.type_);
    }

    fn visit_function_call_expression(&mut self, expression: &mut FunctionCallExpression) {
        self.visit_token(&mut expression.name_token);
    }

    fn visit_compiler_expression(
        &mut self,
        expression: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        self.visit_token(&mut expression.at_token);
    }

    fn visit_array_index_expression(
        &mut self,
        expression: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        self.visit_expression(&mut expression.array);
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
        self.visit_lvalue(&mut expression.lvalue);
    }

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        self.visit_token(&mut lvalue.name_token);
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput {
        self.visit_lvalue(&mut lvalue.array);
    }

    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput {
        self.visit_token(&mut lvalue.open_paren_token);
    }

    fn visit_simple_type(&mut self, int_type: &mut SimpleType) {
        self.visit_token(&mut int_type.token);
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        self.visit_token(&mut unit_type.open_paren_token);
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.visit_token(&mut idk_type.token);
    }

    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput {
        self.visit_type(&mut array_type.subtype);
    }

    fn visit_struct_type(&mut self, struct_type: &mut StructType) -> Self::TypeOutput {
        self.visit_token(&mut struct_type.struct_token);
    }
}
