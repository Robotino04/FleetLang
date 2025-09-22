use either::Either;

use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor, BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression, FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement, LiteralExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody, StructAccessExpression, StructAccessLValue, StructExpression, StructType, ThreadExecutor, TypeAlias, UnaryExpression, UnitType, VariableAccessExpression, VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue, WhileLoopStatement
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

    fn visit_type_alias(&mut self, type_alias: &mut TypeAlias) -> Self::TopLevelOutput {
        self.visit_token(&mut type_alias.let_token);
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

    fn visit_struct_access_expression(
        &mut self,
        expression: &mut StructAccessExpression,
    ) -> Self::ExpressionOutput {
        self.visit_expression(&mut expression.value)
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

    fn visit_struct_access_lvalue(
        &mut self,
        lvalue: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        self.visit_lvalue(&mut lvalue.value)
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

    fn visit_alias_type(&mut self, alias_type: &mut AliasType) -> Self::TypeOutput {
        self.visit_token(&mut alias_type.name_token);
    }

    fn visit_top_level_statement(
        &mut self,
        tls: &mut crate::ast::TopLevelStatement,
    ) -> Self::TopLevelOutput {
        match tls {
            crate::ast::TopLevelStatement::Function(function_definition) => {
                self.visit_function_definition(function_definition)
            }
            crate::ast::TopLevelStatement::TypeAlias(type_alias) => {
                self.visit_type_alias(type_alias)
            }
        }
    }

    fn visit_function_body(
        &mut self,
        function_body: &mut crate::ast::FunctionBody,
    ) -> Self::FunctionBodyOutput {
        match function_body {
            crate::ast::FunctionBody::Statement(statement_function_body) => {
                self.visit_statement_function_body(statement_function_body)
            }
            crate::ast::FunctionBody::Extern(extern_function_body) => {
                self.visit_extern_function_body(extern_function_body)
            }
        }
    }

    fn visit_statement(&mut self, statement: &mut crate::ast::Statement) -> Self::StatementOutput {
        match statement {
            crate::ast::Statement::Expression(expression_statement) => {
                self.visit_expression_statement(expression_statement)
            }
            crate::ast::Statement::On(on_statement) => self.visit_on_statement(on_statement),
            crate::ast::Statement::Block(block_statement) => {
                self.visit_block_statement(block_statement)
            }
            crate::ast::Statement::Return(return_statement) => {
                self.visit_return_statement(return_statement)
            }
            crate::ast::Statement::VariableDefinition(variable_definition_statement) => {
                self.visit_variable_definition_statement(variable_definition_statement)
            }
            crate::ast::Statement::If(if_statement) => self.visit_if_statement(if_statement),
            crate::ast::Statement::WhileLoop(while_loop_statement) => {
                self.visit_while_loop_statement(while_loop_statement)
            }
            crate::ast::Statement::ForLoop(for_loop_statement) => {
                self.visit_for_loop_statement(for_loop_statement)
            }
            crate::ast::Statement::Break(break_statement) => {
                self.visit_break_statement(break_statement)
            }
            crate::ast::Statement::Skip(skip_statement) => {
                self.visit_skip_statement(skip_statement)
            }
        }
    }

    fn visit_executor_host(
        &mut self,
        executor_host: &mut crate::ast::ExecutorHost,
    ) -> Self::ExecutorHostOutput {
        match executor_host {
            crate::ast::ExecutorHost::Self_(self_executor_host) => {
                self.visit_self_executor_host(self_executor_host)
            }
        }
    }

    fn visit_executor(&mut self, executor: &mut crate::ast::Executor) -> Self::ExecutorOutput {
        match executor {
            crate::ast::Executor::Thread(thread_executor) => {
                self.visit_thread_executor(thread_executor)
            }
            crate::ast::Executor::GPU(gpu_executor) => self.visit_gpu_executor(gpu_executor),
        }
    }

    fn visit_expression(
        &mut self,
        expression: &mut crate::ast::Expression,
    ) -> Self::ExpressionOutput {
        match expression {
            crate::ast::Expression::Literal(literal_expression) => {
                self.visit_literal_expression(literal_expression)
            }
            crate::ast::Expression::Array(array_expression) => {
                self.visit_array_expression(array_expression)
            }
            crate::ast::Expression::Struct(struct_expression) => {
                self.visit_struct_expression(struct_expression)
            }
            crate::ast::Expression::FunctionCall(function_call_expression) => {
                self.visit_function_call_expression(function_call_expression)
            }
            crate::ast::Expression::CompilerExpression(compiler_expression) => {
                self.visit_compiler_expression(compiler_expression)
            }
            crate::ast::Expression::ArrayIndex(array_index_expression) => {
                self.visit_array_index_expression(array_index_expression)
            }
            crate::ast::Expression::StructAccess(struct_access_expression) => {
                self.visit_struct_access_expression(struct_access_expression)
            }
            crate::ast::Expression::Grouping(grouping_expression) => {
                self.visit_grouping_expression(grouping_expression)
            }
            crate::ast::Expression::VariableAccess(variable_access_expression) => {
                self.visit_variable_access_expression(variable_access_expression)
            }
            crate::ast::Expression::Cast(cast_expression) => {
                self.visit_cast_expression(cast_expression)
            }
            crate::ast::Expression::Unary(unary_expression) => {
                self.visit_unary_expression(unary_expression)
            }
            crate::ast::Expression::Binary(binary_expression) => {
                self.visit_binary_expression(binary_expression)
            }
            crate::ast::Expression::VariableAssignment(variable_assignment_expression) => {
                self.visit_variable_assignment_expression(variable_assignment_expression)
            }
        }
    }

    fn visit_lvalue(&mut self, lvalue: &mut crate::ast::LValue) -> Self::LValueOutput {
        match lvalue {
            crate::ast::LValue::Variable(var_lvalue) => self.visit_variable_lvalue(var_lvalue),
            crate::ast::LValue::ArrayIndex(array_index_lvalue) => {
                self.visit_array_index_lvalue(array_index_lvalue)
            }
            crate::ast::LValue::StructAccess(struct_access_lvalue) => {
                self.visit_struct_access_lvalue(struct_access_lvalue)
            }
            crate::ast::LValue::Grouping(grouping_lvalue) => {
                self.visit_grouping_lvalue(grouping_lvalue)
            }
        }
    }

    fn visit_type(&mut self, type_: &mut crate::ast::Type) -> Self::TypeOutput {
        match type_ {
            crate::ast::Type::Simple(simple_type) => self.visit_simple_type(simple_type),
            crate::ast::Type::Unit(unit_type) => self.visit_unit_type(unit_type),
            crate::ast::Type::Idk(idk_type) => self.visit_idk_type(idk_type),
            crate::ast::Type::Array(array_type) => self.visit_array_type(array_type),
            crate::ast::Type::Struct(struct_type) => self.visit_struct_type(struct_type),
            crate::ast::Type::Alias(alias_type) => self.visit_alias_type(alias_type),
        }
    }
}
