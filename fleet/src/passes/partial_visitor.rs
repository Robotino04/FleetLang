use crate::ast::{
    ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
    BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression, Executor,
    ExecutorHost, Expression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
    FunctionBody, FunctionCallExpression, FunctionDefinition, GPUExecutor, GroupingExpression,
    GroupingLValue, IdkType, IfStatement, LValue, LiteralExpression, OnStatement,
    OnStatementIterator, Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType,
    SkipStatement, Statement, StatementFunctionBody, ThreadExecutor, TopLevelStatement, Type,
    UnaryExpression, UnitType, VariableAccessExpression, VariableAssignmentExpression,
    VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
};

pub trait PartialAstVisitor {
    fn partial_visit_program(
        mut self,
        Program {
            top_level_statements,
            id: _,
            file_name: _,
        }: &mut Program,
    ) where
        Self: Sized,
    {
        for tls in top_level_statements {
            self.partial_visit_top_level_statement(tls);
        }
    }
    fn partial_visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token: _,
            name: _,
            type_,
            id: _,
        }: &mut SimpleBinding,
    ) {
        if let Some((_colon, type_)) = type_ {
            self.partial_visit_type(type_);
        }
    }

    fn partial_visit_top_level_statement(&mut self, tls: &mut TopLevelStatement) {
        match tls {
            TopLevelStatement::Function(function_definition) => {
                self.partial_visit_function_definition(function_definition)
            }
        }
    }

    fn partial_visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) {
        for (param, _comma) in parameters {
            self.partial_visit_simple_binding(param);
        }
        if let Some(return_type) = return_type {
            self.partial_visit_type(return_type);
        }
        self.partial_visit_function_body(body);
    }

    fn partial_visit_function_body(&mut self, function_body: &mut FunctionBody) {
        match function_body {
            FunctionBody::Statement(statement_function_body) => {
                self.partial_visit_statement_function_body(statement_function_body)
            }
            FunctionBody::Extern(extern_function_body) => {
                self.partial_visit_extern_function_body(extern_function_body)
            }
        }
    }
    fn partial_visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) {
        self.partial_visit_statement(statement);
    }
    fn partial_visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token: _,
            id: _,
        }: &mut ExternFunctionBody,
    ) {
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
            Statement::WhileLoop(while_loop_statement) => {
                self.partial_visit_while_loop_statement(while_loop_statement)
            }
            Statement::ForLoop(for_loop_statement) => {
                self.partial_visit_for_loop_statement(for_loop_statement)
            }
            Statement::Break(break_statement) => {
                self.partial_visit_break_statement(break_statement)
            }
            Statement::Skip(skip_statement) => self.partial_visit_skip_statement(skip_statement),
        }
    }
    fn partial_visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }: &mut ExpressionStatement,
    ) {
        self.partial_visit_expression(expression);
    }

    fn partial_visit_on_statement(
        &mut self,
        OnStatement {
            on_token: _,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id: _,
        }: &mut OnStatement,
    ) {
        self.partial_visit_executor(executor);

        for OnStatementIterator {
            open_bracket_token: _,
            binding,
            equal_token: _,
            max_value,
            close_bracket_token: _,
        } in iterators
        {
            self.partial_visit_simple_binding(binding);
            self.partial_visit_expression(max_value);
        }

        for (binding, _comma) in bindings {
            self.partial_visit_lvalue(binding);
        }
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

    fn partial_visit_return_statement(
        &mut self,
        ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id: _,
        }: &mut ReturnStatement,
    ) {
        if let Some(retvalue) = value {
            self.partial_visit_expression(retvalue);
        }
    }

    fn partial_visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        }: &mut VariableDefinitionStatement,
    ) {
        self.partial_visit_simple_binding(binding);
        self.partial_visit_expression(value);
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
        self.partial_visit_expression(condition);
        self.partial_visit_statement(if_body);

        for (_token, condition, body) in elifs {
            self.partial_visit_expression(condition);
            self.partial_visit_statement(body);
        }

        if let Some((_, else_body)) = else_ {
            self.partial_visit_statement(else_body);
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
        self.partial_visit_expression(condition);
        self.partial_visit_statement(body);
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
        self.partial_visit_statement(&mut *initializer);
        if let Some(c) = condition {
            self.partial_visit_expression(c);
        }
        if let Some(i) = incrementer {
            self.partial_visit_expression(i);
        }

        self.partial_visit_statement(body);
    }
    fn partial_visit_break_statement(
        &mut self,
        BreakStatement {
            break_token: _,
            semicolon_token: _,
            id: _,
        }: &mut BreakStatement,
    ) {
    }
    fn partial_visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id: _,
        }: &mut SkipStatement,
    ) {
    }

    // executor hosts
    fn partial_visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_(self_executor_host) => {
                self.partial_visit_self_executor_host(self_executor_host)
            }
        }
    }
    fn partial_visit_self_executor_host(
        &mut self,
        SelfExecutorHost { token: _, id: _ }: &mut SelfExecutorHost,
    ) {
    }

    fn partial_visit_executor(&mut self, executor: &mut Executor) {
        match executor {
            Executor::Thread(thread_executor) => {
                self.partial_visit_thread_executor(thread_executor)
            }
            Executor::GPU(gpuexecutor) => self.partial_visit_gpu_executor(gpuexecutor),
        }
    }
    fn partial_visit_thread_executor(
        &mut self,
        ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ThreadExecutor,
    ) {
        self.partial_visit_executor_host(host);
        self.partial_visit_expression(index);
    }
    fn partial_visit_gpu_executor(
        &mut self,
        GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index,
            close_bracket_token: _,
            id: _,
        }: &mut GPUExecutor,
    ) {
        self.partial_visit_executor_host(host);
        self.partial_visit_expression(gpu_index);
    }

    // expressions
    fn partial_visit_expression(&mut self, expression: &mut Expression) {
        match expression {
            Expression::Literal(literal_expression) => {
                self.partial_visit_literal_expression(literal_expression)
            }
            Expression::Array(array_expression) => {
                self.partial_visit_array_expression(array_expression)
            }
            Expression::FunctionCall(function_call_expression) => {
                self.partial_visit_function_call_expression(function_call_expression)
            }
            Expression::CompilerExpression(compiler_expression) => {
                self.partial_visit_compiler_expression(compiler_expression)
            }
            Expression::ArrayIndex(array_index_expression) => {
                self.partial_visit_array_index_expression(array_index_expression)
            }
            Expression::Grouping(grouping_expression) => {
                self.partial_visit_grouping_expression(grouping_expression)
            }
            Expression::VariableAccess(variable_access_expression) => {
                self.partial_visit_variable_access_expression(variable_access_expression)
            }
            Expression::Unary(unary_expression) => {
                self.partial_visit_unary_expression(unary_expression)
            }
            Expression::Cast(cast_expression) => {
                self.partial_visit_cast_expression(cast_expression)
            }
            Expression::Binary(binary_expression) => {
                self.partial_visit_binary_expression(binary_expression)
            }
            Expression::VariableAssignment(variable_assignment_expression) => {
                self.partial_visit_variable_assignment_expression(variable_assignment_expression)
            }
        }
    }
    fn partial_visit_literal_expression(
        &mut self,
        LiteralExpression {
            value: _,
            token: _,
            id: _,
        }: &mut LiteralExpression,
    ) {
    }
    fn partial_visit_array_expression(
        &mut self,
        ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayExpression,
    ) {
        for (item, _comma) in elements {
            self.partial_visit_expression(item);
        }
    }
    fn partial_visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }: &mut FunctionCallExpression,
    ) {
        for (arg, _comma) in arguments {
            self.partial_visit_expression(arg);
        }
    }
    fn partial_visit_compiler_expression(
        &mut self,
        CompilerExpression {
            at_token: _,
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }: &mut CompilerExpression,
    ) {
        for (arg, _comma) in arguments {
            self.partial_visit_expression(arg);
        }
    }
    fn partial_visit_array_index_expression(
        &mut self,
        ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayIndexExpression,
    ) {
        self.partial_visit_expression(array);
        self.partial_visit_expression(index);
    }
    fn partial_visit_grouping_expression(
        &mut self,
        GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id: _,
        }: &mut GroupingExpression,
    ) {
        self.partial_visit_expression(subexpression);
    }
    fn partial_visit_variable_access_expression(
        &mut self,
        VariableAccessExpression {
            name: _,
            name_token: _,
            id: _,
        }: &mut VariableAccessExpression,
    ) {
    }
    fn partial_visit_unary_expression(
        &mut self,
        UnaryExpression {
            operator_token: _,
            operation: _,
            operand,
            id: _,
        }: &mut UnaryExpression,
    ) {
        self.partial_visit_expression(operand);
    }
    fn partial_visit_cast_expression(
        &mut self,
        CastExpression {
            operand,
            as_token: _,
            type_,
            id: _,
        }: &mut CastExpression,
    ) {
        self.partial_visit_expression(operand);
        self.partial_visit_type(type_);
    }
    fn partial_visit_binary_expression(
        &mut self,
        BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id: _,
        }: &mut BinaryExpression,
    ) {
        self.partial_visit_expression(left);
        self.partial_visit_expression(right);
    }
    fn partial_visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id: _,
        }: &mut VariableAssignmentExpression,
    ) {
        self.partial_visit_lvalue(&mut *lvalue);
        self.partial_visit_expression(right);
    }

    fn partial_visit_lvalue(&mut self, lvalue: &mut LValue) {
        match lvalue {
            LValue::Variable(variable_lvalue) => {
                self.partial_visit_variable_lvalue(variable_lvalue)
            }
            LValue::ArrayIndex(array_index_lvalue) => {
                self.partial_visit_array_index_lvalue(array_index_lvalue)
            }
            LValue::Grouping(grouping_lvalue) => {
                self.partial_visit_grouping_lvalue(grouping_lvalue)
            }
        }
    }
    fn partial_visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name: _,
            name_token: _,
            id: _,
        }: &mut VariableLValue,
    ) {
    }
    fn partial_visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayIndexLValue,
    ) {
        self.partial_visit_lvalue(&mut *array);
        self.partial_visit_expression(&mut *index);
    }
    fn partial_visit_grouping_lvalue(
        &mut self,
        GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id: _,
        }: &mut GroupingLValue,
    ) {
        self.partial_visit_lvalue(&mut *sublvalue);
    }

    fn partial_visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::Simple(int_type) => self.partial_visit_int_type(int_type),
            Type::Unit(unit_type) => self.partial_visit_unit_type(unit_type),
            Type::Idk(idk_type) => self.partial_visit_idk_type(idk_type),
            Type::Array(array_type) => self.partial_visit_array_type(array_type),
        }
    }
    fn partial_visit_int_type(
        &mut self,
        SimpleType {
            token: _,
            type_: _,
            id: _,
        }: &mut SimpleType,
    ) {
    }
    fn partial_visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id: _,
        }: &mut UnitType,
    ) {
    }
    fn partial_visit_idk_type(&mut self, IdkType { token: _, id: _ }: &mut IdkType) {}
    fn partial_visit_array_type(
        &mut self,
        ArrayType {
            subtype,
            open_bracket_token: _,
            size,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayType,
    ) {
        self.partial_visit_type(subtype);
        if let Some(size) = size {
            self.partial_visit_expression(size);
        }
    }
}

impl<T> AstVisitor for T
where
    T: PartialAstVisitor,
{
    type ProgramOutput = ();
    type TopLevelOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = ();
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = ();
    type LValueOutput = ();
    type TypeOutput = ();

    fn visit_program(self, program: &mut Program) -> Self::ProgramOutput {
        self.partial_visit_program(program);
    }

    fn visit_function_definition(
        &mut self,

        function_definition: &mut FunctionDefinition,
    ) -> Self::TopLevelOutput {
        self.partial_visit_function_definition(function_definition);
    }

    fn visit_statement_function_body(
        &mut self,
        statement_function_body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.partial_visit_statement_function_body(statement_function_body);
    }

    fn visit_extern_function_body(
        &mut self,
        extern_function_body: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.partial_visit_extern_function_body(extern_function_body);
    }

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        self.partial_visit_simple_binding(simple_binding);
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.partial_visit_expression_statement(expr_stmt);
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        self.partial_visit_on_statement(on_stmt);
    }

    fn visit_block_statement(&mut self, block: &mut BlockStatement) -> Self::StatementOutput {
        self.partial_visit_block_statement(block);
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        self.partial_visit_return_statement(return_stmt);
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.partial_visit_variable_definition_statement(vardef_stmt);
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        self.partial_visit_if_statement(if_stmt);
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.partial_visit_while_loop_statement(while_stmt);
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.partial_visit_for_loop_statement(for_stmt);
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        self.partial_visit_break_statement(break_stmt);
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        self.partial_visit_skip_statement(skip_stmt);
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.partial_visit_self_executor_host(executor_host);
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        self.partial_visit_thread_executor(executor);
    }

    fn visit_gpu_executor(&mut self, executor: &mut GPUExecutor) -> Self::ExecutorOutput {
        self.partial_visit_gpu_executor(executor);
    }

    fn visit_expression(&mut self, expression: &mut Expression) -> Self::ExpressionOutput {
        self.partial_visit_expression(expression);
    }

    fn visit_literal_expression(
        &mut self,
        expression: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_literal_expression(expression);
    }

    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_array_expression(expression);
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_function_call_expression(expression);
    }

    fn visit_compiler_expression(
        &mut self,
        expression: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_compiler_expression(expression);
    }

    fn visit_array_index_expression(
        &mut self,
        expression: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_array_index_expression(expression);
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_grouping_expression(expression);
    }
    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_variable_access_expression(expression);
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_unary_expression(expression);
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        self.partial_visit_cast_expression(expression);
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_binary_expression(expression);
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        self.partial_visit_variable_assignment_expression(expression);
    }

    fn visit_lvalue(&mut self, lvalue: &mut LValue) -> Self::LValueOutput {
        self.partial_visit_lvalue(lvalue);
    }

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        self.partial_visit_variable_lvalue(lvalue);
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput {
        self.partial_visit_array_index_lvalue(lvalue);
    }

    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput {
        self.partial_visit_grouping_lvalue(lvalue);
    }

    // types
    fn visit_type(&mut self, type_: &mut Type) -> Self::TypeOutput {
        self.partial_visit_type(type_);
    }

    fn visit_simple_type(&mut self, int_type: &mut SimpleType) -> Self::TypeOutput {
        self.partial_visit_int_type(int_type);
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        self.partial_visit_unit_type(unit_type);
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.partial_visit_idk_type(idk_type);
    }

    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput {
        self.partial_visit_array_type(array_type);
    }
}
