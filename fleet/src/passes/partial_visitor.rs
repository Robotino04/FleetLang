use std::ops::ControlFlow;

use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType,
        AstNodeRefMut, BinaryExpression, BlockStatement, BreakStatement, CastExpression,
        CompilerExpression, Executor, ExecutorHost, Expression, ExpressionStatement,
        ExternFunctionBody, ForLoopStatement, FunctionBody, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LValue, LiteralExpression, OnStatement, OnStatementIterator, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, Statement,
        StatementFunctionBody, StructAccessExpression, StructAccessLValue, StructExpression,
        StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor, TopLevelStatement,
        Type, TypeAlias, UnaryExpression, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    tokenizer::Token,
};

pub trait AstWalker: PartialAstVisitor {
    fn walk_program<'a>(&mut self, program: &'a mut Program) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(program.into())?;

        let Program {
            top_level_statements,
            id: _,
            file_name: _,
        } = program;

        for tls in top_level_statements {
            self.visit_top_level_statement(tls)?;
        }

        self.visit_post(program.into())
    }
    fn walk_simple_binding<'a>(
        &mut self,
        binding: &'a mut SimpleBinding,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(binding.into())?;
        let SimpleBinding {
            name_token,
            name: _,
            type_,
            id: _,
        } = binding;

        self.visit_token(name_token)?;

        if let Some((_colon, type_)) = type_ {
            self.visit_type(type_)?;
        }

        self.visit_post(binding.into())
    }

    fn walk_top_level_statement<'a>(
        &mut self,
        tls: &'a mut TopLevelStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(tls.into())?;

        match tls {
            TopLevelStatement::FunctionDefinition(function_definition) => {
                self.visit_function_definition(function_definition)?
            }
            TopLevelStatement::TypeAlias(type_alias) => self.visit_type_alias(type_alias)?,
        }

        self.visit_post(tls.into())
    }

    fn walk_function_definition<'a>(
        &mut self,
        FunctionDefinition {
            let_token,
            name: _,
            name_token,
            equal_token,
            open_paren_token,
            parameters,
            close_paren_token,
            right_arrow_token,
            return_type,
            body,
            id: _,
        }: &'a mut FunctionDefinition,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(let_token)?;
        self.visit_token(name_token)?;
        self.visit_token(equal_token)?;

        self.visit_token(open_paren_token)?;
        for (param, comma) in parameters {
            self.visit_simple_binding(param)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(close_paren_token)?;

        self.visit_token(right_arrow_token)?;

        if let Some(return_type) = return_type {
            self.visit_type(return_type)?;
        }
        self.visit_function_body(body)?;
        ControlFlow::Continue(())
    }
    fn walk_type_alias<'a>(
        &mut self,
        TypeAlias {
            let_token,
            name: _,
            name_token,
            equal_token,
            type_,
            semicolon_token,
            id: _,
        }: &'a mut TypeAlias,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(let_token)?;
        self.visit_token(name_token)?;
        self.visit_token(equal_token)?;
        self.visit_type(type_)?;
        self.visit_token(semicolon_token)?;
        ControlFlow::Continue(())
    }

    fn walk_function_body<'a>(
        &mut self,
        function_body: &'a mut FunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(function_body.into())?;

        match function_body {
            FunctionBody::Statement(statement_function_body) => {
                self.visit_statement_function_body(statement_function_body)?
            }
            FunctionBody::Extern(extern_function_body) => {
                self.visit_extern_function_body(extern_function_body)?
            }
        }

        self.visit_post(function_body.into())
    }
    fn walk_statement_function_body<'a>(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &'a mut StatementFunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_statement(statement)?;
        ControlFlow::Continue(())
    }
    fn walk_extern_function_body<'a>(
        &mut self,
        ExternFunctionBody {
            at_token,
            extern_token,
            symbol: _,
            symbol_token,
            semicolon_token,
            id: _,
        }: &'a mut ExternFunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(at_token)?;
        self.visit_token(extern_token)?;
        self.visit_token(symbol_token)?;
        self.visit_token(semicolon_token)?;
        ControlFlow::Continue(())
    }

    // statements
    fn walk_statement<'a>(&mut self, statement: &'a mut Statement) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(statement.into())?;

        match statement {
            Statement::Expression(stmt) => self.visit_expression_statement(stmt)?,
            Statement::On(stmt) => self.visit_on_statement(stmt)?,
            Statement::Block(stmt) => self.visit_block_statement(stmt)?,
            Statement::Return(stmt) => self.visit_return_statement(stmt)?,
            Statement::VariableDefinition(stmt) => {
                self.visit_variable_definition_statement(stmt)?
            }
            Statement::If(stmt) => self.visit_if_statement(stmt)?,
            Statement::WhileLoop(stmt) => self.visit_while_loop_statement(stmt)?,
            Statement::ForLoop(stmt) => self.visit_for_loop_statement(stmt)?,
            Statement::Break(stmt) => self.visit_break_statement(stmt)?,
            Statement::Skip(stmt) => self.visit_skip_statement(stmt)?,
        }

        self.visit_post(statement.into())
    }
    fn walk_expression_statement<'a>(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        }: &'a mut ExpressionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(expression)?;
        self.visit_token(semicolon_token)?;
        ControlFlow::Continue(())
    }

    fn walk_on_statement<'a>(
        &mut self,
        OnStatement {
            on_token,
            executor,
            iterators,
            open_paren_token,
            bindings,
            close_paren_token,
            body,
            id: _,
        }: &'a mut OnStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(on_token)?;
        self.visit_executor(executor)?;

        for OnStatementIterator {
            open_bracket_token,
            binding,
            equal_token,
            max_value,
            close_bracket_token,
        } in iterators
        {
            self.visit_token(open_bracket_token)?;
            self.visit_simple_binding(binding)?;
            self.visit_token(equal_token)?;
            self.visit_expression(max_value)?;
            self.visit_token(close_bracket_token)?;
        }

        self.visit_token(open_paren_token)?;
        for (binding, comma) in bindings {
            self.visit_lvalue(binding)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(close_paren_token)?;

        self.visit_statement(body)?;
        ControlFlow::Continue(())
    }

    fn walk_block_statement<'a>(
        &mut self,
        BlockStatement {
            open_brace_token,
            body,
            close_brace_token,
            id: _,
        }: &'a mut BlockStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(open_brace_token)?;
        for stmt in body {
            self.visit_statement(stmt)?;
        }
        self.visit_token(close_brace_token)?;
        ControlFlow::Continue(())
    }

    fn walk_return_statement<'a>(
        &mut self,
        ReturnStatement {
            return_token,
            value,
            semicolon_token,
            id: _,
        }: &'a mut ReturnStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(return_token)?;
        if let Some(retvalue) = value {
            self.visit_expression(retvalue)?;
        }
        self.visit_token(semicolon_token)?;
        ControlFlow::Continue(())
    }

    fn walk_variable_definition_statement<'a>(
        &mut self,
        VariableDefinitionStatement {
            let_token,
            binding,
            equals_token,
            value,
            semicolon_token,
            id: _,
        }: &'a mut VariableDefinitionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(let_token)?;
        self.visit_simple_binding(binding)?;
        self.visit_token(equals_token)?;
        self.visit_expression(value)?;
        self.visit_token(semicolon_token)?;
        ControlFlow::Continue(())
    }

    fn walk_if_statement<'a>(
        &mut self,
        IfStatement {
            if_token,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        }: &'a mut IfStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(if_token)?;
        self.visit_expression(condition)?;
        self.visit_statement(if_body)?;

        for (elif_token, condition, body) in elifs {
            self.visit_token(elif_token)?;
            self.visit_expression(condition)?;
            self.visit_statement(body)?;
        }

        if let Some((else_token, else_body)) = else_ {
            self.visit_token(else_token)?;
            self.visit_statement(else_body)?;
        }
        ControlFlow::Continue(())
    }
    fn walk_while_loop_statement<'a>(
        &mut self,
        WhileLoopStatement {
            while_token,
            condition,
            body,
            id: _,
        }: &'a mut WhileLoopStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(while_token)?;
        self.visit_expression(condition)?;
        self.visit_statement(body)?;
        ControlFlow::Continue(())
    }
    fn walk_for_loop_statement<'a>(
        &mut self,
        ForLoopStatement {
            for_token,
            open_paren_token,
            initializer,
            condition,
            second_semicolon_token,
            incrementer,
            close_paren_token,
            body,
            id: _,
        }: &'a mut ForLoopStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(for_token)?;
        self.visit_token(open_paren_token)?;
        self.visit_statement(initializer)?;
        if let Some(c) = condition {
            self.visit_expression(c)?;
        }
        self.visit_token(second_semicolon_token)?;
        if let Some(i) = incrementer {
            self.visit_expression(i)?;
        }
        self.visit_token(close_paren_token)?;

        self.visit_statement(body)?;
        ControlFlow::Continue(())
    }
    fn walk_break_statement<'a>(
        &mut self,
        BreakStatement {
            break_token,
            semicolon_token,
            id: _,
        }: &'a mut BreakStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(break_token)?;
        self.visit_token(semicolon_token)?;
        ControlFlow::Continue(())
    }
    fn walk_skip_statement<'a>(
        &mut self,
        SkipStatement {
            skip_token,
            semicolon_token,
            id: _,
        }: &'a mut SkipStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(skip_token)?;
        self.visit_token(semicolon_token)?;
        ControlFlow::Continue(())
    }

    // executor hosts
    fn walk_executor_host<'a>(
        &mut self,
        executor_host: &'a mut ExecutorHost,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(executor_host.into())?;

        match executor_host {
            ExecutorHost::Self_(host) => self.visit_self_executor_host(host)?,
        }

        self.visit_post(executor_host.into())
    }
    fn walk_self_executor_host<'a>(
        &mut self,
        SelfExecutorHost { token, id: _ }: &'a mut SelfExecutorHost,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(token)?;
        ControlFlow::Continue(())
    }

    fn walk_executor<'a>(&mut self, executor: &'a mut Executor) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(executor.into())?;

        match executor {
            Executor::Thread(executor) => self.visit_thread_executor(executor)?,
            Executor::GPU(executor) => self.visit_gpu_executor(executor)?,
        }

        self.visit_post(executor.into())
    }
    fn walk_thread_executor<'a>(
        &mut self,
        ThreadExecutor {
            host,
            dot_token,
            thread_token,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &'a mut ThreadExecutor,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_executor_host(host)?;
        self.visit_token(dot_token)?;
        self.visit_token(thread_token)?;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(index)?;
        self.visit_token(close_bracket_token)?;
        ControlFlow::Continue(())
    }
    fn walk_gpu_executor<'a>(
        &mut self,
        GPUExecutor {
            host,
            dot_token,
            gpus_token,
            open_bracket_token,
            gpu_index,
            close_bracket_token,
            id: _,
        }: &'a mut GPUExecutor,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_executor_host(host)?;
        self.visit_token(dot_token)?;
        self.visit_token(gpus_token)?;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(gpu_index)?;
        self.visit_token(close_bracket_token)?;
        ControlFlow::Continue(())
    }

    // expressions
    fn walk_expression<'a>(
        &mut self,
        expression: &'a mut Expression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(expression.into())?;

        match expression {
            Expression::Literal(expr) => self.visit_literal_expression(expr)?,
            Expression::Array(expr) => self.visit_array_expression(expr)?,
            Expression::Struct(expr) => self.visit_struct_expression(expr)?,
            Expression::FunctionCall(expr) => self.visit_function_call_expression(expr)?,
            Expression::CompilerExpression(expr) => self.visit_compiler_expression(expr)?,
            Expression::ArrayIndex(expr) => self.visit_array_index_expression(expr)?,
            Expression::StructAccess(expr) => self.visit_struct_access_expression(expr)?,
            Expression::Grouping(expr) => self.visit_grouping_expression(expr)?,
            Expression::VariableAccess(expr) => self.visit_variable_access_expression(expr)?,
            Expression::Unary(expr) => self.visit_unary_expression(expr)?,
            Expression::Cast(expr) => self.visit_cast_expression(expr)?,
            Expression::Binary(expr) => self.visit_binary_expression(expr)?,
            Expression::VariableAssignment(expr) => {
                self.visit_variable_assignment_expression(expr)?
            }
        }

        self.visit_post(expression.into())
    }
    fn walk_literal_expression<'a>(
        &mut self,
        LiteralExpression {
            value: _,
            token,
            id: _,
        }: &'a mut LiteralExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(token)?;
        ControlFlow::Continue(())
    }
    fn walk_array_expression<'a>(
        &mut self,
        ArrayExpression {
            open_bracket_token,
            elements,
            close_bracket_token,
            id: _,
        }: &'a mut ArrayExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(open_bracket_token)?;
        for (item, comma) in elements {
            self.visit_expression(item)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(close_bracket_token)?;
        ControlFlow::Continue(())
    }
    fn walk_struct_expression<'a>(
        &mut self,
        StructExpression {
            type_,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        }: &'a mut StructExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_type(type_)?;
        self.visit_token(open_brace_token)?;
        for (
            StructMemberValue {
                name: _,
                name_token,
                colon_token,
                value,
            },
            comma,
        ) in members
        {
            self.visit_token(name_token)?;
            self.visit_token(colon_token)?;
            self.visit_expression(value)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(close_brace_token)?;
        ControlFlow::Continue(())
    }
    fn walk_function_call_expression<'a>(
        &mut self,
        FunctionCallExpression {
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        }: &'a mut FunctionCallExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(name_token)?;
        self.visit_token(open_paren_token)?;
        for (arg, comma) in arguments {
            self.visit_expression(arg)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(close_paren_token)?;
        ControlFlow::Continue(())
    }
    fn walk_compiler_expression<'a>(
        &mut self,
        CompilerExpression {
            at_token,
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        }: &'a mut CompilerExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(at_token)?;
        self.visit_token(name_token)?;
        self.visit_token(open_paren_token)?;
        for (arg, comma) in arguments {
            self.visit_expression(arg)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(close_paren_token)?;
        ControlFlow::Continue(())
    }
    fn walk_array_index_expression<'a>(
        &mut self,
        ArrayIndexExpression {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &'a mut ArrayIndexExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(array)?;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(index)?;
        self.visit_token(close_bracket_token)?;
        ControlFlow::Continue(())
    }
    fn walk_struct_access_expression<'a>(
        &mut self,
        StructAccessExpression {
            value,
            dot_token,
            member_name: _,
            member_name_token,
            id: _,
        }: &'a mut StructAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(value)?;
        self.visit_token(dot_token)?;
        self.visit_token(member_name_token)?;
        ControlFlow::Continue(())
    }
    fn walk_grouping_expression<'a>(
        &mut self,
        GroupingExpression {
            open_paren_token,
            subexpression,
            close_paren_token,
            id: _,
        }: &'a mut GroupingExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(open_paren_token)?;
        self.visit_expression(subexpression)?;
        self.visit_token(close_paren_token)?;
        ControlFlow::Continue(())
    }
    fn walk_variable_access_expression<'a>(
        &mut self,
        VariableAccessExpression {
            name: _,
            name_token,
            id: _,
        }: &'a mut VariableAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(name_token)?;
        ControlFlow::Continue(())
    }
    fn walk_unary_expression<'a>(
        &mut self,
        UnaryExpression {
            operator_token,
            operation: _,
            operand,
            id: _,
        }: &'a mut UnaryExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(operator_token)?;
        self.visit_expression(operand)?;
        ControlFlow::Continue(())
    }
    fn walk_cast_expression<'a>(
        &mut self,
        CastExpression {
            operand,
            as_token,
            type_,
            id: _,
        }: &'a mut CastExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(operand)?;
        self.visit_token(as_token)?;
        self.visit_type(type_)?;
        ControlFlow::Continue(())
    }
    fn walk_binary_expression<'a>(
        &mut self,
        BinaryExpression {
            left,
            operator_token,
            operation: _,
            right,
            id: _,
        }: &'a mut BinaryExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(left)?;
        self.visit_token(operator_token)?;
        self.visit_expression(right)?;
        ControlFlow::Continue(())
    }
    fn walk_variable_assignment_expression<'a>(
        &mut self,
        VariableAssignmentExpression {
            lvalue,
            equal_token,
            right,
            id: _,
        }: &'a mut VariableAssignmentExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_lvalue(lvalue)?;
        self.visit_token(equal_token)?;
        self.visit_expression(right)?;
        ControlFlow::Continue(())
    }

    fn walk_lvalue<'a>(&mut self, lvalue: &'a mut LValue) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(lvalue.into())?;

        match lvalue {
            LValue::Variable(lvalue) => self.visit_variable_lvalue(lvalue)?,
            LValue::ArrayIndex(lvalue) => self.visit_array_index_lvalue(lvalue)?,
            LValue::StructAccess(lvalue) => self.visit_struct_access_lvalue(lvalue)?,
            LValue::Grouping(lvalue) => self.visit_grouping_lvalue(lvalue)?,
        }

        self.visit_post(lvalue.into())
    }
    fn walk_variable_lvalue<'a>(
        &mut self,
        VariableLValue {
            name: _,
            name_token,
            id: _,
        }: &'a mut VariableLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(name_token)?;
        ControlFlow::Continue(())
    }
    fn walk_array_index_lvalue<'a>(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        }: &'a mut ArrayIndexLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_lvalue(array)?;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(index)?;
        self.visit_token(close_bracket_token)?;
        ControlFlow::Continue(())
    }
    fn walk_struct_access_lvalue<'a>(
        &mut self,
        StructAccessLValue {
            value,
            dot_token,
            member_name: _,
            member_name_token,
            id: _,
        }: &'a mut StructAccessLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_lvalue(value)?;
        self.visit_token(dot_token)?;
        self.visit_token(member_name_token)?;
        ControlFlow::Continue(())
    }
    fn walk_grouping_lvalue<'a>(
        &mut self,
        GroupingLValue {
            open_paren_token,
            sublvalue,
            close_paren_token,
            id: _,
        }: &'a mut GroupingLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(open_paren_token)?;
        self.visit_lvalue(sublvalue)?;
        self.visit_token(close_paren_token)?;
        ControlFlow::Continue(())
    }

    fn walk_type<'a>(&mut self, ty: &'a mut Type) -> ControlFlow<Self::Break<'a>> {
        self.visit_pre(ty.into())?;

        match ty {
            Type::Simple(ty) => self.visit_simple_type(ty)?,
            Type::Unit(ty) => self.visit_unit_type(ty)?,
            Type::Idk(ty) => self.visit_idk_type(ty)?,
            Type::Array(ty) => self.visit_array_type(ty)?,
            Type::Struct(ty) => self.visit_struct_type(ty)?,
            Type::Alias(ty) => self.visit_alias_type(ty)?,
        }

        self.visit_post(ty.into())
    }
    fn walk_simple_type<'a>(
        &mut self,
        SimpleType {
            token,
            type_: _,
            id: _,
        }: &'a mut SimpleType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(token)?;
        ControlFlow::Continue(())
    }
    fn walk_unit_type<'a>(
        &mut self,
        UnitType {
            open_paren_token,
            close_paren_token,
            id: _,
        }: &'a mut UnitType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(open_paren_token)?;
        self.visit_token(close_paren_token)?;
        ControlFlow::Continue(())
    }
    fn walk_idk_type<'a>(
        &mut self,
        IdkType { token, id: _ }: &'a mut IdkType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(token)?;
        ControlFlow::Continue(())
    }
    fn walk_array_type<'a>(
        &mut self,
        ArrayType {
            subtype,
            open_bracket_token,
            size,
            close_bracket_token,
            id: _,
        }: &'a mut ArrayType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_type(subtype)?;
        self.visit_token(open_bracket_token)?;
        if let Some(size) = size {
            self.visit_expression(size)?;
        }
        self.visit_token(close_bracket_token)?;
        ControlFlow::Continue(())
    }
    fn walk_struct_type<'a>(
        &mut self,
        StructType {
            struct_token,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        }: &'a mut StructType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(struct_token)?;
        self.visit_token(open_brace_token)?;
        for (
            StructMemberDefinition {
                name: _,
                name_token,
                colon_token,
                type_,
            },
            comma,
        ) in members
        {
            self.visit_token(name_token)?;
            self.visit_token(colon_token)?;
            self.visit_type(type_)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(close_brace_token)?;
        ControlFlow::Continue(())
    }
    fn walk_alias_type<'a>(
        &mut self,
        AliasType {
            name: _,
            name_token,
            id: _,
        }: &'a mut AliasType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(name_token)?;
        ControlFlow::Continue(())
    }
}

impl<T> AstWalker for T where T: PartialAstVisitor + ?Sized {}

/// Allows you to hook into parts of the AST traversal and even take over traversal if needed. In
/// your `visit_*` function, you either manually visit the children (using their respective
/// `visit_*` functions) or, if you don't need custom traversal, you can call `walk_*` to traverse
/// the children for you (in the order they appear in the source file).
///
/// `visit_token` and `visit_pre`/`visit_post` are similar in spirit. Under default traversal, the
/// token variant gets called for each token in the order they appeared in the source file.
/// `visit_pre` just gets called once when "entering" each node (i.e. before the children get
/// visited, also called pre-order traversal) and `visit_post` gets called once when "leaving" the
/// node (i.e. after the children got visited, also called post-order traversal). `visit_token`
/// shares some of the notes below, notably that it must be called manually if you're doing manual
/// traversal. But because visiting tokens is a lot more rare than visiting nodes, most visitors can
/// simply ignore this - if you are writing a visitor that needs the tokens, you'll most likely know
/// that from the start and implement your traversal accordingly.
///
/// # Notes regarding `visit_pre` and `visit_post`
///
/// `visit_pre` and `visit_post` don't automatically run during manual traversal. If you need that,
/// make sure to call them in every `visit_*` function marked as needing to do so that doesn't
/// delegate to the corresponding `walk_*` function.
///
/// If you don't foresee needing one or both hooks and therefore aren't calling them during custom
/// traversal, consider implementing them anyways with a comment explaining that it won't be called
/// reliably to make future readers aware of this.
///
/// Because the AST structure is a bit inconsistent, it isn't obvious where you do and don't need to
/// call `visit_pre` and `visit_post`. In general, for enum nodes, they called from the function
/// that dispatches to the variants corresponding function. But nodes like `SimpleBinding` aren't
/// enums and so they must call the two directly. It is noted in the doc comments of each `visit_*`
/// function whether it needs to call `visit_pre`/`visit_post` or not (you always need to call both
/// or neither). There is no annotation for when to call `visit_token` though, because that is
/// pretty obvious; if you have access to tokens, you should visit them if you need `visit_token` to
/// work.
pub trait PartialAstVisitor {
    type Break<'a>;

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_token<'a>(&mut self, token: &'a mut Token) -> ControlFlow<Self::Break<'a>> {
        let _ = token;
        ControlFlow::Continue(())
    }

    /// Should (obviously) not call `visit_pre` or `visit_post`.
    fn visit_pre<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        let _ = node;
        ControlFlow::Continue(())
    }
    /// Should (obviously) not call `visit_pre` or `visit_post`.
    fn visit_post<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        let _ = node;
        ControlFlow::Continue(())
    }

    /// Should call `visit_pre` and `visit_post`.
    fn visit_program<'a>(&mut self, program: &'a mut Program) -> ControlFlow<Self::Break<'a>> {
        self.walk_program(program)
    }
    /// Should call `visit_pre` and `visit_post`.
    fn visit_simple_binding<'a>(
        &mut self,
        binding: &'a mut SimpleBinding,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_simple_binding(binding)
    }

    /// Should call `visit_pre` and `visit_post`.
    fn visit_top_level_statement<'a>(
        &mut self,
        tls: &'a mut TopLevelStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_top_level_statement(tls)
    }

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_function_definition<'a>(
        &mut self,
        fdef: &'a mut FunctionDefinition,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_function_definition(fdef)
    }

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_type_alias<'a>(&mut self, alias: &'a mut TypeAlias) -> ControlFlow<Self::Break<'a>> {
        self.walk_type_alias(alias)
    }

    /// Should call `visit_pre` and `visit_post`.
    fn visit_function_body<'a>(
        &mut self,
        function_body: &'a mut FunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_function_body(function_body)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_statement_function_body<'a>(
        &mut self,
        body: &'a mut StatementFunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_statement_function_body(body)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_extern_function_body<'a>(
        &mut self,
        body: &'a mut ExternFunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_extern_function_body(body)
    }

    // statements
    /// Should call `visit_pre` and `visit_post`.
    fn visit_statement<'a>(
        &mut self,
        statement: &'a mut Statement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_statement(statement)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_expression_statement<'a>(
        &mut self,
        stmt: &'a mut ExpressionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_expression_statement(stmt)
    }

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_on_statement<'a>(
        &mut self,
        stmt: &'a mut OnStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_on_statement(stmt)
    }

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_block_statement<'a>(
        &mut self,
        stmt: &'a mut BlockStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_block_statement(stmt)
    }

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_return_statement<'a>(
        &mut self,
        stmt: &'a mut ReturnStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_return_statement(stmt)
    }

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_variable_definition_statement<'a>(
        &mut self,
        stmt: &'a mut VariableDefinitionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_variable_definition_statement(stmt)
    }

    /// Should not call `visit_pre` or `visit_post`.
    fn visit_if_statement<'a>(
        &mut self,
        stmt: &'a mut IfStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_if_statement(stmt)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_while_loop_statement<'a>(
        &mut self,
        stmt: &'a mut WhileLoopStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_while_loop_statement(stmt)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_for_loop_statement<'a>(
        &mut self,
        stmt: &'a mut ForLoopStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_for_loop_statement(stmt)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_break_statement<'a>(
        &mut self,
        stmt: &'a mut BreakStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_break_statement(stmt)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_skip_statement<'a>(
        &mut self,
        stmt: &'a mut SkipStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_skip_statement(stmt)
    }

    // executor hosts
    /// Should call `visit_pre` and `visit_post`.
    fn visit_executor_host<'a>(
        &mut self,
        executor_host: &'a mut ExecutorHost,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_executor_host(executor_host)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_self_executor_host<'a>(
        &mut self,
        host: &'a mut SelfExecutorHost,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_self_executor_host(host)
    }

    /// Should call `visit_pre` and `visit_post`.
    fn visit_executor<'a>(&mut self, executor: &'a mut Executor) -> ControlFlow<Self::Break<'a>> {
        self.walk_executor(executor)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_thread_executor<'a>(
        &mut self,
        executor: &'a mut ThreadExecutor,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_thread_executor(executor)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_gpu_executor<'a>(
        &mut self,
        executor: &'a mut GPUExecutor,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_gpu_executor(executor)
    }

    // expressions
    /// Should call `visit_pre` and `visit_post`.
    fn visit_expression<'a>(
        &mut self,
        expression: &'a mut Expression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_expression(expression)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_literal_expression<'a>(
        &mut self,
        expr: &'a mut LiteralExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_literal_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_array_expression<'a>(
        &mut self,
        expr: &'a mut ArrayExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_array_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_struct_expression<'a>(
        &mut self,
        expr: &'a mut StructExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_struct_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_function_call_expression<'a>(
        &mut self,
        expr: &'a mut FunctionCallExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_function_call_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_compiler_expression<'a>(
        &mut self,
        expr: &'a mut CompilerExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_compiler_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_array_index_expression<'a>(
        &mut self,
        expr: &'a mut ArrayIndexExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_array_index_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_struct_access_expression<'a>(
        &mut self,
        expr: &'a mut StructAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_struct_access_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_grouping_expression<'a>(
        &mut self,
        expr: &'a mut GroupingExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_grouping_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_variable_access_expression<'a>(
        &mut self,
        expr: &'a mut VariableAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_variable_access_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_unary_expression<'a>(
        &mut self,
        expr: &'a mut UnaryExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_unary_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_cast_expression<'a>(
        &mut self,
        expr: &'a mut CastExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_cast_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_binary_expression<'a>(
        &mut self,
        expr: &'a mut BinaryExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_binary_expression(expr)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_variable_assignment_expression<'a>(
        &mut self,
        expr: &'a mut VariableAssignmentExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_variable_assignment_expression(expr)
    }

    // lvalues
    /// Should call `visit_pre` and `visit_post`.
    fn visit_lvalue<'a>(&mut self, lvalue: &'a mut LValue) -> ControlFlow<Self::Break<'a>> {
        self.walk_lvalue(lvalue)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_variable_lvalue<'a>(
        &mut self,
        lvalue: &'a mut VariableLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_variable_lvalue(lvalue)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_array_index_lvalue<'a>(
        &mut self,
        lvalue: &'a mut ArrayIndexLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_array_index_lvalue(lvalue)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_struct_access_lvalue<'a>(
        &mut self,
        lvalue: &'a mut StructAccessLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_struct_access_lvalue(lvalue)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_grouping_lvalue<'a>(
        &mut self,
        lvalue: &'a mut GroupingLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_grouping_lvalue(lvalue)
    }

    // types
    /// Should call `visit_pre` and `visit_post`.
    fn visit_type<'a>(&mut self, ty: &'a mut Type) -> ControlFlow<Self::Break<'a>> {
        self.walk_type(ty)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_simple_type<'a>(&mut self, ty: &'a mut SimpleType) -> ControlFlow<Self::Break<'a>> {
        self.walk_simple_type(ty)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_unit_type<'a>(&mut self, ty: &'a mut UnitType) -> ControlFlow<Self::Break<'a>> {
        self.walk_unit_type(ty)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_idk_type<'a>(&mut self, ty: &'a mut IdkType) -> ControlFlow<Self::Break<'a>> {
        self.walk_idk_type(ty)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_array_type<'a>(&mut self, ty: &'a mut ArrayType) -> ControlFlow<Self::Break<'a>> {
        self.walk_array_type(ty)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_struct_type<'a>(&mut self, ty: &'a mut StructType) -> ControlFlow<Self::Break<'a>> {
        self.walk_struct_type(ty)
    }
    /// Should not call `visit_pre` or `visit_post`.
    fn visit_alias_type<'a>(&mut self, ty: &'a mut AliasType) -> ControlFlow<Self::Break<'a>> {
        self.walk_alias_type(ty)
    }
}

pub trait PartialAstVisitorExt: PartialAstVisitor {
    fn visit_node<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        match node {
            AstNodeRefMut::Program(program) => self.visit_program(program),
            AstNodeRefMut::TopLevelStatement(tls) => self.visit_top_level_statement(tls),
            AstNodeRefMut::FunctionBody(function_body) => self.visit_function_body(function_body),
            AstNodeRefMut::SimpleBinding(binding) => self.visit_simple_binding(binding),
            AstNodeRefMut::Statement(statement) => self.visit_statement(statement),
            AstNodeRefMut::ExecutorHost(executor_host) => self.visit_executor_host(executor_host),
            AstNodeRefMut::Executor(executor) => self.visit_executor(executor),
            AstNodeRefMut::Expression(expression) => self.visit_expression(expression),
            AstNodeRefMut::LValue(lvalue) => self.visit_lvalue(lvalue),
            AstNodeRefMut::Type(ty) => self.visit_type(ty),
        }
    }
}

impl<T> PartialAstVisitorExt for T where T: PartialAstVisitor {}
