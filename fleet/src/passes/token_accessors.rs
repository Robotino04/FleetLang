use std::ops::ControlFlow;

use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType,
        AstNodeRefMut, BinaryExpression, BlockStatement, BreakStatement, CastExpression,
        CompilerExpression, Executor, ExecutorHost, Expression, ExpressionStatement,
        ExternFunctionBody, ForLoopStatement, FunctionBody, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LValue, LiteralExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost,
        SimpleBinding, SimpleType, SkipStatement, Statement, StatementFunctionBody,
        StructAccessExpression, StructAccessLValue, StructExpression, StructType, ThreadExecutor,
        TopLevelStatement, Type, TypeAlias, UnaryExpression, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    passes::partial_visitor::{AstWalker, PartialAstVisitor, PartialAstVisitorExt},
    tokenizer::Token,
};

pub fn get_first_token<'a>(node: impl Into<AstNodeRefMut<'a>>) -> Option<&'a mut Token> {
    struct GetFirstToken;

    impl PartialAstVisitor for GetFirstToken {
        type Break<'a> = &'a mut Token;

        fn visit_token<'a>(&mut self, token: &'a mut Token) -> ControlFlow<Self::Break<'a>> {
            ControlFlow::Break(token)
        }
    }

    GetFirstToken.visit_node(node.into()).break_value()
}

pub fn get_last_token<'a>(node: impl Into<AstNodeRefMut<'a>>) -> Option<&'a mut Token> {
    GetLastToken.visit_node(node.into()).break_value()
}

// not in `get_last_token` to avoid making it way too long
struct GetLastToken;

// This deny essentially turns PartialAstVisitor into a non-partial AstVisitor
#[deny(clippy::missing_trait_methods)]
impl PartialAstVisitor for GetLastToken {
    type Break<'a> = &'a mut Token;

    fn visit_token<'a>(&mut self, token: &'a mut Token) -> ControlFlow<Self::Break<'a>> {
        ControlFlow::Break(token)
    }

    // need an empty impl here to make `clippy::missing_trait_methods` work correctly
    fn visit_pre<'a>(&mut self, _: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        ControlFlow::Continue(())
    }

    // need an empty impl here to make `clippy::missing_trait_methods` work correctly
    fn visit_post<'a>(&mut self, _: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        ControlFlow::Continue(())
    }

    fn visit_program<'a>(&mut self, program: &'a mut Program) -> ControlFlow<Self::Break<'a>> {
        if let Some(tls) = program.top_level_statements.last_mut() {
            self.visit_top_level_statement(tls)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_simple_binding<'a>(
        &mut self,
        SimpleBinding {
            type_, name_token, ..
        }: &'a mut SimpleBinding,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some((_colon, type_)) = type_ {
            self.visit_type(type_)
        } else {
            self.visit_token(name_token)
        }
    }

    fn visit_top_level_statement<'a>(
        &mut self,
        tls: &'a mut TopLevelStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_top_level_statement(tls)
    }

    fn visit_function_definition<'a>(
        &mut self,
        function_definition: &'a mut FunctionDefinition,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_function_body(&mut function_definition.body)
    }

    fn visit_type_alias<'a>(
        &mut self,
        type_alias: &'a mut TypeAlias,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut type_alias.semicolon_token)
    }

    fn visit_function_body<'a>(
        &mut self,
        function_body: &'a mut FunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_function_body(function_body)
    }

    fn visit_statement_function_body<'a>(
        &mut self,
        StatementFunctionBody { statement, .. }: &'a mut StatementFunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_statement(statement)
    }

    fn visit_extern_function_body<'a>(
        &mut self,
        ExternFunctionBody {
            semicolon_token, ..
        }: &'a mut ExternFunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(semicolon_token)
    }

    fn visit_statement<'a>(
        &mut self,
        statement: &'a mut Statement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_statement(statement)
    }

    fn visit_expression_statement<'a>(
        &mut self,
        ExpressionStatement {
            semicolon_token, ..
        }: &'a mut ExpressionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(semicolon_token)
    }

    fn visit_on_statement<'a>(
        &mut self,
        OnStatement { body, .. }: &'a mut OnStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_statement(body)
    }

    fn visit_block_statement<'a>(
        &mut self,
        BlockStatement {
            close_brace_token, ..
        }: &'a mut BlockStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(close_brace_token)
    }

    fn visit_return_statement<'a>(
        &mut self,
        ReturnStatement {
            semicolon_token, ..
        }: &'a mut ReturnStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(semicolon_token)
    }

    fn visit_variable_definition_statement<'a>(
        &mut self,
        VariableDefinitionStatement {
            semicolon_token, ..
        }: &'a mut VariableDefinitionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(semicolon_token)
    }

    fn visit_if_statement<'a>(
        &mut self,
        IfStatement {
            if_body,
            elifs,
            else_,
            ..
        }: &'a mut IfStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some((_, else_body)) = else_ {
            self.visit_statement(else_body)
        } else if let Some((_, _, body)) = elifs.last_mut() {
            self.visit_statement(body)
        } else {
            self.visit_statement(if_body)
        }
    }

    fn visit_while_loop_statement<'a>(
        &mut self,
        while_stmt: &'a mut WhileLoopStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_statement(&mut while_stmt.body)
    }

    fn visit_for_loop_statement<'a>(
        &mut self,
        for_stmt: &'a mut ForLoopStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_statement(&mut for_stmt.body)
    }

    fn visit_break_statement<'a>(
        &mut self,
        break_stmt: &'a mut BreakStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut break_stmt.break_token)
    }

    fn visit_skip_statement<'a>(
        &mut self,
        skip_stmt: &'a mut SkipStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut skip_stmt.skip_token)
    }

    fn visit_executor_host<'a>(
        &mut self,
        executor_host: &'a mut ExecutorHost,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_executor_host(executor_host)
    }

    fn visit_self_executor_host<'a>(
        &mut self,
        executor_host: &'a mut SelfExecutorHost,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut executor_host.token)
    }

    fn visit_executor<'a>(&mut self, executor: &'a mut Executor) -> ControlFlow<Self::Break<'a>> {
        self.walk_executor(executor)
    }

    fn visit_thread_executor<'a>(
        &mut self,
        executor: &'a mut ThreadExecutor,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut executor.close_bracket_token)
    }

    fn visit_gpu_executor<'a>(
        &mut self,
        executor: &'a mut GPUExecutor,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut executor.close_bracket_token)
    }

    fn visit_expression<'a>(
        &mut self,
        expression: &'a mut Expression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.walk_expression(expression)
    }

    fn visit_literal_expression<'a>(
        &mut self,
        expression: &'a mut LiteralExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.token)
    }

    fn visit_array_expression<'a>(
        &mut self,
        expression: &'a mut ArrayExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.close_bracket_token)
    }

    fn visit_struct_expression<'a>(
        &mut self,
        expression: &'a mut StructExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.close_brace_token)
    }

    fn visit_function_call_expression<'a>(
        &mut self,
        expression: &'a mut FunctionCallExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.close_paren_token)
    }

    fn visit_compiler_expression<'a>(
        &mut self,
        expression: &'a mut CompilerExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.close_paren_token)
    }

    fn visit_array_index_expression<'a>(
        &mut self,
        expression: &'a mut ArrayIndexExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.close_bracket_token)
    }

    fn visit_struct_access_expression<'a>(
        &mut self,
        expression: &'a mut StructAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.member_name_token)
    }

    fn visit_grouping_expression<'a>(
        &mut self,
        expression: &'a mut GroupingExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.close_paren_token)
    }

    fn visit_variable_access_expression<'a>(
        &mut self,
        expression: &'a mut VariableAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut expression.name_token)
    }

    fn visit_unary_expression<'a>(
        &mut self,
        expression: &'a mut UnaryExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(&mut expression.operand)
    }

    fn visit_cast_expression<'a>(
        &mut self,
        expression: &'a mut CastExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_type(&mut expression.type_)
    }

    fn visit_binary_expression<'a>(
        &mut self,
        expression: &'a mut BinaryExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(&mut expression.right)
    }

    fn visit_variable_assignment_expression<'a>(
        &mut self,
        expression: &'a mut VariableAssignmentExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_expression(&mut expression.right)
    }

    fn visit_lvalue<'a>(&mut self, lvalue: &'a mut LValue) -> ControlFlow<Self::Break<'a>> {
        self.walk_lvalue(lvalue)
    }

    fn visit_variable_lvalue<'a>(
        &mut self,
        lvalue: &'a mut VariableLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut lvalue.name_token)
    }

    fn visit_array_index_lvalue<'a>(
        &mut self,
        lvalue: &'a mut ArrayIndexLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut lvalue.close_bracket_token)
    }

    fn visit_struct_access_lvalue<'a>(
        &mut self,
        lvalue: &'a mut StructAccessLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut lvalue.member_name_token)
    }

    fn visit_grouping_lvalue<'a>(
        &mut self,
        lvalue: &'a mut GroupingLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut lvalue.close_paren_token)
    }

    fn visit_type<'a>(&mut self, ty: &'a mut Type) -> ControlFlow<Self::Break<'a>> {
        self.walk_type(ty)
    }

    fn visit_simple_type<'a>(
        &mut self,
        int_type: &'a mut SimpleType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut int_type.token)
    }

    fn visit_unit_type<'a>(&mut self, unit_type: &'a mut UnitType) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut unit_type.close_paren_token)
    }

    fn visit_idk_type<'a>(&mut self, idk_type: &'a mut IdkType) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut idk_type.token)
    }

    fn visit_array_type<'a>(
        &mut self,
        array_type: &'a mut ArrayType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut array_type.close_bracket_token)
    }

    fn visit_struct_type<'a>(
        &mut self,
        struct_type: &'a mut StructType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut struct_type.close_brace_token)
    }

    fn visit_alias_type<'a>(
        &mut self,
        alias_type: &'a mut AliasType,
    ) -> ControlFlow<Self::Break<'a>> {
        self.visit_token(&mut alias_type.name_token)
    }
}
