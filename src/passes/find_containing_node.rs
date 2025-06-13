use crate::{
    ast::{
        AstNode, AstVisitor, BinaryExpression, BlockStatement, BoolExpression, BoolType,
        BreakStatement, CastExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionCallExpression, FunctionDefinition, GroupingExpression, IdkType, IfStatement,
        IntType, NumberExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost,
        SimpleBinding, SkipStatement, StatementFunctionBody, ThreadExecutor, UnaryExpression,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, WhileLoopStatement,
    },
    tokenizer::{SourceLocation, Token},
};

pub struct FindContainingNodePass {
    node_hierarchy: Vec<AstNode>,
    token: Option<Token>,
    search_position: SourceLocation,
}

impl FindContainingNodePass {
    pub fn new(search_position: SourceLocation) -> Self {
        Self {
            node_hierarchy: vec![],
            token: None,
            search_position,
        }
    }

    fn visit_token(&mut self, token: &Token) -> Result<(SourceLocation, SourceLocation), ()> {
        if (token.start.line..=token.end.line).contains(&self.search_position.line)
            && (token.start.column..token.end.column.saturating_sub(1))
                .contains(&self.search_position.column)
        {
            self.token = Some(token.clone());
            return Err(());
        }
        return Ok((
            token.start,
            SourceLocation {
                column: token.end.column.saturating_sub(1),
                ..token.end
            },
        ));
    }
}

impl AstVisitor for FindContainingNodePass {
    type ProgramOutput = Result<(Vec<AstNode>, Option<Token>), ()>;
    type FunctionDefinitionOutput = Result<(SourceLocation, SourceLocation), ()>;
    type FunctionBodyOutput = Result<(SourceLocation, SourceLocation), ()>;
    type SimpleBindingOutput = Result<(SourceLocation, SourceLocation), ()>;
    type StatementOutput = Result<(SourceLocation, SourceLocation), ()>;
    type ExecutorHostOutput = Result<(SourceLocation, SourceLocation), ()>;
    type ExecutorOutput = Result<(SourceLocation, SourceLocation), ()>;

    type ExpressionOutput = Result<(SourceLocation, SourceLocation), ()>;

    type TypeOutput = Result<(SourceLocation, SourceLocation), ()>;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        self.node_hierarchy.push(program.clone().into());

        for f in &mut program.functions {
            if let Err(()) = self.visit_function_definition(f) {
                return Ok((self.node_hierarchy, self.token));
            }
        }

        self.node_hierarchy.pop();
        return Err(());
    }

    fn visit_function_definition(
        &mut self,
        function: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        self.node_hierarchy.push(function.clone().into());

        let FunctionDefinition {
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
        } = function;

        let left_bound = self.visit_token(let_token)?.0;
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
        let right_bound = self.visit_function_body(body)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_statement_function_body(
        &mut self,
        statement_function_body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.node_hierarchy
            .push(statement_function_body.clone().into());

        let StatementFunctionBody { statement, id: _ } = statement_function_body;

        let (left_bound, right_bound) = self.visit_statement(statement)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_extern_function_body(
        &mut self,
        extern_function_body: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.node_hierarchy
            .push(extern_function_body.clone().into());

        let ExternFunctionBody {
            at_token,
            extern_token,
            symbol: _,
            symbol_token,
            semicolon_token,
            id: _,
        } = extern_function_body;

        let left_bound = self.visit_token(at_token)?.0;
        self.visit_token(extern_token)?;
        self.visit_token(symbol_token)?;
        let right_bound = self.visit_token(semicolon_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_simple_binding(&mut self, binding: &mut SimpleBinding) -> Self::SimpleBindingOutput {
        self.node_hierarchy.push(binding.clone().into());

        let (left_bound, mut right_bound) = self.visit_token(&mut binding.name_token)?;
        if let Some((colon_token, type_)) = &mut binding.type_ {
            self.visit_token(&colon_token)?;
            right_bound = self.visit_type(type_)?.1;
        }

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(expr_stmt.clone().into());

        let left_bound = self.visit_expression(&mut expr_stmt.expression)?.0;
        let right_bound = self.visit_token(&expr_stmt.semicolon_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(on_stmt.clone().into());

        let left_bound = self.visit_token(&on_stmt.on_token)?.0;
        self.visit_token(&on_stmt.open_paren_token)?;
        self.visit_executor(&mut on_stmt.executor)?;
        self.visit_token(&on_stmt.close_paren_token)?;
        let right_bound = self.visit_statement(&mut on_stmt.body)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_block_statement(&mut self, block_stmt: &mut BlockStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(block_stmt.clone().into());

        let left_bound = self.visit_token(&block_stmt.open_brace_token)?.0;
        for stmt in &mut block_stmt.body {
            self.visit_statement(stmt)?;
        }
        let right_bound = self.visit_token(&block_stmt.close_brace_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(return_stmt.clone().into());

        let left_bound = self.visit_token(&return_stmt.return_token)?.0;
        if let Some(retvalue) = &mut return_stmt.value {
            self.visit_expression(retvalue)?;
        }
        let right_bound = self.visit_token(&return_stmt.semicolon_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(vardef_stmt.clone().into());

        let left_bound = self.visit_token(&mut vardef_stmt.let_token)?.0;
        self.visit_simple_binding(&mut vardef_stmt.binding)?;
        self.visit_token(&vardef_stmt.equals_token)?;
        self.visit_expression(&mut vardef_stmt.value)?;
        let right_bound = self.visit_token(&vardef_stmt.semicolon_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(if_stmt.clone().into());

        let left_bound = self.visit_token(&if_stmt.if_token)?.0;
        self.visit_expression(&mut if_stmt.condition)?;
        let mut right_bound = self.visit_statement(&mut if_stmt.if_body)?.1;
        for (elif_token, elif_condition, elif_body) in &mut if_stmt.elifs {
            self.visit_token(elif_token)?;
            self.visit_expression(elif_condition)?;
            right_bound = self.visit_statement(elif_body)?.1;
        }
        if let Some((else_token, else_body)) = &mut if_stmt.else_ {
            self.visit_token(else_token)?;
            right_bound = self.visit_statement(else_body)?.1;
        }

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(while_stmt.clone().into());

        let left_bound = self.visit_token(&mut while_stmt.while_token)?.0;
        self.visit_expression(&mut while_stmt.condition)?;
        let right_bound = self.visit_statement(&mut while_stmt.body)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(for_stmt.clone().into());

        let left_bound = self.visit_token(&mut for_stmt.for_token)?.0;
        self.visit_token(&mut for_stmt.open_paren_token)?;
        self.visit_statement(&mut for_stmt.initializer)?;
        if let Some(c) = &mut for_stmt.condition {
            self.visit_expression(c)?;
        }
        self.visit_token(&mut for_stmt.second_semicolon_token)?;
        if let Some(i) = &mut for_stmt.incrementer {
            self.visit_expression(i)?;
        }
        self.visit_token(&mut for_stmt.close_paren_token)?;
        let right_bound = self.visit_statement(&mut for_stmt.body)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(break_stmt.clone().into());

        let left_bound = self.visit_token(&break_stmt.break_token)?.0;
        let right_bound = self.visit_token(&break_stmt.semicolon_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(skip_stmt.clone().into());

        let left_bound = self.visit_token(&skip_stmt.skip_token)?.0;
        let right_bound = self.visit_token(&skip_stmt.semicolon_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.node_hierarchy.push(executor_host.clone().into());

        let (left_bound, right_bound) = self.visit_token(&executor_host.token)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        self.node_hierarchy.push(executor.clone().into());

        let left_bound = self.visit_executor_host(&mut executor.host)?.0;
        self.visit_token(&executor.dot_token)?;
        self.visit_token(&executor.thread_token)?;
        self.visit_token(&executor.open_bracket_token)?;
        self.visit_expression(&mut executor.index)?;
        let right_bound = self.visit_token(&executor.close_bracket_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let (left_bound, right_bound) = self.visit_token(&expression.token)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_bool_expression(&mut self, expression: &mut BoolExpression) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let (left_bound, right_bound) = self.visit_token(&expression.token)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let left_bound = self.visit_token(&expression.name_token)?.0;
        self.visit_token(&expression.open_paren_token)?;
        for (arg, comma) in &mut expression.arguments {
            self.visit_expression(arg)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        let right_bound = self.visit_token(&expression.close_paren_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let left_bound = self.visit_token(&expression.open_paren_token)?.0;
        self.visit_expression(&mut expression.subexpression)?;
        let right_bound = self.visit_token(&expression.close_paren_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let (left_bound, right_bound) = self.visit_token(&expression.name_token)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let left_bound = self.visit_token(&expression.operator_token)?.0;
        let right_bound = self.visit_expression(&mut expression.operand)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let left_bound = self.visit_expression(&mut expression.operand)?.0;
        self.visit_token(&expression.as_token)?;
        let right_bound = self.visit_type(&mut expression.type_)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let left_bound = self.visit_expression(&mut expression.left)?.0;
        self.visit_token(&expression.operator_token)?;
        let right_bound = self.visit_expression(&mut expression.right)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let left_bound = self.visit_token(&expression.name_token)?.0;
        self.visit_token(&expression.equal_token)?;
        let right_bound = self.visit_expression(&mut expression.right)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_int_type(&mut self, type_: &mut IntType) -> Self::TypeOutput {
        self.node_hierarchy.push(type_.clone().into());

        let (left_bound, right_bound) = self.visit_token(&type_.token)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        self.node_hierarchy.push(unit_type.clone().into());

        let left_bound = self.visit_token(&unit_type.open_paren_token)?.0;
        let right_bound = self.visit_token(&unit_type.close_paren_token)?.1;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_bool_type(&mut self, bool_type: &mut BoolType) -> Self::TypeOutput {
        self.node_hierarchy.push(bool_type.clone().into());

        let (left_bound, right_bound) = self.visit_token(&bool_type.token)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.node_hierarchy.push(idk_type.clone().into());

        let (left_bound, right_bound) = self.visit_token(&idk_type.token)?;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        return Ok((left_bound, right_bound));
    }
}
