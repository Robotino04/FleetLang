use crate::{
    ast::{
        AstNode, AstVisitor, BinaryExpression, BlockStatement, BreakStatement, ExpressionStatement,
        ForLoopStatement, FunctionCallExpression, FunctionDefinition, GroupingExpression, I32Type,
        IfStatement, NumberExpression, OnStatement, Program, ReturnStatement, SelfExecutorHost,
        SimpleBinding, SkipStatement, ThreadExecutor, UnaryExpression, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, WhileLoopStatement,
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

    fn visit_token(&mut self, token: &Token) -> Result<(), ()> {
        if (token.start.line..=token.end.line).contains(&self.search_position.line)
            && (token.start.column..token.end.column).contains(&self.search_position.column)
        {
            self.token = Some(token.clone());
            return Err(());
        }
        return Ok(());
    }
}

impl AstVisitor for FindContainingNodePass {
    type ProgramOutput = Result<(Vec<AstNode>, Token), ()>;
    type FunctionDefinitionOutput = Result<(), ()>;
    type SimpleBindingOutput = Result<(), ()>;
    type StatementOutput = Result<(), ()>;
    type ExecutorHostOutput = Result<(), ()>;
    type ExecutorOutput = Result<(), ()>;
    type ExpressionOutput = Result<(), ()>;

    type TypeOutput = Result<(), ()>;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        self.node_hierarchy.push(program.clone().into());

        for f in &mut program.functions {
            if let Err(()) = self.visit_function_definition(f) {
                return Ok((self.node_hierarchy, self.token.expect("a token should have been found if visit_function_definition returns Err(())")));
            }
        }
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

        self.visit_statement(body)?;
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
        self.visit_type(return_type)?;
        self.visit_statement(body)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token,
            name: _,
            colon_token,
            type_,
            id: _,
        }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        self.visit_token(name_token)?;
        self.visit_token(colon_token)?;
        self.visit_type(type_)?;
        return Ok(());
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(expr_stmt.clone().into());

        self.visit_expression(&mut expr_stmt.expression)?;
        self.visit_token(&expr_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(on_stmt.clone().into());

        self.visit_token(&on_stmt.on_token)?;
        self.visit_token(&on_stmt.open_paren_token)?;
        self.visit_executor(&mut on_stmt.executor)?;
        self.visit_token(&on_stmt.close_paren_token)?;
        self.visit_statement(&mut on_stmt.body)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_block_statement(&mut self, block_stmt: &mut BlockStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(block_stmt.clone().into());

        self.visit_token(&block_stmt.open_brace_token)?;
        for stmt in &mut block_stmt.body {
            self.visit_statement(stmt)?;
        }
        self.visit_token(&block_stmt.close_brace_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(return_stmt.clone().into());

        self.visit_token(&return_stmt.return_token)?;
        self.visit_expression(&mut return_stmt.value)?;
        self.visit_token(&return_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(vardef_stmt.clone().into());

        self.visit_token(&mut vardef_stmt.let_token)?;
        self.visit_simple_binding(&mut vardef_stmt.binding)?;
        self.visit_token(&vardef_stmt.equals_token)?;
        self.visit_expression(&mut vardef_stmt.value)?;
        self.visit_token(&vardef_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(if_stmt.clone().into());

        self.visit_token(&if_stmt.if_token)?;
        self.visit_expression(&mut if_stmt.condition)?;
        self.visit_statement(&mut if_stmt.if_body)?;
        for (elif_token, elif_condition, elif_body) in &mut if_stmt.elifs {
            self.visit_token(elif_token)?;
            self.visit_expression(elif_condition)?;
            self.visit_statement(elif_body)?;
        }
        if let Some((else_token, else_body)) = &mut if_stmt.else_ {
            self.visit_token(else_token)?;
            self.visit_statement(else_body)?;
        }

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(while_stmt.clone().into());

        self.visit_token(&mut while_stmt.while_token)?;
        self.visit_expression(&mut while_stmt.condition)?;
        self.visit_statement(&mut while_stmt.body)?;

        self.node_hierarchy.pop();
        Ok(())
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(for_stmt.clone().into());

        self.visit_token(&mut for_stmt.for_token)?;
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
        self.visit_statement(&mut for_stmt.body)?;

        self.node_hierarchy.pop();
        Ok(())
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(break_stmt.clone().into());

        self.visit_token(&break_stmt.break_token)?;
        self.visit_token(&break_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        Ok(())
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(skip_stmt.clone().into());

        self.visit_token(&skip_stmt.skip_token)?;
        self.visit_token(&skip_stmt.semicolon_token)?;

        self.node_hierarchy.pop();
        Ok(())
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.node_hierarchy.push(executor_host.clone().into());

        self.visit_token(&executor_host.token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        self.node_hierarchy.push(executor.clone().into());

        self.visit_executor_host(&mut executor.host)?;
        self.visit_token(&executor.dot_token)?;
        self.visit_token(&executor.thread_token)?;
        self.visit_token(&executor.open_bracket_token)?;
        self.visit_expression(&mut executor.index)?;
        self.visit_token(&executor.close_bracket_token)?;

        self.node_hierarchy.pop();

        return Ok(());
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());
        self.visit_token(&expression.token)?;
        self.node_hierarchy.pop();

        return Ok(());
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        self.visit_token(&expression.name_token)?;
        self.visit_token(&expression.open_paren_token)?;
        for (arg, comma) in &mut expression.arguments {
            self.visit_expression(arg)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        self.visit_token(&expression.close_paren_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        self.visit_token(&expression.open_paren_token)?;
        self.visit_expression(&mut expression.subexpression)?;
        self.visit_token(&expression.close_paren_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        self.visit_token(&expression.name_token)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        self.visit_token(&expression.operator_token)?;
        self.visit_expression(&mut expression.operand)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        self.visit_expression(&mut expression.left)?;
        self.visit_token(&expression.operator_token)?;
        self.visit_expression(&mut expression.right)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        self.visit_token(&expression.name_token)?;
        self.visit_token(&expression.equal_token)?;
        self.visit_expression(&mut expression.right)?;

        self.node_hierarchy.pop();
        return Ok(());
    }

    fn visit_i32_type(&mut self, type_: &mut I32Type) -> Self::TypeOutput {
        self.node_hierarchy.push(type_.clone().into());

        self.visit_token(&type_.token)?;

        self.node_hierarchy.pop();

        return Ok(());
    }
}
