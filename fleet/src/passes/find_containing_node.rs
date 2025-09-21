use crate::ast::{
    ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNode, AstVisitor,
    BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
    ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
    FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
    LiteralExpression, OnStatement, OnStatementIterator, Program, ReturnStatement,
    SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody,
    StructAccessExpression, StructAccessLValue, StructExpression, StructMemberDefinition,
    StructMemberValue, StructType, ThreadExecutor, TypeAlias, UnaryExpression, UnitType,
    VariableAccessExpression, VariableAssignmentExpression, VariableDefinitionStatement,
    VariableLValue, WhileLoopStatement,
};
use crate::tokenizer::{SourceLocation, SourceRange, Token};

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

    fn visit_token(&mut self, token: &Token) -> Result<SourceRange, ()> {
        if token.range.contains(self.search_position) {
            self.token = Some(token.clone());
            return Err(());
        }
        Ok(token.range.start.until(SourceLocation {
            column: token.range.end.column.saturating_sub(1),
            ..token.range.end
        }))
    }
}

type ResultRange = Result<SourceRange, ()>;

impl AstVisitor for FindContainingNodePass {
    type ProgramOutput = Result<(Vec<AstNode>, Option<Token>), ()>;
    type TopLevelOutput = ResultRange;
    type FunctionBodyOutput = ResultRange;
    type SimpleBindingOutput = ResultRange;
    type StatementOutput = ResultRange;
    type ExecutorHostOutput = ResultRange;
    type ExecutorOutput = ResultRange;
    type ExpressionOutput = ResultRange;
    type LValueOutput = ResultRange;
    type TypeOutput = ResultRange;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        self.node_hierarchy.push(program.clone().into());

        for tls in &mut program.top_level_statements {
            if let Err(()) = self.visit_top_level_statement(tls) {
                return Ok((self.node_hierarchy, self.token));
            }
        }

        self.node_hierarchy.pop();
        Err(())
    }

    fn visit_function_definition(
        &mut self,
        function: &mut FunctionDefinition,
    ) -> Self::TopLevelOutput {
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

        let left_bound = self.visit_token(let_token)?.start;
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
        let right_bound = self.visit_function_body(body)?.end;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(left_bound.until(right_bound))
    }

    fn visit_type_alias(&mut self, type_alias: &mut TypeAlias) -> Self::TopLevelOutput {
        self.node_hierarchy.push(type_alias.clone().into());

        let TypeAlias {
            let_token,
            name: _,
            name_token,
            equal_token,
            type_,
            semicolon_token,
            id: _,
        } = type_alias;

        let left_bound = self.visit_token(let_token)?.start;
        self.visit_token(name_token)?;
        self.visit_token(equal_token)?;
            self.visit_type(type_)?;
        let right_bound = self.visit_token(semicolon_token)?.end;

        if left_bound <= self.search_position && self.search_position <= right_bound {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(left_bound.until(right_bound))
    }

    fn visit_statement_function_body(
        &mut self,
        statement_function_body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.node_hierarchy
            .push(statement_function_body.clone().into());

        let StatementFunctionBody { statement, id: _ } = statement_function_body;

        let range = self.visit_statement(statement)?;

        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
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

        let left_bound = self.visit_token(at_token)?.start;
        self.visit_token(extern_token)?;
        self.visit_token(symbol_token)?;
        let right_bound = self.visit_token(semicolon_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_simple_binding(&mut self, binding: &mut SimpleBinding) -> Self::SimpleBindingOutput {
        self.node_hierarchy.push(binding.clone().into());

        let SimpleBinding {
            name_token,
            name: _,
            type_,
            id: _,
        } = binding;

        let SourceRange {
            start: left_bound,
            end: mut right_bound,
        } = self.visit_token(name_token)?;
        if let Some((colon_token, type_)) = type_ {
            self.visit_token(colon_token)?;
            right_bound = self.visit_type(type_)?.end;
        }

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(expr_stmt.clone().into());

        let ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        } = expr_stmt;

        let left_bound = self.visit_expression(expression)?.start;
        let right_bound = self.visit_token(semicolon_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(on_stmt.clone().into());

        let OnStatement {
            on_token,
            executor,
            iterators,
            open_paren_token,
            bindings,
            close_paren_token,
            body,
            id: _,
        } = on_stmt;

        let left_bound = self.visit_token(on_token)?.start;
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
        let right_bound = self.visit_statement(body)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_block_statement(&mut self, block_stmt: &mut BlockStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(block_stmt.clone().into());

        let BlockStatement {
            open_brace_token,
            body,
            close_brace_token,
            id: _,
        } = block_stmt;

        let left_bound = self.visit_token(open_brace_token)?.start;
        for stmt in body {
            self.visit_statement(stmt)?;
        }
        let right_bound = self.visit_token(close_brace_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(return_stmt.clone().into());

        let ReturnStatement {
            return_token,
            value,
            semicolon_token,
            id: _,
        } = return_stmt;

        let left_bound = self.visit_token(return_token)?.start;
        if let Some(retvalue) = value {
            self.visit_expression(retvalue)?;
        }
        let right_bound = self.visit_token(semicolon_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(vardef_stmt.clone().into());

        let VariableDefinitionStatement {
            let_token,
            binding,
            equals_token,
            value,
            semicolon_token,
            id: _,
        } = vardef_stmt;

        let left_bound = self.visit_token(let_token)?.start;
        self.visit_simple_binding(binding)?;
        self.visit_token(equals_token)?;
        self.visit_expression(value)?;
        let right_bound = self.visit_token(semicolon_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(if_stmt.clone().into());

        let IfStatement {
            if_token,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        } = if_stmt;

        let left_bound = self.visit_token(if_token)?.start;
        self.visit_expression(condition)?;
        let mut right_bound = self.visit_statement(if_body)?.end;
        for (elif_token, elif_condition, elif_body) in elifs {
            self.visit_token(elif_token)?;
            self.visit_expression(elif_condition)?;
            right_bound = self.visit_statement(elif_body)?.end;
        }
        if let Some((else_token, else_body)) = else_ {
            self.visit_token(else_token)?;
            right_bound = self.visit_statement(else_body)?.end;
        }

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(while_stmt.clone().into());

        let WhileLoopStatement {
            while_token,
            condition,
            body,
            id: _,
        } = while_stmt;

        let left_bound = self.visit_token(while_token)?.start;
        self.visit_expression(condition)?;
        let right_bound = self.visit_statement(body)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.node_hierarchy.push(for_stmt.clone().into());

        let ForLoopStatement {
            for_token,
            open_paren_token,
            initializer,
            condition,
            second_semicolon_token,
            incrementer,
            close_paren_token,
            body,
            id: _,
        } = for_stmt;

        let left_bound = self.visit_token(for_token)?.start;
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
        let right_bound = self.visit_statement(body)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(break_stmt.clone().into());

        let BreakStatement {
            break_token,
            semicolon_token,
            id: _,
        } = break_stmt;

        let left_bound = self.visit_token(break_token)?.start;
        let right_bound = self.visit_token(semicolon_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        self.node_hierarchy.push(skip_stmt.clone().into());

        let SkipStatement {
            skip_token,
            semicolon_token,
            id: _,
        } = skip_stmt;

        let left_bound = self.visit_token(skip_token)?.start;
        let right_bound = self.visit_token(semicolon_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.node_hierarchy.push(executor_host.clone().into());

        let SelfExecutorHost { token, id: _ } = executor_host;

        let range = self.visit_token(token)?;

        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        self.node_hierarchy.push(executor.clone().into());

        let ThreadExecutor {
            host,
            dot_token,
            thread_token,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        } = executor;

        let left_bound = self.visit_executor_host(host)?.start;
        self.visit_token(dot_token)?;
        self.visit_token(thread_token)?;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(index)?;
        let right_bound = self.visit_token(close_bracket_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_gpu_executor(&mut self, executor: &mut GPUExecutor) -> Self::ExecutorOutput {
        self.node_hierarchy.push(executor.clone().into());

        let GPUExecutor {
            host,
            dot_token,
            gpus_token,
            open_bracket_token,
            gpu_index,
            close_bracket_token,
            id: _,
        } = executor;

        let left_bound = self.visit_executor_host(host)?.start;
        self.visit_token(dot_token)?;
        self.visit_token(gpus_token)?;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(gpu_index)?;
        let right_bound = self.visit_token(close_bracket_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_literal_expression(
        &mut self,
        expression: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let LiteralExpression {
            value: _,
            token,
            id: _,
        } = expression;

        let SourceRange {
            start: left_bound,
            end: right_bound,
        } = self.visit_token(token)?;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let ArrayExpression {
            open_bracket_token,
            elements,
            close_bracket_token,
            id: _,
        } = expression;

        let left_bound = self.visit_token(open_bracket_token)?.start;

        for (item, comma) in elements {
            self.visit_expression(item)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }

        let right_bound = self.visit_token(close_bracket_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_struct_expression(
        &mut self,
        expression: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let StructExpression {
            type_,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        } = expression;

        let left_bound = self.visit_type(type_)?.start;
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

        let right_bound = self.visit_token(close_brace_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let FunctionCallExpression {
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        } = expression;

        let left_bound = self.visit_token(name_token)?.start;
        self.visit_token(open_paren_token)?;
        for (arg, comma) in arguments {
            self.visit_expression(arg)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        let right_bound = self.visit_token(close_paren_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_compiler_expression(
        &mut self,
        expression: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let CompilerExpression {
            at_token,
            name: _,
            name_token,
            open_paren_token,
            arguments,
            close_paren_token,
            id: _,
        } = expression;

        let left_bound = self.visit_token(at_token)?.start;
        self.visit_token(name_token)?;
        self.visit_token(open_paren_token)?;
        for (arg, comma) in arguments {
            self.visit_expression(arg)?;
            if let Some(comma) = comma {
                self.visit_token(comma)?;
            }
        }
        let right_bound = self.visit_token(close_paren_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_array_index_expression(
        &mut self,
        expression: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let ArrayIndexExpression {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        } = expression;

        let left_bound = self.visit_expression(array)?.start;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(index)?;
        let right_bound = self.visit_token(close_bracket_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_struct_access_expression(
        &mut self,
        expression: &mut StructAccessExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let StructAccessExpression {
            value,
            dot_token,
            member_name: _,
            member_name_token,
            id: _,
        } = expression;

        let left_bound = self.visit_expression(value)?.start;
        self.visit_token(dot_token)?;
        let right_bound = self.visit_token(member_name_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let GroupingExpression {
            open_paren_token,
            subexpression,
            close_paren_token,
            id: _,
        } = expression;

        let left_bound = self.visit_token(open_paren_token)?.start;
        self.visit_expression(subexpression)?;
        let right_bound = self.visit_token(close_paren_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let VariableAccessExpression {
            name: _,
            name_token,
            id: _,
        } = expression;

        let SourceRange {
            start: left_bound,
            end: right_bound,
        } = self.visit_token(name_token)?;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let UnaryExpression {
            operator_token,
            operation: _,
            operand,
            id: _,
        } = expression;

        let left_bound = self.visit_token(operator_token)?.start;
        let right_bound = self.visit_expression(operand)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let CastExpression {
            operand,
            as_token,
            type_,
            id: _,
        } = expression;

        let left_bound = self.visit_expression(operand)?.start;
        self.visit_token(as_token)?;
        let right_bound = self.visit_type(type_)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let BinaryExpression {
            left,
            operator_token,
            operation: _,
            right,
            id: _,
        } = expression;

        let left_bound = self.visit_expression(left)?.start;
        self.visit_token(operator_token)?;
        let right_bound = self.visit_expression(right)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        self.node_hierarchy.push(expression.clone().into());

        let VariableAssignmentExpression {
            lvalue,
            equal_token,
            right,
            id: _,
        } = expression;

        let left_bound = self.visit_lvalue(lvalue)?.start;
        self.visit_token(equal_token)?;
        let right_bound = self.visit_expression(right)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        self.node_hierarchy.push(lvalue.clone().into());

        let VariableLValue {
            name: _,
            name_token,
            id: _,
        } = lvalue;

        let SourceRange {
            start: left_bound,
            end: right_bound,
        } = self.visit_token(name_token)?;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput {
        self.node_hierarchy.push(lvalue.clone().into());

        let ArrayIndexLValue {
            array,
            open_bracket_token,
            index,
            close_bracket_token,
            id: _,
        } = lvalue;

        let left_bound = self.visit_lvalue(array)?.start;
        self.visit_token(open_bracket_token)?;
        self.visit_expression(index)?;
        let right_bound = self.visit_token(close_bracket_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_struct_access_lvalue(
        &mut self,
        lvalue: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        self.node_hierarchy.push(lvalue.clone().into());

        let StructAccessLValue {
            value,
            dot_token,
            member_name: _,
            member_name_token,
            id: _,
        } = lvalue;

        let left_bound = self.visit_lvalue(value)?.start;
        self.visit_token(dot_token)?;
        let right_bound = self.visit_token(member_name_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput {
        self.node_hierarchy.push(lvalue.clone().into());

        let GroupingLValue {
            open_paren_token,
            sublvalue,
            close_paren_token,
            id: _,
        } = lvalue;

        let left_bound = self.visit_token(open_paren_token)?.start;
        self.visit_lvalue(sublvalue)?;
        let right_bound = self.visit_token(close_paren_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_simple_type(&mut self, type_: &mut SimpleType) -> Self::TypeOutput {
        self.node_hierarchy.push(type_.clone().into());

        let SimpleType {
            token,
            type_: _,
            id: _,
        } = type_;

        let SourceRange {
            start: left_bound,
            end: right_bound,
        } = self.visit_token(token)?;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        self.node_hierarchy.push(unit_type.clone().into());

        let UnitType {
            open_paren_token,
            close_paren_token,
            id: _,
        } = unit_type;

        let left_bound = self.visit_token(open_paren_token)?.start;
        let right_bound = self.visit_token(close_paren_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.node_hierarchy.push(idk_type.clone().into());

        let IdkType { token, id: _ } = idk_type;

        let SourceRange {
            start: left_bound,
            end: right_bound,
        } = self.visit_token(token)?;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput {
        self.node_hierarchy.push(array_type.clone().into());

        let ArrayType {
            subtype,
            open_bracket_token,
            size,
            close_bracket_token,
            id: _,
        } = array_type;

        let left_bound = self.visit_type(subtype)?.start;
        self.visit_token(open_bracket_token)?;
        if let Some(size) = size {
            self.visit_expression(size)?;
        }
        let right_bound = self.visit_token(close_bracket_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }

    fn visit_struct_type(&mut self, struct_type: &mut StructType) -> Self::TypeOutput {
        self.node_hierarchy.push(struct_type.clone().into());

        let StructType {
            struct_token,
            open_brace_token,
            members,
            close_brace_token,
            id: _,
        } = struct_type;

        let left_bound = self.visit_token(struct_token)?.start;
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
        let right_bound = self.visit_token(close_brace_token)?.end;

        let range = left_bound.until(right_bound);
        if range.contains(self.search_position) {
            return Err(());
        }

        self.node_hierarchy.pop();
        Ok(range)
    }
}
