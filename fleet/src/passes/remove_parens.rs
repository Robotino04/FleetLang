use crate::ast::{
    ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, Associativity, AstVisitor,
    BinaryExpression, CastExpression, Expression, ExpressionStatement, ForLoopStatement,
    FunctionCallExpression, GroupingExpression, GroupingLValue, IfStatement, LValue,
    ReturnStatement, ThreadExecutor, UnaryExpression, VariableAssignmentExpression,
    VariableDefinitionStatement, WhileLoopStatement,
};

use super::{
    add_leading_trivia_pass::AddLeadingTriviaPass, add_trailing_trivia_pass::AddTrailingTriviaPass,
    partial_visitor::PartialAstVisitor,
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum OperandSide {
    Left,
    Right,
}

pub struct RemoveParensPass {
    parent_precedence: usize,
    parent_associativity: Associativity,
    current_side: OperandSide,
}

impl Default for RemoveParensPass {
    fn default() -> Self {
        Self {
            parent_precedence: Expression::TOP_PRECEDENCE,
            parent_associativity: Associativity::Both,
            current_side: OperandSide::Left,
        }
    }
}

impl RemoveParensPass {
    fn can_parens_be_removed(
        old_parent_precedence: usize,
        old_parent_associativity: Associativity,
        old_side: OperandSide,
        child_precedence: usize,
        child_associativity: Associativity,
    ) -> bool {
        let associativity_compatible = old_parent_associativity == child_associativity
            || old_parent_associativity == Associativity::Both
            || child_associativity == Associativity::Both;
        let can_remove_associativity = match old_side {
            OperandSide::Left => matches!(
                old_parent_associativity,
                Associativity::Left | Associativity::Both
            ),

            OperandSide::Right => matches!(
                old_parent_associativity,
                Associativity::Right | Associativity::Both
            ),
        };

        let parent_precedence_stronger = old_parent_precedence > child_precedence;
        let same_precedence_and_safe = old_parent_precedence == child_precedence
            && associativity_compatible
            && can_remove_associativity;

        parent_precedence_stronger || same_precedence_and_safe
    }
}

impl PartialAstVisitor for RemoveParensPass {
    fn partial_visit_expression_statement(&mut self, expr_stmt: &mut ExpressionStatement) {
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut expr_stmt.expression);
    }
    fn partial_visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) {
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        if let Some(retvalue) = &mut return_stmt.value {
            self.visit_expression(retvalue);
        }
    }

    fn partial_visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) {
        self.visit_simple_binding(&mut vardef_stmt.binding);
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut vardef_stmt.value);
    }

    fn partial_visit_if_statement(&mut self, if_stmt: &mut IfStatement) {
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut if_stmt.condition);
        self.visit_statement(&mut if_stmt.if_body);

        for (_token, condition, body) in &mut if_stmt.elifs {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(condition);
            self.visit_statement(&mut *body);
        }

        if let Some((_, else_body)) = &mut if_stmt.else_ {
            self.visit_statement(&mut *else_body);
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
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(condition);
        self.visit_statement(body);
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
        self.visit_statement(initializer);
        if let Some(cond) = condition {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(cond);
        }
        if let Some(inc) = incrementer {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(inc);
        }
        self.visit_statement(body);
    }

    fn partial_visit_thread_executor(&mut self, executor: &mut ThreadExecutor) {
        self.visit_executor_host(&mut executor.host);
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut executor.index);
    }

    // need to use the general function here because we may potentially change a nodes type which
    // is impossible otherwise
    fn partial_visit_expression(&mut self, expression: &mut Expression) {
        match expression {
            Expression::Grouping(GroupingExpression {
                open_paren_token,
                subexpression,
                close_paren_token,
                id: _,
            }) => {
                let old_parent_precedence = self.parent_precedence;
                let old_parent_associativity = self.parent_associativity;
                let old_side = self.current_side;
                self.parent_precedence = Expression::TOP_PRECEDENCE;
                self.parent_associativity = Associativity::Both;
                self.visit_expression(&mut *subexpression);

                if RemoveParensPass::can_parens_be_removed(
                    old_parent_precedence,
                    old_parent_associativity,
                    old_side,
                    subexpression.get_precedence(),
                    subexpression.get_associativity(),
                ) {
                    let leading_trivia = [
                        open_paren_token.leading_trivia.clone(),
                        open_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();
                    let trailing_trivia = [
                        close_paren_token.leading_trivia.clone(),
                        close_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();

                    let mut leading_pass = AddLeadingTriviaPass::new(leading_trivia);
                    leading_pass.visit_expression(&mut *subexpression);
                    let mut trailing_pass = AddTrailingTriviaPass::new(trailing_trivia);
                    trailing_pass.visit_expression(&mut *subexpression);
                    *expression = *subexpression.clone();
                }
            }
            Expression::Number(number_expression) => {
                self.partial_visit_number_expression(number_expression)
            }
            Expression::Bool(bool_expression) => {
                self.partial_visit_bool_expression(bool_expression)
            }
            Expression::Array(array_expression) => {
                self.partial_visit_array_expression(array_expression)
            }
            Expression::FunctionCall(function_call_expression) => {
                self.partial_visit_function_call_expression(function_call_expression)
            }
            Expression::ArrayIndex(array_index_expression) => {
                self.partial_visit_array_index_expression(array_index_expression)
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
    fn partial_visit_array_expression(&mut self, expression: &mut ArrayExpression) {
        for (item, _comma) in &mut expression.elements {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(item);
        }
    }

    fn partial_visit_function_call_expression(&mut self, expression: &mut FunctionCallExpression) {
        for (arg, _comma) in &mut expression.arguments {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(arg);
        }
    }

    fn partial_visit_array_index_expression(&mut self, expression: &mut ArrayIndexExpression) {
        self.parent_precedence = Expression::ArrayIndex(expression.clone()).get_precedence();
        self.parent_associativity = Expression::ArrayIndex(expression.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.array);
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut expression.index);
    }
    fn partial_visit_unary_expression(&mut self, expression: &mut UnaryExpression) {
        self.parent_precedence = Expression::Unary(expression.clone()).get_precedence();
        self.parent_associativity = Expression::Unary(expression.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.operand);
    }
    fn partial_visit_cast_expression(&mut self, expression: &mut CastExpression) {
        self.parent_precedence = Expression::Cast(expression.clone()).get_precedence();
        self.parent_associativity = Expression::Cast(expression.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.operand);
        self.visit_type(&mut expression.type_);
    }

    fn partial_visit_binary_expression(&mut self, expression: &mut BinaryExpression) {
        let this_precedence = Expression::Binary(expression.clone()).get_precedence();
        let this_associativity = Expression::Binary(expression.clone()).get_associativity();

        self.parent_precedence = this_precedence;
        self.parent_associativity = this_associativity;
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.left);

        self.parent_precedence = this_precedence;
        self.parent_associativity = this_associativity;
        self.current_side = OperandSide::Right;
        self.visit_expression(&mut expression.right);
    }
    fn partial_visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) {
        self.parent_precedence = LValue::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_lvalue(&mut expression.lvalue);

        let this_precedence = Expression::VariableAssignment(expression.clone()).get_precedence();
        let this_associativity =
            Expression::VariableAssignment(expression.clone()).get_associativity();

        self.parent_precedence = this_precedence;
        self.parent_associativity = this_associativity;
        self.current_side = OperandSide::Right;
        self.visit_expression(&mut expression.right);
    }

    // need to use the general function here because we may potentially change a nodes type which
    // is impossible otherwise
    fn partial_visit_lvalue(&mut self, lvalue: &mut LValue) {
        match lvalue {
            LValue::Grouping(GroupingLValue {
                open_paren_token,
                sublvalue,
                close_paren_token,
                id: _,
            }) => {
                let old_parent_precedence = self.parent_precedence;
                let old_parent_associativity = self.parent_associativity;
                let old_side = self.current_side;
                self.parent_precedence = LValue::TOP_PRECEDENCE;
                self.parent_associativity = Associativity::Both;
                self.visit_lvalue(&mut *sublvalue);

                if RemoveParensPass::can_parens_be_removed(
                    old_parent_precedence,
                    old_parent_associativity,
                    old_side,
                    sublvalue.get_precedence(),
                    sublvalue.get_associativity(),
                ) {
                    let leading_trivia = [
                        open_paren_token.leading_trivia.clone(),
                        open_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();
                    let trailing_trivia = [
                        close_paren_token.leading_trivia.clone(),
                        close_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();

                    let mut leading_pass = AddLeadingTriviaPass::new(leading_trivia);
                    leading_pass.visit_lvalue(&mut *sublvalue);
                    let mut trailing_pass = AddTrailingTriviaPass::new(trailing_trivia);
                    trailing_pass.visit_lvalue(&mut *sublvalue);
                    *lvalue = *sublvalue.clone();
                }
            }
            LValue::Variable(variable_lvalue) => {
                self.partial_visit_variable_lvalue(variable_lvalue);
            }
            LValue::ArrayIndex(array_index_lvalue) => {
                self.partial_visit_array_index_lvalue(array_index_lvalue);
            }
        }
    }

    fn partial_visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) {
        self.parent_precedence = LValue::ArrayIndex(lvalue.clone()).get_precedence();
        self.parent_associativity = LValue::ArrayIndex(lvalue.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_lvalue(&mut lvalue.array);
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut lvalue.index);
    }
}
