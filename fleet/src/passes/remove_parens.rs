use std::{cell::RefMut, ops::ControlFlow};

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, Associativity,
        BinaryExpression, CastExpression, Expression, ExpressionStatement, ForLoopStatement,
        FunctionCallExpression, GroupingExpression, GroupingLValue, IfStatement, LValue, Program,
        ReturnStatement, StructAccessExpression, ThreadExecutor, Type, UnaryExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, WhileLoopStatement,
    },
    error_reporting::{ErrorKind, Errors, Lint},
    passes::{
        partial_visitor::{AstWalker, PartialAstVisitor},
        pass_manager::{GlobalState, Pass, PassFactory, PassResult},
        token_accessors::{get_first_token, get_last_token},
    },
};

#[derive(Clone, Copy, PartialEq, Eq)]
enum OperandSide {
    Left,
    Right,
}

pub struct RemoveParensPass<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,

    parent_precedence: usize,
    parent_associativity: Associativity,
    current_side: OperandSide,

    /// used to keep parenthesized struct literals in if-conditions and thelike
    allow_bare_struct_literals: bool,
}

impl PassFactory for RemoveParensPass<'_> {
    type Output<'state> = RemoveParensPass<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        Ok(Self::Output {
            errors: state.get_mut_named()?,
            program: Some(state.get_mut_named()?),
            parent_precedence: Expression::TOP_PRECEDENCE,
            parent_associativity: Associativity::Both,
            current_side: OperandSide::Left,
            allow_bare_struct_literals: true,
        })
    }
}
impl Pass for RemoveParensPass<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        let _ = self.visit_program(&mut program);

        Ok(())
    }
}

impl RemoveParensPass<'_> {
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

    fn type_has_bare_struct_literal(type_: &Type) -> bool {
        match type_ {
            Type::Simple(_) => false,
            Type::Unit(_) => false,
            Type::Idk(_) => false,
            Type::Array(ArrayType {
                subtype,
                open_bracket_token: _,
                size: _,
                close_bracket_token: _,
                id: _,
            }) => Self::type_has_bare_struct_literal(subtype),
            Type::Struct(_) => true,
            Type::Alias(_) => false,
        }
    }

    fn has_bare_struct_literal(expr: &Expression) -> bool {
        match expr {
            Expression::Literal(_) => false,
            Expression::Array(_) => false,
            Expression::Struct(_) => true,
            Expression::FunctionCall(_) => false,
            Expression::CompilerExpression(_) => false,
            Expression::ArrayIndex(ArrayIndexExpression {
                array,
                open_bracket_token: _,
                index: _,
                close_bracket_token: _,
                id: _,
            }) => Self::has_bare_struct_literal(array),
            Expression::StructAccess(StructAccessExpression {
                value,
                dot_token: _,
                member_name: _,
                member_name_token: _,
                id: _,
            }) => Self::has_bare_struct_literal(value),
            Expression::Grouping(_) => false,
            Expression::VariableAccess(_) => false,
            Expression::Unary(UnaryExpression {
                operator_token: _,
                operation: _,
                operand,
                id: _,
            }) => Self::has_bare_struct_literal(operand),
            Expression::Cast(CastExpression {
                operand,
                as_token: _,
                type_,
                id: _,
            }) => {
                Self::has_bare_struct_literal(operand) || Self::type_has_bare_struct_literal(type_)
            }
            Expression::Binary(BinaryExpression {
                left,
                operator_token: _,
                operation: _,
                right,
                id: _,
            }) => Self::has_bare_struct_literal(left) || Self::has_bare_struct_literal(right),
            Expression::VariableAssignment(VariableAssignmentExpression {
                lvalue: _,
                equal_token: _,
                right,
                id: _,
            }) => Self::has_bare_struct_literal(right),
        }
    }
}

impl PartialAstVisitor for RemoveParensPass<'_> {
    type Break<'a> = ();

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> ControlFlow<()> {
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut expr_stmt.expression)?;

        ControlFlow::Continue(())
    }
    fn visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) -> ControlFlow<()> {
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        if let Some(retvalue) = &mut return_stmt.value {
            self.visit_expression(retvalue)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        }: &mut VariableDefinitionStatement,
    ) -> ControlFlow<()> {
        self.visit_simple_binding(binding)?;
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(value)?;

        ControlFlow::Continue(())
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> ControlFlow<()> {
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.allow_bare_struct_literals = false;
        self.visit_expression(&mut if_stmt.condition)?;
        self.allow_bare_struct_literals = true;
        self.visit_statement(&mut if_stmt.if_body)?;

        for (_token, condition, body) in &mut if_stmt.elifs {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.allow_bare_struct_literals = false;
            self.visit_expression(condition)?;
            self.allow_bare_struct_literals = true;
            self.visit_statement(&mut *body)?;
        }

        if let Some((_, else_body)) = &mut if_stmt.else_ {
            self.visit_statement(&mut *else_body)?;
        }

        ControlFlow::Continue(())
    }
    fn visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        }: &mut WhileLoopStatement,
    ) -> ControlFlow<()> {
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.allow_bare_struct_literals = false;
        self.visit_expression(condition)?;
        self.allow_bare_struct_literals = true;
        self.visit_statement(body)?;

        ControlFlow::Continue(())
    }
    fn visit_for_loop_statement(
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
    ) -> ControlFlow<()> {
        self.visit_statement(initializer)?;
        if let Some(cond) = condition {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(cond)?;
        }
        if let Some(inc) = incrementer {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(inc)?;
        }
        self.visit_statement(body)?;

        ControlFlow::Continue(())
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> ControlFlow<()> {
        self.visit_executor_host(&mut executor.host)?;
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut executor.index)?;

        ControlFlow::Continue(())
    }

    // need to use the general function here because we potentially change a nodes type which is
    // impossible otherwise
    fn visit_expression(&mut self, expression: &mut Expression) -> ControlFlow<()> {
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
                self.visit_expression(&mut *subexpression)?;

                let not_blocked_by_struct = self.allow_bare_struct_literals
                    || !Self::has_bare_struct_literal(subexpression);

                if not_blocked_by_struct
                    && RemoveParensPass::can_parens_be_removed(
                        old_parent_precedence,
                        old_parent_associativity,
                        old_side,
                        subexpression.get_precedence(),
                        subexpression.get_associativity(),
                    )
                {
                    let mut leading_trivia = [
                        open_paren_token.leading_trivia.clone(),
                        open_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();
                    let mut trailing_trivia = [
                        close_paren_token.leading_trivia.clone(),
                        close_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();

                    self.errors
                        .push(ErrorKind::Lint(Lint::UnnecessaryParentheses {
                            opening: open_paren_token.range.clone(),
                            closing: close_paren_token.range.clone(),
                        }));

                    get_first_token(&mut *subexpression)
                        .unwrap()
                        .leading_trivia
                        .append(&mut leading_trivia);
                    get_last_token(&mut *subexpression)
                        .unwrap()
                        .trailing_trivia
                        .append(&mut trailing_trivia);
                    *expression = (**subexpression).clone();
                }

                ControlFlow::Continue(())
            }
            _ => self.walk_expression(expression),
        }
    }
    fn visit_array_expression(&mut self, expression: &mut ArrayExpression) -> ControlFlow<()> {
        for (item, _comma) in &mut expression.elements {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(item)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> ControlFlow<()> {
        for (arg, _comma) in &mut expression.arguments {
            self.parent_precedence = Expression::TOP_PRECEDENCE;
            self.parent_associativity = Associativity::Both;
            self.visit_expression(arg)?;
        }

        ControlFlow::Continue(())
    }

    fn visit_array_index_expression(
        &mut self,
        expression: &mut ArrayIndexExpression,
    ) -> ControlFlow<()> {
        self.parent_precedence = Expression::ArrayIndex(expression.clone()).get_precedence();
        self.parent_associativity = Expression::ArrayIndex(expression.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.array)?;
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut expression.index)?;

        ControlFlow::Continue(())
    }
    fn visit_unary_expression(&mut self, expression: &mut UnaryExpression) -> ControlFlow<()> {
        self.parent_precedence = Expression::Unary(expression.clone()).get_precedence();
        self.parent_associativity = Expression::Unary(expression.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.operand)?;

        ControlFlow::Continue(())
    }
    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> ControlFlow<()> {
        self.parent_precedence = Expression::Cast(expression.clone()).get_precedence();
        self.parent_associativity = Expression::Cast(expression.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.operand)?;
        self.visit_type(&mut expression.type_)?;

        ControlFlow::Continue(())
    }

    fn visit_binary_expression(&mut self, expression: &mut BinaryExpression) -> ControlFlow<()> {
        let this_precedence = Expression::Binary(expression.clone()).get_precedence();
        let this_associativity = Expression::Binary(expression.clone()).get_associativity();

        self.parent_precedence = this_precedence;
        self.parent_associativity = this_associativity;
        self.current_side = OperandSide::Left;
        self.visit_expression(&mut expression.left)?;

        self.parent_precedence = this_precedence;
        self.parent_associativity = this_associativity;
        self.current_side = OperandSide::Right;
        self.visit_expression(&mut expression.right)?;

        ControlFlow::Continue(())
    }
    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> ControlFlow<()> {
        self.parent_precedence = LValue::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_lvalue(&mut expression.lvalue)?;

        let this_precedence = Expression::VariableAssignment(expression.clone()).get_precedence();
        let this_associativity =
            Expression::VariableAssignment(expression.clone()).get_associativity();

        self.parent_precedence = this_precedence;
        self.parent_associativity = this_associativity;
        self.current_side = OperandSide::Right;
        self.visit_expression(&mut expression.right)?;

        ControlFlow::Continue(())
    }

    // need to use the general function here because we potentially change a nodes type which is
    // impossible otherwise
    fn visit_lvalue(&mut self, lvalue: &mut LValue) -> ControlFlow<()> {
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
                self.visit_lvalue(&mut *sublvalue)?;

                if RemoveParensPass::can_parens_be_removed(
                    old_parent_precedence,
                    old_parent_associativity,
                    old_side,
                    sublvalue.get_precedence(),
                    sublvalue.get_associativity(),
                ) {
                    let mut leading_trivia = [
                        open_paren_token.leading_trivia.clone(),
                        open_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();
                    let mut trailing_trivia = [
                        close_paren_token.leading_trivia.clone(),
                        close_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();

                    self.errors
                        .push(ErrorKind::Lint(Lint::UnnecessaryParentheses {
                            opening: open_paren_token.range.clone(),
                            closing: close_paren_token.range.clone(),
                        }));

                    get_first_token(&mut *sublvalue)
                        .unwrap()
                        .leading_trivia
                        .append(&mut leading_trivia);
                    get_last_token(&mut *sublvalue)
                        .unwrap()
                        .trailing_trivia
                        .append(&mut trailing_trivia);
                    *lvalue = (**sublvalue).clone();
                }

                ControlFlow::Continue(())
            }
            _ => self.walk_lvalue(lvalue),
        }
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> ControlFlow<()> {
        self.parent_precedence = LValue::ArrayIndex(lvalue.clone()).get_precedence();
        self.parent_associativity = LValue::ArrayIndex(lvalue.clone()).get_associativity();
        self.current_side = OperandSide::Left;
        self.visit_lvalue(&mut lvalue.array)?;
        self.parent_precedence = Expression::TOP_PRECEDENCE;
        self.parent_associativity = Associativity::Both;
        self.visit_expression(&mut lvalue.index)?;

        ControlFlow::Continue(())
    }
}
