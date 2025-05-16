use crate::ast::{
    Associativity, AstVisitor, Executor, ExecutorHost, Expression, FunctionDefinition, Program,
    Statement, Type,
};

use super::{
    add_leading_trivia_pass::AddLeadingTriviaPass, add_trailing_trivia_pass::AddTrailingTriviaPass,
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

impl RemoveParensPass {
    pub fn new() -> Self {
        Self {
            parent_precedence: Expression::TOP_PRECEDENCE,
            parent_associativity: Associativity::Both,
            current_side: OperandSide::Left,
        }
    }
}

impl RemoveParensPass {}

impl AstVisitor for RemoveParensPass {
    fn visit_program(&mut self, program: &mut Program) {
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }
    }

    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.visit_statement(&mut function_definition.body);
    }

    fn visit_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Expression {
                expression,
                semicolon_token: _,
            } => {
                self.visit_expression(expression);
            }
            Statement::On {
                on_token: _,
                open_paren_token: _,
                executor,
                close_paren_token: _,
                body,
            } => {
                self.visit_executor(executor);
                self.visit_statement(body);
            }
            Statement::Block {
                open_brace_token: _,
                body,
                close_brace_token: _,
            } => {
                for stmt in body {
                    self.visit_statement(stmt);
                }
            }
            Statement::Return {
                return_token: _,
                value,
                semicolon_token: _,
            } => {
                self.visit_expression(value);
            }
            Statement::VariableDefinition {
                let_token: _,
                name_token: _,
                name: _,
                colon_token: _,
                type_,
                equals_token: _,
                value,
                semicolon_token: _,
            } => {
                self.visit_type(type_);
                self.visit_expression(value);
            }
        }
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_ { token: _ } => {}
        }
    }

    fn visit_executor(&mut self, executor: &mut Executor) {
        match executor {
            Executor::Thread {
                host,
                dot_token: _,
                thread_token: _,
                open_bracket_token: _,
                index,
                close_bracket_token: _,
            } => {
                self.visit_executor_host(host);
                self.visit_expression(index);
            }
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        let this_precedence = expression.get_precedence();
        let this_associativity = expression.get_associativity();

        match expression {
            Expression::Number { value: _, token: _ } => {}
            Expression::VariableAccess {
                name: _,
                name_token: _,
            } => {}
            Expression::FunctionCall {
                name: _,
                name_token: _,
                open_paren_token: _,
                arguments,
                close_paren_token: _,
            } => {
                for arg in arguments {
                    self.parent_precedence = Expression::TOP_PRECEDENCE;
                    self.parent_associativity = Associativity::Both;
                    self.visit_expression(arg);
                }
            }
            Expression::Grouping {
                open_paren_token,
                subexpression,
                close_paren_token,
            } => {
                let old_parent_precedence = self.parent_precedence;
                let old_parent_associativity = self.parent_associativity;
                let old_side = self.current_side;
                self.parent_precedence = Expression::TOP_PRECEDENCE;
                self.visit_expression(&mut *subexpression);

                let child_precedence = subexpression.get_precedence();
                let child_associativity = subexpression.get_associativity();

                let assiciativity_compatible = old_parent_associativity == child_associativity
                    || old_parent_associativity == Associativity::Both
                    || child_associativity == Associativity::Both;
                let can_remove_associativity = match old_side {
                    OperandSide::Left => {
                        old_parent_associativity == Associativity::Both
                            || old_parent_associativity == Associativity::Left
                    }
                    OperandSide::Right => {
                        old_parent_associativity == Associativity::Both
                            || old_parent_associativity == Associativity::Right
                    }
                };

                if old_parent_precedence > child_precedence
                    || (old_parent_precedence == child_precedence
                        && assiciativity_compatible
                        && can_remove_associativity)
                {
                    let leading_trivia = vec![
                        open_paren_token.leading_trivia.clone(),
                        open_paren_token.trailing_trivia.clone(),
                    ]
                    .concat();
                    let trailing_trivia = vec![
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
            Expression::Unary {
                operator_token: _,
                operation: _,
                operand,
            } => {
                self.parent_precedence = this_precedence;
                self.current_side = OperandSide::Left;
                self.visit_expression(&mut *operand);
            }
            Expression::Binary {
                left,
                operator_token: _,
                operation: _,
                right,
            } => {
                self.parent_precedence = this_precedence;
                self.parent_associativity = this_associativity;
                self.current_side = OperandSide::Left;
                self.visit_expression(&mut *left);

                self.parent_precedence = this_precedence;
                self.parent_associativity = this_associativity;
                self.current_side = OperandSide::Right;
                self.visit_expression(&mut *right);
            }
            Expression::VariableAssignment {
                name: _,
                name_token: _,
                equal_token: _,
                right,
            } => {
                self.parent_precedence = this_precedence;
                self.parent_associativity = this_associativity;
                self.current_side = OperandSide::Right;
                self.visit_expression(&mut *right);
            }
        }
    }

    fn visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { token: _ } => {}
        }
    }
}
