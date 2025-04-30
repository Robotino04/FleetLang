use crate::ast::{
    AstNode, BinaryOperation, Executor, ExecutorHost, Expression, FunctionDefinition, Program,
    Statement,
};

use super::ast_pass::AstPass;

pub struct RemoveParensPass {
    parent_precedence: usize,
}

impl RemoveParensPass {
    pub fn new() -> Self {
        Self {
            parent_precedence: Self::TOP_PRECEDENCE,
        }
    }
}

impl RemoveParensPass {
    const TOP_PRECEDENCE: usize = usize::MAX;
    fn get_precedence(&self, expr: &Expression) -> usize {
        match expr {
            Expression::Number { .. } => 0,
            Expression::FunctionCall { .. } => 0,
            Expression::Grouping { .. } => 0,

            Expression::Unary { .. } => 1,
            Expression::Binary {
                operation:
                    BinaryOperation::Multiply | BinaryOperation::Divide | BinaryOperation::Modulo,
                ..
            } => 2,
            Expression::Binary {
                operation: BinaryOperation::Add | BinaryOperation::Subtract,
                ..
            } => 3,
        }
    }
}

impl AstPass for RemoveParensPass {
    type Output = AstNode;
    fn run(&mut self, node: AstNode) -> AstNode {
        match node {
            AstNode::Program(program) => Program {
                functions: program
                    .functions
                    .iter()
                    .map(|f| self.run(f.clone().into()).unwrap_function_definition())
                    .collect(),
            }
            .into(),

            AstNode::FunctionDefinition(FunctionDefinition {
                name,
                let_token,
                name_token,
                return_type,
                body,
            }) => FunctionDefinition {
                name,
                let_token,
                name_token,
                return_type: self.run(return_type.into()).unwrap_type(),
                body: self.run(body.into()).unwrap_statement(),
            }
            .into(),
            type_ @ AstNode::Type(_) => type_,

            AstNode::Statement(Statement::Expression(expression)) => {
                self.parent_precedence = Self::TOP_PRECEDENCE;
                return AstNode::Statement(Statement::Expression(
                    self.run(AstNode::Expression(expression))
                        .unwrap_expression(),
                ));
            }

            AstNode::Statement(Statement::On {
                on_token,
                executor,
                body,
            }) => Statement::On {
                on_token,
                executor: self.run(executor.into()).unwrap_executor(),
                body: Box::new(self.run((*body).into()).unwrap_statement()),
            }
            .into(),
            AstNode::Statement(Statement::Block(body)) => Statement::Block(
                body.iter()
                    .map(|stmt| self.run(stmt.clone().into()).unwrap_statement())
                    .collect(),
            )
            .into(),
            AstNode::Statement(Statement::Return {
                return_token,
                value,
            }) => Statement::Return {
                return_token,
                value: self.run(value.into()).unwrap_expression(),
            }
            .into(),
            executor_host @ AstNode::ExecutorHost(ExecutorHost::Self_ { .. }) => executor_host,
            AstNode::Executor(Executor::Thread {
                thread_token,
                index,
                host,
            }) => Executor::Thread {
                thread_token,
                index: self.run(index.into()).unwrap_expression(),
                host: self.run(host.into()).unwrap_executor_host(),
            }
            .into(),
            number @ AstNode::Expression(Expression::Number { .. }) => number,
            AstNode::Expression(Expression::FunctionCall {
                name,
                name_token,
                arguments,
            }) => {
                return Expression::FunctionCall {
                    name,
                    name_token,
                    arguments: arguments
                        .iter()
                        .map(|arg| {
                            self.parent_precedence = Self::TOP_PRECEDENCE;
                            self.run(arg.clone().into()).unwrap_expression()
                        })
                        .collect(),
                }
                .into();
            }
            AstNode::Expression(
                ref expr @ Expression::Unary {
                    ref operator_token,
                    ref operation,
                    ref operand,
                },
            ) => {
                self.parent_precedence = self.get_precedence(&expr);
                return Expression::Unary {
                    operator_token: operator_token.clone(),
                    operation: operation.clone(),
                    operand: Box::new(self.run((**operand).clone().into()).unwrap_expression()),
                }
                .into();
            }
            AstNode::Expression(
                ref expr @ Expression::Binary {
                    ref left,
                    ref operator_token,
                    ref operation,
                    ref right,
                },
            ) => {
                self.parent_precedence = self.get_precedence(&expr);
                let left_tmp = self.run((**left).clone().into()).unwrap_expression();
                self.parent_precedence = self.get_precedence(&expr);
                let right_tmp = self.run((**right).clone().into()).unwrap_expression();
                return Expression::Binary {
                    left: Box::new(left_tmp.into()),
                    operator_token: operator_token.clone(),
                    operation: operation.clone(),
                    right: Box::new(right_tmp.into()),
                }
                .into();
            }
            AstNode::Expression(Expression::Grouping { subexpression }) => {
                if self.parent_precedence >= self.get_precedence(&*subexpression) {
                    self.parent_precedence = Self::TOP_PRECEDENCE;
                    return self.run((*subexpression).into()).into();
                } else {
                    self.parent_precedence = Self::TOP_PRECEDENCE;
                    return Expression::Grouping {
                        subexpression: Box::new(
                            self.run((*subexpression).into()).unwrap_expression(),
                        ),
                    }
                    .into();
                }
            }
        }
    }
}
