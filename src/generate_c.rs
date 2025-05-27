use crate::ast::{
    AstNode, BinaryOperation, Executor, ExecutorHost, Expression, FunctionDefinition, Statement,
    Type, UnaryOperation,
};

fn generate_function_declaration(function: &FunctionDefinition) -> String {
    return generate_c(AstNode::Type(function.return_type.clone()))
        + " "
        + function.name.as_str()
        + "(void)";
}

pub fn generate_c(node: impl Into<AstNode>) -> String {
    match node.into() {
        AstNode::Program(program) => {
            let function_definitions = program
                .functions
                .iter()
                .map(|f| generate_c(AstNode::FunctionDefinition(f.clone())))
                .collect::<Vec<_>>()
                .join("\n");

            let function_declarations = program
                .functions
                .iter()
                .map(|f| generate_function_declaration(&f) + ";")
                .collect::<Vec<_>>()
                .join("\n");

            return format!(
                r##"#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// function declarations
{function_declarations}

// function definitions
{function_definitions}
"##
            );
        }
        AstNode::FunctionDefinition(function) => {
            return generate_function_declaration(&function)
                + " "
                + generate_c(AstNode::Statement(function.body)).as_str();
        }
        AstNode::Type(Type::I32 { token: _, id: _ }) => "int32_t".to_string(),
        AstNode::Statement(Statement::Expression {
            expression,
            semicolon_token: _,
            id: _,
        }) => generate_c(AstNode::Expression(expression)) + ";",
        AstNode::Statement(Statement::On {
            on_token: _,
            open_paren_token: _,
            executor: _,
            close_paren_token: _,
            body: _,
            id: _,
        }) => {
            todo!();
        }
        AstNode::Statement(Statement::Block {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        }) => {
            return "{\n".to_string()
                + indent::indent_all_by(
                    4,
                    body.iter()
                        .map(|tls| generate_c(AstNode::Statement(tls.clone())))
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
                .as_str()
                + "\n}";
        }
        AstNode::Statement(Statement::Return {
            return_token: _,
            value,
            semicolon_token: _,
            id: _,
        }) => {
            return "return ".to_string() + generate_c(AstNode::Expression(value)).as_str() + ";";
        }
        AstNode::Statement(Statement::VariableDefinition {
            let_token: _,
            name_token: _,
            name,
            colon_token: _,
            type_,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        }) => {
            return generate_c(type_) + " " + &name + " = " + &generate_c(value) + ";";
        }
        AstNode::Statement(Statement::If {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        }) => {
            return "if (".to_string()
                + &generate_c(condition)
                + ") {"
                + &generate_c(*if_body)
                + "}"
                + &elifs
                    .iter()
                    .map(|(_token, condition, body)| {
                        "else if (".to_string()
                            + &generate_c(condition.clone())
                            + ") {"
                            + &generate_c(body.clone())
                            + "}"
                    })
                    .collect::<String>()
                + &else_
                    .map(|(_token, body)| " else {".to_string() + &generate_c(*body) + "}")
                    .unwrap_or("".to_string());
        }
        AstNode::ExecutorHost(ExecutorHost::Self_ { token: _, id: _ }) => "self".to_string(),
        AstNode::Executor(Executor::Thread {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }) => {
            return generate_c(AstNode::ExecutorHost(host))
                + ".threads["
                + generate_c(AstNode::Expression(index)).as_str()
                + "]";
        }
        AstNode::Expression(Expression::Number {
            value,
            token: _,
            id: _,
        }) => value.to_string(),
        AstNode::Expression(Expression::FunctionCall {
            name,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }) => {
            return name.clone()
                + "("
                + arguments
                    .iter()
                    .map(|e| generate_c(AstNode::Expression(e.clone())))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str()
                + ")";
        }
        AstNode::Expression(Expression::Unary {
            operation,
            operand,
            operator_token: _,
            id: _,
        }) => {
            return match operation {
                UnaryOperation::BitwiseNot => "~",
                UnaryOperation::LogicalNot => "!",
                UnaryOperation::Negate => "-",
            }
            .to_string()
                + generate_c(AstNode::Expression(*operand)).as_str();
        }
        AstNode::Expression(Expression::Binary {
            left,
            operator_token: _,
            operation,
            right,
            id: _,
        }) => {
            return format!(
                "({} {} {})",
                generate_c(AstNode::Expression(*left)),
                match operation {
                    BinaryOperation::Add => "+",
                    BinaryOperation::Subtract => "-",
                    BinaryOperation::Multiply => "*",
                    BinaryOperation::Divide => "/",
                    BinaryOperation::Modulo => "%",
                    BinaryOperation::GreaterThan => ">",
                    BinaryOperation::GreaterThanOrEqual => ">=",
                    BinaryOperation::LessThan => "<",
                    BinaryOperation::LessThanOrEqual => "<=",
                    BinaryOperation::Equal => "==",
                    BinaryOperation::NotEqual => "!=",
                    BinaryOperation::LogicalAnd => "&&",
                    BinaryOperation::LogicalOr => "||",
                },
                generate_c(AstNode::Expression(*right))
            );
        }
        AstNode::Expression(Expression::Grouping {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id: _,
        }) => {
            return format!("({})", generate_c(AstNode::Expression(*subexpression)),);
        }
        AstNode::Expression(Expression::VariableAccess {
            name,
            name_token: _,
            id: _,
        }) => {
            return name;
        }
        AstNode::Expression(Expression::VariableAssignment {
            name,
            name_token: _,
            equal_token: _,
            right,
            id: _,
        }) => {
            return format!("({name} = {})", generate_c(AstNode::Expression(*right)),);
        }
    }
}
