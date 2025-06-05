use crate::ast::{
    AstNode, BinaryExpression, BinaryOperation, BlockStatement, ExpressionStatement,
    ForLoopStatement, FunctionCallExpression, FunctionDefinition, GroupingExpression, I32Type,
    IfStatement, NumberExpression, OnStatement, ReturnStatement, SelfExecutorHost, SimpleBinding,
    ThreadExecutor, UnaryExpression, UnaryOperation, VariableAccessExpression,
    VariableAssignmentExpression, VariableDefinitionStatement, WhileLoopStatement,
};

fn generate_function_declaration(function: &FunctionDefinition) -> String {
    return generate_c(function.return_type.clone()) + " " + function.name.as_str() + "(void)";
}

pub fn generate_c(node: impl Into<AstNode>) -> String {
    match node.into() {
        AstNode::Program(program) => {
            let function_definitions = program
                .functions
                .iter()
                .map(|f| generate_c(f.clone()))
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
                + generate_c(function.body).as_str();
        }
        AstNode::SimpleBinding(SimpleBinding {
            name_token: _,
            name,
            colon_token: _,
            type_,
            id: _,
        }) => {
            return format!("{} {}", generate_c(type_), name);
        }
        AstNode::I32Type(I32Type { token: _, id: _ }) => "int32_t".to_string(),
        AstNode::ExpressionStatement(ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }) => generate_c(expression) + ";",
        AstNode::OnStatement(OnStatement {
            on_token: _,
            open_paren_token: _,
            executor: _,
            close_paren_token: _,
            body: _,
            id: _,
        }) => {
            todo!();
        }
        AstNode::BlockStatement(BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        }) => {
            return "{\n".to_string()
                + indent::indent_all_by(
                    4,
                    body.iter()
                        .map(|tls| generate_c(tls.clone()))
                        .collect::<Vec<_>>()
                        .join("\n"),
                )
                .as_str()
                + "\n}";
        }
        AstNode::ReturnStatement(ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id: _,
        }) => {
            return "return ".to_string() + generate_c(value).as_str() + ";";
        }
        AstNode::VariableDefinitionStatement(VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        }) => {
            return generate_c(binding) + " = " + &generate_c(value) + ";";
        }
        AstNode::IfStatement(IfStatement {
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
        AstNode::WhileLoopStatement(WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        }) => {
            return "while (".to_string()
                + &generate_c(condition)
                + ") {"
                + &generate_c(*body)
                + "}";
        }
        AstNode::ForLoopStatement(ForLoopStatement {
            for_token: _,
            open_paren_token: _,
            initializer,
            condition,
            second_semicolon_token: _,
            incrementer,
            close_paren_token: _,
            body,
            id: _,
        }) => {
            return "for (".to_string()
                + &generate_c(*initializer)
                + &condition.map(|c| generate_c(c)).unwrap_or("".to_string())
                + ";"
                + &incrementer.map(|i| generate_c(i)).unwrap_or("".to_string())
                + ") {"
                + &generate_c(*body)
                + "}";
        }
        AstNode::BreakStatement(_break_statement) => "break;".to_string(),
        AstNode::SkipStatement(_skip_statement) => "continue;".to_string(),
        AstNode::SelfExecutorHost(SelfExecutorHost { token: _, id: _ }) => "self".to_string(),
        AstNode::ThreadExecutor(ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }) => {
            return generate_c(host) + ".threads[" + generate_c(index).as_str() + "]";
        }
        AstNode::NumberExpression(NumberExpression {
            value,
            token: _,
            id: _,
        }) => value.to_string(),
        AstNode::FunctionCallExpression(FunctionCallExpression {
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
                    .map(|e| generate_c(e.clone()))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str()
                + ")";
        }
        AstNode::UnaryExpression(UnaryExpression {
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
                + generate_c(*operand).as_str();
        }
        AstNode::BinaryExpression(BinaryExpression {
            left,
            operator_token: _,
            operation,
            right,
            id: _,
        }) => {
            return format!(
                "({} {} {})",
                generate_c(*left),
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
                generate_c(*right)
            );
        }
        AstNode::GroupingExpression(GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id: _,
        }) => {
            return format!("({})", generate_c(*subexpression),);
        }
        AstNode::VariableAccessExpression(VariableAccessExpression {
            name,
            name_token: _,
            id: _,
        }) => {
            return name;
        }
        AstNode::VariableAssignmentExpression(VariableAssignmentExpression {
            name,
            name_token: _,
            equal_token: _,
            right,
            id: _,
        }) => {
            return format!("({name} = {})", generate_c(*right),);
        }
    }
}
