use crate::ast::{
    AstNode, Executor, ExecutorHost, Expression, FunctionDefinition, Statement, Type,
};

fn generate_function_declaration(function: &FunctionDefinition) -> String {
    return generate_c(AstNode::Type(function.return_type.clone()))
        + " "
        + function.name.as_str()
        + "(void)";
}

pub fn generate_c(node: AstNode) -> String {
    match node {
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
                .map(|f| generate_function_declaration(&f))
                .collect::<Vec<_>>()
                .join(";\n");

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
        AstNode::Type(Type::I32 { token: _ }) => "int32_t".to_string(),
        AstNode::Statement(Statement::Expression(expression)) => {
            generate_c(AstNode::Expression(expression)) + ";"
        }
        AstNode::Statement(Statement::On {
            on_token: _,
            executor: _,
            body: _,
        }) => {
            todo!();
        }
        AstNode::Statement(Statement::Block(body)) => {
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
        AstNode::ExecutorHost(ExecutorHost::Self_ { token: _ }) => "self".to_string(),
        AstNode::Executor(Executor::Thread {
            thread_token: _,
            index,
            host,
        }) => {
            return generate_c(AstNode::ExecutorHost(host))
                + ".threads["
                + generate_c(AstNode::Expression(index)).as_str()
                + "]";
        }
        AstNode::Expression(Expression::Number { value, token: _ }) => value.to_string(),
        AstNode::Expression(Expression::FunctionCall {
            name,
            name_token: _,
            arguments,
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
    }
}
