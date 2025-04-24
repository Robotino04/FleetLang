use crate::ast::{
    AstNode, Executor, ExecutorHost, Expression, Function, Statement, TopLevelStatement,
};

fn generate_function_declaration(function: &Function) -> String {
    return "void ".to_string() + function.name.as_str() + "(void)";
}

pub fn generate_c(node: AstNode) -> String {
    match node {
        AstNode::Program(program) => {
            let functions = program.toplevel_statements.iter().filter_map(|tls| {
                if let TopLevelStatement::FunctionDefinition(f) = tls {
                    Some(f)
                } else {
                    None
                }
            });
            let function_definitions = functions
                .clone()
                .map(|f| {
                    generate_c(AstNode::TopLevelStatement(
                        TopLevelStatement::FunctionDefinition(f.clone()),
                    ))
                })
                .collect::<Vec<_>>()
                .join("\n");
            let function_declarations = functions
                .map(|f| generate_function_declaration(&f))
                .collect::<Vec<_>>()
                .join(";\n");

            let main_function = Function {
                name: "main".to_string(),
                name_token: None,
                body: Statement::Block(
                    program
                        .toplevel_statements
                        .iter()
                        .filter_map(|tls| match tls {
                            TopLevelStatement::LooseStatement(s) => Some(s.clone()),
                            _ => None,
                        })
                        .collect(),
                ),
            };
            let main_function_code = generate_c(AstNode::FunctionDefinition(main_function));

            return format!(
                r##"#include <stdio.h>
#include <stdlib.h>

// function declarations
{function_declarations}

// function definitions
{function_definitions}

{main_function_code}
"##
            );
        }
        AstNode::TopLevelStatement(TopLevelStatement::FunctionDefinition(function)) => {
            generate_c(AstNode::FunctionDefinition(function))
        }
        AstNode::TopLevelStatement(TopLevelStatement::LooseStatement(stmt)) => {
            generate_c(AstNode::Statement(stmt))
        }
        AstNode::FunctionDefinition(function) => {
            return generate_function_declaration(&function)
                + " "
                + generate_c(AstNode::Statement(function.body)).as_str();
        }
        AstNode::Statement(Statement::Expression(expression)) => {
            generate_c(AstNode::Expression(expression)) + ";"
        }
        AstNode::Statement(Statement::On {
            on_token: _,
            executor,
            body,
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
