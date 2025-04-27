use crate::ast::{
    AstNode, Executor, ExecutorHost, Expression, FunctionDefinition, Statement, Type,
};

pub fn pretty_print(node: AstNode) -> String {
    match node {
        AstNode::Program(program) => {
            return program
                .functions
                .iter()
                .map(|f| pretty_print(AstNode::FunctionDefinition(f.clone())))
                .collect::<Vec<_>>()
                .join("\n");
        }
        AstNode::FunctionDefinition(FunctionDefinition {
            name,
            name_token: _,
            let_token: _,
            return_type,
            body,
        }) => {
            return format!(
                "let {name} = () -> {} {}",
                pretty_print(AstNode::Type(return_type)),
                pretty_print(AstNode::Statement(body))
            );
        }
        AstNode::Type(Type::I32 { token: _ }) => "i32".to_string(),
        AstNode::Statement(Statement::Expression(expression)) => {
            pretty_print(AstNode::Expression(expression)) + ";"
        }
        AstNode::Statement(Statement::On {
            on_token: _,
            executor,
            body,
        }) => {
            return "on (".to_string()
                + pretty_print(AstNode::Executor(executor)).as_str()
                + ") "
                + pretty_print(AstNode::Statement(*body)).as_str();
        }
        AstNode::Statement(Statement::Block(body)) => {
            return "{\n".to_string()
                + indent::indent_all_by(
                    4,
                    body.iter()
                        .map(|tls| pretty_print(AstNode::Statement(tls.clone())))
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
            return pretty_print(AstNode::ExecutorHost(host))
                + ".threads["
                + pretty_print(AstNode::Expression(index)).as_str()
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
                    .map(|e| pretty_print(AstNode::Expression(e.clone())))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str()
                + ")";
        }
    }
}
