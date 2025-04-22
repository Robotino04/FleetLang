use crate::ast::{AstNode, Executor, ExecutorHost, Expression, Statement, TopLevelStatement};

pub fn pretty_print(node: AstNode) -> String {
    match node {
        AstNode::Program(program) => {
            return program
                .toplevel_statements
                .iter()
                .map(|stmt| pretty_print(AstNode::TopLevelStatement(stmt.clone())))
                .collect::<Vec<_>>()
                .join("\n");
        }
        AstNode::TopLevelStatement(TopLevelStatement::FunctionDefinition(function)) => {
            pretty_print(AstNode::FunctionDefinition(function))
        }
        AstNode::TopLevelStatement(TopLevelStatement::LooseStatement(stmt)) => {
            pretty_print(AstNode::Statement(stmt))
        }
        AstNode::FunctionDefinition(function) => todo!(),
        AstNode::Statement(Statement::Expression(expression)) => {
            pretty_print(AstNode::Expression(expression)) + ";"
        }
        AstNode::Statement(Statement::On {
            on_token,
            executor,
            body,
        }) => {
            return "on (".to_string()
                + pretty_print(AstNode::Executor(executor)).as_str()
                + ") {\n"
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
        AstNode::ExecutorHost(ExecutorHost::Self_ { token }) => "self".to_string(),
        AstNode::Executor(Executor::Thread {
            thread_token,
            index,
            host,
        }) => {
            return pretty_print(AstNode::ExecutorHost(host))
                + ".threads["
                + pretty_print(AstNode::Expression(index)).as_str()
                + "]";
        }
        AstNode::Expression(Expression::Number { value, token }) => value.to_string(),
        AstNode::Expression(Expression::FunctionCall {
            name,
            name_token,
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
