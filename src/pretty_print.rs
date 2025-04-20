use crate::ast::{AstNode, Executor, ExecutorHost, Expression, Function, Program, Statement};

pub trait PrettyPrint: AstNode {
    fn pretty_print(&self) -> String;
}

impl PrettyPrint for Program {
    fn pretty_print(&self) -> String {
        return self
            .loose_statements
            .iter()
            .map(|stmt| stmt.pretty_print())
            .collect::<String>()
            + self
                .functions
                .iter()
                .map(|f| f.pretty_print())
                .collect::<String>()
                .as_str();
    }
}

impl PrettyPrint for Statement {
    fn pretty_print(&self) -> String {
        match self {
            Statement::Expression(expression) => expression.pretty_print() + ";\n",
            Statement::On {
                on_token: _,
                executor,
                body,
            } => {
                return "on (".to_string()
                    + executor.pretty_print().as_str()
                    + ") {\n    "
                    + indent::indent_by(
                        4,
                        body.iter()
                            .map(|stmt| stmt.pretty_print())
                            .collect::<String>(),
                    )
                    .as_str()
                    + "}\n";
            }
        }
    }
}

impl PrettyPrint for Function {
    fn pretty_print(&self) -> String {
        todo!()
    }
}

impl PrettyPrint for Expression {
    fn pretty_print(&self) -> String {
        match self {
            Expression::Number { value, token: _ } => value.to_string(),
            Expression::FunctionCall {
                name,
                name_token: _,
                arguments,
            } => {
                return name.clone()
                    + "("
                    + arguments
                        .iter()
                        .map(|e| e.pretty_print())
                        .collect::<String>()
                        .as_str()
                    + ")";
            }
        }
    }
}

impl PrettyPrint for Executor {
    fn pretty_print(&self) -> String {
        match self {
            Executor::Thread {
                thread_token: _,
                index,
                host,
            } => {
                return host.pretty_print() + ".threads[" + index.pretty_print().as_str() + "]";
            }
        }
    }
}

impl PrettyPrint for ExecutorHost {
    fn pretty_print(&self) -> String {
        match self {
            ExecutorHost::Self_ { token: _ } => "self".to_string(),
        }
    }
}
