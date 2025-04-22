use crate::tokenizer::Token;

pub enum AstNode {
    Program(Program),
    TopLevelStatement(TopLevelStatement),
    FunctionDefinition(Function),
    Statement(Statement),
    ExecutorHost(ExecutorHost),
    Executor(Executor),
    Expression(Expression),
}

#[derive(Clone, Debug)]
pub struct Program {
    pub toplevel_statements: Vec<TopLevelStatement>,
}

#[derive(Clone, Debug)]
pub enum TopLevelStatement {
    FunctionDefinition(Function),
    LooseStatement(Statement),
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub name_token: Token,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Expression),
    On {
        on_token: Token,
        executor: Executor,
        body: Vec<Statement>,
    },
}

#[derive(Clone, Debug)]
pub enum ExecutorHost {
    Self_ { token: Token },
}

#[derive(Clone, Debug)]
pub enum Executor {
    Thread {
        thread_token: Token,
        index: Expression,
        host: ExecutorHost,
    },
}

#[derive(Clone, Debug)]
pub enum Expression {
    Number {
        value: i64,
        token: Token,
    },
    FunctionCall {
        name: String,
        name_token: Token,
        arguments: Vec<Expression>,
    },
}
