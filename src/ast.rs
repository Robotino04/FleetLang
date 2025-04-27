use crate::tokenizer::Token;

pub enum AstNode {
    Program(Program),
    FunctionDefinition(FunctionDefinition),
    Statement(Statement),
    ExecutorHost(ExecutorHost),
    Executor(Executor),
    Expression(Expression),
    Type(Type),
}

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub let_token: Token,
    pub name_token: Token,
    pub return_type: Type,
    pub body: Statement,
}

#[derive(Clone, Debug)]
pub enum Type {
    I32 { token: Token },
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Expression),
    On {
        on_token: Token,
        executor: Executor,
        body: Box<Statement>,
    },
    Block(Vec<Statement>),
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
