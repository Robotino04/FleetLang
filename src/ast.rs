use crate::tokenizer::Token;

pub trait AstNode {}

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<Function>,
    pub loose_statements: Vec<Statement>,
}

impl AstNode for Program {}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub name_token: Token,
    pub body: Vec<Statement>,
}
impl AstNode for Function {}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Expression),
    On {
        on_token: Token,
        executor: Executor,
        body: Vec<Statement>,
    },
}
impl AstNode for Statement {}

#[derive(Clone, Debug)]
pub enum ExecutorHost {
    Self_ { token: Token },
}
impl AstNode for ExecutorHost {}

#[derive(Clone, Debug)]
pub enum Executor {
    Thread {
        thread_token: Token,
        index: Expression,
        host: ExecutorHost,
    },
}
impl AstNode for Executor {}

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
impl AstNode for Expression {}
