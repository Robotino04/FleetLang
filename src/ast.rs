use crate::tokenizer::Token;

#[derive(Clone, Debug)]
pub enum AstNode {
    Program(Program),
    FunctionDefinition(FunctionDefinition),
    Statement(Statement),
    ExecutorHost(ExecutorHost),
    Executor(Executor),
    Expression(Expression),
    Type(Type),
}

macro_rules! generate_unwrap {
    ($type:tt, $name:ident) => {
        pub fn $name(self) -> $type {
            if let AstNode::$type(contents) = self {
                contents
            } else {
                panic!("Expected AstNode::{}, found {:#?}", stringify!($type), self)
            }
        }
    };
}

impl AstNode {
    generate_unwrap!(Program, unwrap_program);
    generate_unwrap!(FunctionDefinition, unwrap_function_definition);
    generate_unwrap!(Statement, unwrap_statement);
    generate_unwrap!(ExecutorHost, unwrap_executor_host);
    generate_unwrap!(Executor, unwrap_executor);
    generate_unwrap!(Expression, unwrap_expression);
    generate_unwrap!(Type, unwrap_type);
}

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
}

impl From<Program> for AstNode {
    fn from(value: Program) -> Self {
        Self::Program(value)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub name: String,
    pub let_token: Token,
    pub name_token: Token,
    pub return_type: Type,
    pub body: Statement,
}
impl From<FunctionDefinition> for AstNode {
    fn from(value: FunctionDefinition) -> Self {
        Self::FunctionDefinition(value)
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    I32 { token: Token },
}

impl From<Type> for AstNode {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
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
    Return {
        return_token: Token,
        value: Expression,
    },
}

impl From<Statement> for AstNode {
    fn from(value: Statement) -> Self {
        Self::Statement(value)
    }
}

#[derive(Clone, Debug)]
pub enum ExecutorHost {
    Self_ { token: Token },
}

impl From<ExecutorHost> for AstNode {
    fn from(value: ExecutorHost) -> Self {
        Self::ExecutorHost(value)
    }
}

#[derive(Clone, Debug)]
pub enum Executor {
    Thread {
        thread_token: Token,
        index: Expression,
        host: ExecutorHost,
    },
}

impl From<Executor> for AstNode {
    fn from(value: Executor) -> Self {
        Self::Executor(value)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum UnaryOperation {
    BitwiseNot,
    LogicalNot,
    Negate,
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOperation {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
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
    Grouping {
        subexpression: Box<Expression>,
    },
    Unary {
        operator_token: Token,
        operation: UnaryOperation,
        operand: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator_token: Token,
        operation: BinaryOperation,
        right: Box<Expression>,
    },
}

impl From<Expression> for AstNode {
    fn from(value: Expression) -> Self {
        Self::Expression(value)
    }
}
