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

pub trait AstVisitor {
    fn visit_program(&mut self, program: &mut Program);
    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition);
    fn visit_statement(&mut self, statement: &mut Statement);
    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost);
    fn visit_executor(&mut self, executor: &mut Executor);
    fn visit_expression(&mut self, expression: &mut Expression);
    fn visit_type(&mut self, type_: &mut Type);
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
    pub let_token: Token,
    pub name: String,
    pub name_token: Token,
    pub equal_token: Token,
    pub open_paren_token: Token,
    pub close_paren_token: Token,
    pub right_arrow_token: Token,
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
    Expression {
        expression: Expression,
        semicolon_token: Token,
    },
    On {
        on_token: Token,
        open_paren_token: Token,
        executor: Executor,
        close_paren_token: Token,
        body: Box<Statement>,
    },
    Block {
        open_brace_token: Token,
        body: Vec<Statement>,
        close_brace_token: Token,
    },
    Return {
        return_token: Token,
        value: Expression,
        semicolon_token: Token,
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
        host: ExecutorHost,
        dot_token: Token,
        thread_token: Token,
        open_bracket_token: Token,
        index: Expression,
        close_bracket_token: Token,
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
        open_paren_token: Token,
        arguments: Vec<Expression>,
        close_paren_token: Token,
    },
    Grouping {
        open_paren_token: Token,
        subexpression: Box<Expression>,
        close_paren_token: Token,
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

impl Expression {
    pub const TOP_PRECEDENCE: usize = usize::MAX;
    pub fn get_precedence(&self) -> usize {
        match self {
            Expression::Number { .. } => 0,
            Expression::FunctionCall { .. } => 0,
            Expression::Grouping { .. } => 0,

            Expression::Unary { .. } => 1,
            Expression::Binary {
                operation:
                    BinaryOperation::Multiply | BinaryOperation::Divide | BinaryOperation::Modulo,
                ..
            } => 2,
            Expression::Binary {
                operation: BinaryOperation::Add | BinaryOperation::Subtract,
                ..
            } => 3,
        }
    }
}

impl From<Expression> for AstNode {
    fn from(value: Expression) -> Self {
        Self::Expression(value)
    }
}
