use std::collections::HashMap;

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

impl HasID for AstNode {
    fn get_id(&self) -> NodeID {
        match self {
            AstNode::Program(program) => program.get_id(),
            AstNode::FunctionDefinition(function_definition) => function_definition.get_id(),
            AstNode::Statement(statement) => statement.get_id(),
            AstNode::ExecutorHost(executor_host) => executor_host.get_id(),
            AstNode::Executor(executor) => executor.get_id(),
            AstNode::Expression(expression) => expression.get_id(),
            AstNode::Type(type_) => type_.get_id(),
        }
    }
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
    type SubOutput;
    type Output;

    fn visit_program(self, program: &mut Program) -> Self::Output;
    fn visit_function_definition(
        &mut self,
        function_definition: &mut FunctionDefinition,
    ) -> Self::SubOutput;
    fn visit_statement(&mut self, statement: &mut Statement) -> Self::SubOutput;
    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) -> Self::SubOutput;
    fn visit_executor(&mut self, executor: &mut Executor) -> Self::SubOutput;
    fn visit_expression(&mut self, expression: &mut Expression) -> Self::SubOutput;
    fn visit_type(&mut self, type_: &mut Type) -> Self::SubOutput;
}

pub trait HasID {
    fn get_id(&self) -> NodeID;
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct NodeID(pub u64);

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
    pub id: NodeID,
}

impl HasID for Program {
    fn get_id(&self) -> NodeID {
        self.id
    }
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
    // TODO: maybe store comma tokens too, once we have arguments
    pub close_paren_token: Token,
    pub right_arrow_token: Token,
    pub return_type: Type,
    pub body: Statement,
    pub id: NodeID,
}

impl HasID for FunctionDefinition {
    fn get_id(&self) -> NodeID {
        self.id
    }
}

impl From<FunctionDefinition> for AstNode {
    fn from(value: FunctionDefinition) -> Self {
        Self::FunctionDefinition(value)
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    I32 { token: Token, id: NodeID },
}

impl HasID for Type {
    fn get_id(&self) -> NodeID {
        match self {
            Type::I32 { id, .. } => *id,
        }
    }
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
        id: NodeID,
    },
    On {
        on_token: Token,
        open_paren_token: Token,
        executor: Executor,
        close_paren_token: Token,
        body: Box<Statement>,
        id: NodeID,
    },
    Block {
        open_brace_token: Token,
        body: Vec<Statement>,
        close_brace_token: Token,
        id: NodeID,
    },
    Return {
        return_token: Token,
        value: Expression,
        semicolon_token: Token,
        id: NodeID,
    },
    VariableDefinition {
        let_token: Token,
        name_token: Token,
        name: String,
        colon_token: Token,
        type_: Type,
        equals_token: Token,
        value: Expression,
        semicolon_token: Token,
        id: NodeID,
    },
    If {
        if_token: Token,
        condition: Expression,
        if_body: Box<Statement>,
        elifs: Vec<(Token, Expression, Statement)>,
        else_: Option<(Token, Box<Statement>)>,
        id: NodeID,
    },
}

impl HasID for Statement {
    fn get_id(&self) -> NodeID {
        match self {
            Statement::Expression { id, .. } => *id,
            Statement::On { id, .. } => *id,
            Statement::Block { id, .. } => *id,
            Statement::Return { id, .. } => *id,
            Statement::VariableDefinition { id, .. } => *id,
            Statement::If { id, .. } => *id,
        }
    }
}

impl From<Statement> for AstNode {
    fn from(value: Statement) -> Self {
        Self::Statement(value)
    }
}

#[derive(Clone, Debug)]
pub enum ExecutorHost {
    Self_ { token: Token, id: NodeID },
}

impl HasID for ExecutorHost {
    fn get_id(&self) -> NodeID {
        match self {
            ExecutorHost::Self_ { id, .. } => *id,
        }
    }
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
        id: NodeID,
    },
}

impl HasID for Executor {
    fn get_id(&self) -> NodeID {
        match self {
            Executor::Thread { id, .. } => *id,
        }
    }
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

    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,

    Equal,
    NotEqual,

    LogicalAnd,
    LogicalOr,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
    Both,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Number {
        value: i64,
        token: Token,
        id: NodeID,
    },
    FunctionCall {
        name: String,
        name_token: Token,
        open_paren_token: Token,
        // TODO: maybe store comma tokens too
        arguments: Vec<Expression>,
        close_paren_token: Token,
        id: NodeID,
    },
    Grouping {
        open_paren_token: Token,
        subexpression: Box<Expression>,
        close_paren_token: Token,
        id: NodeID,
    },
    VariableAccess {
        name: String,
        name_token: Token,
        id: NodeID,
    },
    Unary {
        operator_token: Token,
        operation: UnaryOperation,
        operand: Box<Expression>,
        id: NodeID,
    },
    Binary {
        left: Box<Expression>,
        operator_token: Token,
        operation: BinaryOperation,
        right: Box<Expression>,
        id: NodeID,
    },
    VariableAssignment {
        name: String,
        name_token: Token,
        equal_token: Token,
        right: Box<Expression>,
        id: NodeID,
    },
}

impl Expression {
    pub const TOP_PRECEDENCE: usize = usize::MAX;
    pub fn get_precedence(&self) -> usize {
        use BinaryOperation::*;
        match self {
            Expression::Number { .. } => 0,
            Expression::FunctionCall { .. } => 0,
            Expression::Grouping { .. } => 0,
            Expression::VariableAccess { .. } => 0,

            Expression::Unary { .. } => 1,
            Expression::Binary {
                operation: Multiply | Divide | Modulo,
                ..
            } => 2,
            Expression::Binary {
                operation: Add | Subtract,
                ..
            } => 3,
            Expression::Binary {
                operation: LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual,
                ..
            } => 4,
            Expression::Binary {
                operation: Equal | NotEqual,
                ..
            } => 5,
            Expression::Binary {
                operation: LogicalAnd,
                ..
            } => 6,
            Expression::Binary {
                operation: LogicalOr,
                ..
            } => 7,

            Expression::VariableAssignment { .. } => 8,
        }
    }
    pub fn get_associativity(&self) -> Associativity {
        use BinaryOperation::*;
        match self {
            Expression::Number { .. } => Associativity::Both,
            Expression::FunctionCall { .. } => Associativity::Both,
            Expression::Grouping { .. } => Associativity::Both,
            Expression::VariableAccess { .. } => Associativity::Both,

            Expression::Unary { .. } => Associativity::Left,
            Expression::Binary {
                operation: Add | Multiply,
                ..
            } => Associativity::Both,
            Expression::Binary {
                operation:
                    Subtract | Divide | Modulo | GreaterThan | GreaterThanOrEqual | LessThan
                    | LessThanOrEqual | Equal | NotEqual | LogicalAnd | LogicalOr,
                ..
            } => Associativity::Left,

            Expression::VariableAssignment { .. } => Associativity::Right,
        }
    }
}

impl HasID for Expression {
    fn get_id(&self) -> NodeID {
        match self {
            Expression::Number { id, .. } => *id,
            Expression::FunctionCall { id, .. } => *id,
            Expression::Grouping { id, .. } => *id,
            Expression::VariableAccess { id, .. } => *id,
            Expression::Unary { id, .. } => *id,
            Expression::Binary { id, .. } => *id,
            Expression::VariableAssignment { id, .. } => *id,
        }
    }
}

impl From<Expression> for AstNode {
    fn from(value: Expression) -> Self {
        Self::Expression(value)
    }
}

#[derive(Clone, Debug)]
pub struct PerNodeData<T> {
    map: HashMap<NodeID, T>,
}

impl<T> PerNodeData<T> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get_id(&self, id: NodeID) -> Option<&T> {
        self.map.get(&id)
    }
    pub fn get(&self, node: &impl HasID) -> Option<&T> {
        self.map.get(&node.get_id())
    }
    pub fn insert_id(&mut self, id: NodeID, value: T) {
        self.map.insert(id, value);
    }
    pub fn insert(&mut self, node: &impl HasID, value: T) {
        self.map.insert(node.get_id(), value);
    }
}
