use std::collections::HashMap;

use crate::tokenizer::Token;

#[derive(Clone, Debug)]
pub enum AstNode {
    Program(Program),
    FunctionDefinition(FunctionDefinition),

    ExpressionStatement(ExpressionStatement),
    OnStatement(OnStatement),
    BlockStatement(BlockStatement),
    ReturnStatement(ReturnStatement),
    VariableDefinitionStatement(VariableDefinitionStatement),
    IfStatement(IfStatement),

    ExecutorHost(ExecutorHost),
    Executor(Executor),
    Expression(Expression),
    Type(Type),
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct NodeID(pub u64);

pub trait HasID {
    fn get_id(&self) -> NodeID;
}

impl HasID for AstNode {
    fn get_id(&self) -> NodeID {
        match self {
            AstNode::Program(program) => program.get_id(),
            AstNode::FunctionDefinition(function_definition) => function_definition.get_id(),
            AstNode::ExecutorHost(executor_host) => executor_host.get_id(),
            AstNode::Executor(executor) => executor.get_id(),
            AstNode::Expression(expression) => expression.get_id(),
            AstNode::Type(type_) => type_.get_id(),

            AstNode::ExpressionStatement(expression_statement) => expression_statement.get_id(),
            AstNode::OnStatement(on_statement) => on_statement.get_id(),
            AstNode::BlockStatement(block_statement) => block_statement.get_id(),
            AstNode::ReturnStatement(return_statement) => return_statement.get_id(),
            AstNode::VariableDefinitionStatement(vardef) => vardef.get_id(),
            AstNode::IfStatement(if_statement) => if_statement.get_id(),
        }
    }
}
macro_rules! generate_ast_requirements {
    ($Self:tt, $unwrap_name:ident) => {
        impl AstNode {
            pub fn $unwrap_name(self) -> $Self {
                if let AstNode::$Self(contents) = self {
                    contents
                } else {
                    panic!("Expected AstNode::{}, found {:#?}", stringify!($Self), self)
                }
            }
        }

        impl HasID for $Self {
            fn get_id(&self) -> NodeID {
                self.id
            }
        }

        impl From<$Self> for AstNode {
            fn from(value: $Self) -> Self {
                Self::$Self(value)
            }
        }
    };
}

pub trait AstVisitor {
    type SubOutput;
    type Output;

    fn visit_program(self, program: &mut Program) -> Self::Output;
    fn visit_function_definition(
        &mut self,
        function_definition: &mut FunctionDefinition,
    ) -> Self::SubOutput;

    fn visit_statement(&mut self, statement: &mut Statement) -> Self::SubOutput {
        match statement {
            Statement::Expression(expression_statement) => {
                self.visit_expression_statement(expression_statement)
            }
            Statement::On(on_statement) => self.visit_on_statement(on_statement),
            Statement::Block(block_statement) => self.visit_block_statement(block_statement),
            Statement::Return(return_statement) => self.visit_return_statement(return_statement),
            Statement::VariableDefinition(variable_definition_statement) => {
                self.visit_variable_definition_statement(variable_definition_statement)
            }
            Statement::If(if_statement) => self.visit_if_statement(if_statement),
        }
    }
    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::SubOutput;
    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::SubOutput;
    fn visit_block_statement(&mut self, block: &mut BlockStatement) -> Self::SubOutput;
    fn visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) -> Self::SubOutput;
    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::SubOutput;
    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::SubOutput;

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) -> Self::SubOutput;
    fn visit_executor(&mut self, executor: &mut Executor) -> Self::SubOutput;
    fn visit_expression(&mut self, expression: &mut Expression) -> Self::SubOutput;
    fn visit_type(&mut self, type_: &mut Type) -> Self::SubOutput;
}

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
    pub id: NodeID,
}
generate_ast_requirements!(Program, unwrap_program);

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

generate_ast_requirements!(FunctionDefinition, unwrap_function_definition);

#[derive(Clone, Debug)]
pub enum Type {
    I32 { token: Token, id: NodeID },
}
//generate_ast_requirements!(Type, unwrap_type);
impl AstNode {
    pub fn unwrap_type(self) -> Type {
        if let AstNode::Type(contents) = self {
            contents
        } else {
            panic!("Expected AstNode::{}, found {:#?}", stringify!(Type), self)
        }
    }
}

impl From<Type> for AstNode {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}

impl HasID for Type {
    fn get_id(&self) -> NodeID {
        match self {
            Type::I32 { token: _, id } => *id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ExpressionStatement, unwrap_expression_statement);

#[derive(Clone, Debug)]
pub struct OnStatement {
    pub on_token: Token,
    pub open_paren_token: Token,
    pub executor: Executor,
    pub close_paren_token: Token,
    pub body: Box<Statement>,
    pub id: NodeID,
}
generate_ast_requirements!(OnStatement, unwrap_or_statement);

#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub open_brace_token: Token,
    pub body: Vec<Statement>,
    pub close_brace_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(BlockStatement, unwrap_block_statement);

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub return_token: Token,
    pub value: Expression,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ReturnStatement, unwrap_return_statement);

#[derive(Clone, Debug)]
pub struct VariableDefinitionStatement {
    pub let_token: Token,
    pub name_token: Token,
    pub name: String,
    pub colon_token: Token,
    pub type_: Type,
    pub equals_token: Token,
    pub value: Expression,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(
    VariableDefinitionStatement,
    unwrap_variable_definition_statement
);

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub if_token: Token,
    pub condition: Expression,
    pub if_body: Box<Statement>,
    pub elifs: Vec<(Token, Expression, Statement)>,
    pub else_: Option<(Token, Box<Statement>)>,
    pub id: NodeID,
}
generate_ast_requirements!(IfStatement, unwrap_if_statement);

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(ExpressionStatement),
    On(OnStatement),
    Block(BlockStatement),
    Return(ReturnStatement),
    VariableDefinition(VariableDefinitionStatement),
    If(IfStatement),
}

impl HasID for Statement {
    fn get_id(&self) -> NodeID {
        match self {
            Statement::Expression(exp) => exp.get_id(),
            Statement::On(on) => on.get_id(),
            Statement::Block(block) => block.get_id(),
            Statement::Return(return_) => return_.get_id(),
            Statement::VariableDefinition(vardef) => vardef.get_id(),
            Statement::If(if_) => if_.get_id(),
        }
    }
}

impl From<Statement> for AstNode {
    fn from(value: Statement) -> Self {
        match value {
            Statement::Expression(exp) => exp.into(),
            Statement::On(on) => on.into(),
            Statement::Block(block) => block.into(),
            Statement::Return(return_) => return_.into(),
            Statement::VariableDefinition(vardef) => vardef.into(),
            Statement::If(if_) => if_.into(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExecutorHost {
    Self_ { token: Token, id: NodeID },
}
impl AstNode {
    pub fn unwrap_executor_host(self) -> ExecutorHost {
        if let AstNode::ExecutorHost(contents) = self {
            contents
        } else {
            panic!(
                "Expected AstNode::{}, found {:#?}",
                stringify!(ExecutorHost),
                self
            )
        }
    }
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
impl AstNode {
    pub fn unwrap_executor(self) -> Executor {
        if let AstNode::Executor(contents) = self {
            contents
        } else {
            panic!(
                "Expected AstNode::{}, found {:#?}",
                stringify!(Executor),
                self
            )
        }
    }
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
impl AstNode {
    pub fn unwrap_expression(self) -> Expression {
        if let AstNode::Expression(contents) = self {
            contents
        } else {
            panic!(
                "Expected AstNode::{}, found {:#?}",
                stringify!(Expression),
                self
            )
        }
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
