use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::tokenizer::Token;

#[derive(Clone, Debug)]
pub enum AstNode {
    Program(Program),
    FunctionDefinition(FunctionDefinition),

    SimpleBinding(SimpleBinding),

    ExpressionStatement(ExpressionStatement),
    OnStatement(OnStatement),
    BlockStatement(BlockStatement),
    ReturnStatement(ReturnStatement),
    VariableDefinitionStatement(VariableDefinitionStatement),
    IfStatement(IfStatement),
    WhileLoopStatement(WhileLoopStatement),
    ForLoopStatement(ForLoopStatement),
    BreakStatement(BreakStatement),
    SkipStatement(SkipStatement),

    SelfExecutorHost(SelfExecutorHost),

    ThreadExecutor(ThreadExecutor),

    NumberExpression(NumberExpression),
    FunctionCallExpression(FunctionCallExpression),
    GroupingExpression(GroupingExpression),
    VariableAccessExpression(VariableAccessExpression),
    UnaryExpression(UnaryExpression),
    BinaryExpression(BinaryExpression),
    VariableAssignmentExpression(VariableAssignmentExpression),

    I32Type(I32Type),
    UnitType(UnitType),
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

            AstNode::SimpleBinding(simple_binding) => simple_binding.get_id(),

            AstNode::SelfExecutorHost(executor_host) => executor_host.get_id(),
            AstNode::ThreadExecutor(executor) => executor.get_id(),

            AstNode::ExpressionStatement(expression_statement) => expression_statement.get_id(),
            AstNode::OnStatement(on_statement) => on_statement.get_id(),
            AstNode::BlockStatement(block_statement) => block_statement.get_id(),
            AstNode::ReturnStatement(return_statement) => return_statement.get_id(),
            AstNode::VariableDefinitionStatement(vardef) => vardef.get_id(),
            AstNode::IfStatement(if_statement) => if_statement.get_id(),
            AstNode::WhileLoopStatement(while_loop_statement) => while_loop_statement.get_id(),
            AstNode::ForLoopStatement(for_loop_statement) => for_loop_statement.get_id(),
            AstNode::BreakStatement(break_statement) => break_statement.get_id(),
            AstNode::SkipStatement(skip_statement) => skip_statement.get_id(),

            AstNode::NumberExpression(number_expression) => number_expression.get_id(),
            AstNode::FunctionCallExpression(function_call_expression) => {
                function_call_expression.get_id()
            }
            AstNode::GroupingExpression(grouping_expression) => grouping_expression.get_id(),
            AstNode::VariableAccessExpression(variable_access_expression) => {
                variable_access_expression.get_id()
            }
            AstNode::UnaryExpression(unary_expression) => unary_expression.get_id(),
            AstNode::BinaryExpression(binary_expression) => binary_expression.get_id(),
            AstNode::VariableAssignmentExpression(variable_assignment_expression) => {
                variable_assignment_expression.get_id()
            }

            AstNode::I32Type(i32_type) => i32_type.get_id(),
            AstNode::UnitType(unit_type) => unit_type.get_id(),
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
    type ProgramOutput;
    type FunctionDefinitionOutput;
    type SimpleBindingOutput;
    type StatementOutput;
    type ExecutorHostOutput;
    type ExecutorOutput;
    type ExpressionOutput;
    type TypeOutput;

    fn visit_program(self, program: &mut Program) -> Self::ProgramOutput;
    fn visit_function_definition(
        &mut self,
        function_definition: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput;

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput;

    // statements
    fn visit_statement(&mut self, statement: &mut Statement) -> Self::StatementOutput {
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
            Statement::WhileLoop(while_loop_statement) => {
                self.visit_while_loop_statement(while_loop_statement)
            }
            Statement::ForLoop(for_loop_statement) => {
                self.visit_for_loop_statement(for_loop_statement)
            }
            Statement::Break(break_statement) => self.visit_break_statement(break_statement),
            Statement::Skip(skip_statement) => self.visit_skip_statement(skip_statement),
        }
    }
    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput;
    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput;
    fn visit_block_statement(&mut self, block: &mut BlockStatement) -> Self::StatementOutput;
    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput;
    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput;
    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput;
    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput;
    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput;
    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput;
    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput;

    // executor hosts
    fn visit_executor_host(
        &mut self,
        executor_host: &mut ExecutorHost,
    ) -> Self::ExecutorHostOutput {
        match executor_host {
            ExecutorHost::Self_(self_executor_host) => {
                self.visit_self_executor_host(self_executor_host)
            }
        }
    }
    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput;

    // executors
    fn visit_executor(&mut self, executor: &mut Executor) -> Self::ExecutorOutput {
        match executor {
            Executor::Thread(thread_executor) => self.visit_thread_executor(thread_executor),
        }
    }
    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput;

    // expressions
    fn visit_expression(&mut self, expression: &mut Expression) -> Self::ExpressionOutput {
        match expression {
            Expression::Number(number_expression) => {
                self.visit_number_expression(number_expression)
            }
            Expression::FunctionCall(function_call_expression) => {
                self.visit_function_call_expression(function_call_expression)
            }
            Expression::Grouping(grouping_expression) => {
                self.visit_grouping_expression(grouping_expression)
            }
            Expression::VariableAccess(variable_access_expression) => {
                self.visit_variable_access_expression(variable_access_expression)
            }
            Expression::Unary(unary_expression) => self.visit_unary_expression(unary_expression),
            Expression::Binary(binary_expression) => {
                self.visit_binary_expression(binary_expression)
            }
            Expression::VariableAssignment(variable_assignment_expression) => {
                self.visit_variable_assignment_expression(variable_assignment_expression)
            }
        }
    }
    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput;
    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput;
    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput;
    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput;
    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput;
    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput;
    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput;

    // types
    fn visit_type(&mut self, type_: &mut Type) -> Self::TypeOutput {
        match type_ {
            Type::I32(i32_type) => self.visit_i32_type(i32_type),
            Type::Unit(unit_type) => self.visit_unit_type(unit_type),
        }
    }

    fn visit_i32_type(&mut self, i32_type: &mut I32Type) -> Self::TypeOutput;
    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput;
}

#[derive(Clone, Debug)]
pub struct Program {
    pub functions: Vec<FunctionDefinition>,
    pub id: NodeID,
}
generate_ast_requirements!(Program, unwrap_program);

#[derive(Clone, Debug)]
pub struct SimpleBinding {
    pub name_token: Token,
    pub name: String,
    pub colon_token: Token,
    pub type_: Type,
    pub id: NodeID,
}
generate_ast_requirements!(SimpleBinding, unwrap_simple_binding);

#[derive(Clone, Debug)]
pub struct FunctionDefinition {
    pub let_token: Token,
    pub name: String,
    pub name_token: Token,
    pub equal_token: Token,
    pub open_paren_token: Token,
    pub parameters: Vec<(SimpleBinding, Option<Token>)>,
    pub close_paren_token: Token,
    pub right_arrow_token: Token,
    pub return_type: Type,
    pub body: Statement,
    pub id: NodeID,
}

generate_ast_requirements!(FunctionDefinition, unwrap_function_definition);

#[derive(Clone, Debug)]
pub struct I32Type {
    pub token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(I32Type, unwrap_i32_type);

#[derive(Clone, Debug)]
pub struct UnitType {
    pub open_paren_token: Token,
    pub close_paren_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(UnitType, unwrap_unit_type);

#[derive(Clone, Debug)]
pub enum Type {
    I32(I32Type),
    Unit(UnitType),
}

impl From<Type> for AstNode {
    fn from(value: Type) -> Self {
        match value {
            Type::I32(i32_type) => i32_type.into(),
            Type::Unit(unit_type) => unit_type.into(),
        }
    }
}

impl HasID for Type {
    fn get_id(&self) -> NodeID {
        match self {
            Type::I32(i32_type) => i32_type.get_id(),
            Type::Unit(unit_type) => unit_type.get_id(),
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
    pub value: Option<Expression>,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ReturnStatement, unwrap_return_statement);

#[derive(Clone, Debug)]
pub struct VariableDefinitionStatement {
    pub let_token: Token,
    pub binding: SimpleBinding,
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
pub struct WhileLoopStatement {
    pub while_token: Token,
    pub condition: Expression,
    pub body: Box<Statement>,
    pub id: NodeID,
}
generate_ast_requirements!(WhileLoopStatement, unwrap_while_loop_statement);

#[derive(Clone, Debug)]
pub struct ForLoopStatement {
    pub for_token: Token,
    pub open_paren_token: Token,
    pub initializer: Box<Statement>,
    pub condition: Option<Expression>,
    pub second_semicolon_token: Token,
    pub incrementer: Option<Expression>,
    pub close_paren_token: Token,
    pub body: Box<Statement>,
    pub id: NodeID,
}
generate_ast_requirements!(ForLoopStatement, unwrap_for_loop_statement);

#[derive(Clone, Debug)]
pub struct BreakStatement {
    pub break_token: Token,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(BreakStatement, unwrap_break_statement);

#[derive(Clone, Debug)]
pub struct SkipStatement {
    pub skip_token: Token,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(SkipStatement, unwrap_skip_statement);

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(ExpressionStatement),
    On(OnStatement),
    Block(BlockStatement),
    Return(ReturnStatement),
    VariableDefinition(VariableDefinitionStatement),
    If(IfStatement),
    WhileLoop(WhileLoopStatement),
    ForLoop(ForLoopStatement),
    Break(BreakStatement),
    Skip(SkipStatement),
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
            Statement::WhileLoop(while_loop_statement) => while_loop_statement.get_id(),
            Statement::ForLoop(for_loop_statement) => for_loop_statement.get_id(),
            Statement::Break(break_statement) => break_statement.get_id(),
            Statement::Skip(skip_statement) => skip_statement.get_id(),
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
            Statement::WhileLoop(while_loop_statement) => while_loop_statement.into(),
            Statement::ForLoop(for_loop_statement) => for_loop_statement.into(),
            Statement::Break(break_statement) => break_statement.into(),
            Statement::Skip(skip_statement) => skip_statement.into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SelfExecutorHost {
    pub token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(SelfExecutorHost, unwrap_self_executor_host);

#[derive(Clone, Debug)]
pub enum ExecutorHost {
    Self_(SelfExecutorHost),
}

impl HasID for ExecutorHost {
    fn get_id(&self) -> NodeID {
        match self {
            ExecutorHost::Self_(host) => host.get_id(),
        }
    }
}

impl From<ExecutorHost> for AstNode {
    fn from(value: ExecutorHost) -> Self {
        match value {
            ExecutorHost::Self_(self_executor_host) => self_executor_host.into(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ThreadExecutor {
    pub host: ExecutorHost,
    pub dot_token: Token,
    pub thread_token: Token,
    pub open_bracket_token: Token,
    pub index: Expression,
    pub close_bracket_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ThreadExecutor, unwrap_thread_executor);

#[derive(Clone, Debug)]
pub enum Executor {
    Thread(ThreadExecutor),
}

impl HasID for Executor {
    fn get_id(&self) -> NodeID {
        match self {
            Executor::Thread(executor) => executor.get_id(),
        }
    }
}

impl From<Executor> for AstNode {
    fn from(value: Executor) -> Self {
        match value {
            Executor::Thread(thread_executor) => thread_executor.into(),
        }
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
pub struct NumberExpression {
    pub value: i64,
    pub token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(NumberExpression, unwrap_number_expression);

#[derive(Clone, Debug)]
pub struct FunctionCallExpression {
    pub name: String,
    pub name_token: Token,
    pub open_paren_token: Token,
    pub arguments: Vec<(Expression, Option<Token>)>,
    pub close_paren_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(FunctionCallExpression, unwrap_function_call_expression);

#[derive(Clone, Debug)]
pub struct GroupingExpression {
    pub open_paren_token: Token,
    pub subexpression: Box<Expression>,
    pub close_paren_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(GroupingExpression, unwrap_grouping_expression);

#[derive(Clone, Debug)]
pub struct VariableAccessExpression {
    pub name: String,
    pub name_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(VariableAccessExpression, unwrap_variable_access_expression);

#[derive(Clone, Debug)]
pub struct UnaryExpression {
    pub operator_token: Token,
    pub operation: UnaryOperation,
    pub operand: Box<Expression>,
    pub id: NodeID,
}

generate_ast_requirements!(UnaryExpression, unwrap_unary_expression);

#[derive(Clone, Debug)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator_token: Token,
    pub operation: BinaryOperation,
    pub right: Box<Expression>,
    pub id: NodeID,
}
generate_ast_requirements!(BinaryExpression, unwrap_binary_expression);

#[derive(Clone, Debug)]
pub struct VariableAssignmentExpression {
    pub name: String,
    pub name_token: Token,
    pub equal_token: Token,
    pub right: Box<Expression>,
    pub id: NodeID,
}

generate_ast_requirements!(
    VariableAssignmentExpression,
    unwrap_variable_asssignment_expression
);

#[derive(Clone, Debug)]
pub enum Expression {
    Number(NumberExpression),
    FunctionCall(FunctionCallExpression),
    Grouping(GroupingExpression),
    VariableAccess(VariableAccessExpression),
    Unary(UnaryExpression),
    Binary(BinaryExpression),
    VariableAssignment(VariableAssignmentExpression),
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
            Expression::Binary(BinaryExpression {
                operation: Multiply | Divide | Modulo,
                ..
            }) => 2,
            Expression::Binary(BinaryExpression {
                operation: Add | Subtract,
                ..
            }) => 3,
            Expression::Binary(BinaryExpression {
                operation: LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual,
                ..
            }) => 4,
            Expression::Binary(BinaryExpression {
                operation: Equal | NotEqual,
                ..
            }) => 5,
            Expression::Binary(BinaryExpression {
                operation: LogicalAnd,
                ..
            }) => 6,
            Expression::Binary(BinaryExpression {
                operation: LogicalOr,
                ..
            }) => 7,

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
            Expression::Binary(BinaryExpression {
                operation: Add | Multiply,
                ..
            }) => Associativity::Both,
            Expression::Binary(BinaryExpression {
                operation:
                    Subtract | Divide | Modulo | GreaterThan | GreaterThanOrEqual | LessThan
                    | LessThanOrEqual | Equal | NotEqual | LogicalAnd | LogicalOr,
                ..
            }) => Associativity::Left,

            Expression::VariableAssignment { .. } => Associativity::Right,
        }
    }
}

impl HasID for Expression {
    fn get_id(&self) -> NodeID {
        match self {
            Expression::Number(expr) => expr.get_id(),
            Expression::FunctionCall(expr) => expr.get_id(),
            Expression::Grouping(expr) => expr.get_id(),
            Expression::VariableAccess(expr) => expr.get_id(),
            Expression::Unary(expr) => expr.get_id(),
            Expression::Binary(expr) => expr.get_id(),
            Expression::VariableAssignment(expr) => expr.get_id(),
        }
    }
}

impl From<Expression> for AstNode {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Number(number_expression) => number_expression.into(),
            Expression::FunctionCall(function_call_expression) => function_call_expression.into(),
            Expression::Grouping(grouping_expression) => grouping_expression.into(),
            Expression::VariableAccess(variable_access_expression) => {
                variable_access_expression.into()
            }
            Expression::Unary(unary_expression) => unary_expression.into(),
            Expression::Binary(binary_expression) => binary_expression.into(),
            Expression::VariableAssignment(variable_assignment_expression) => {
                variable_assignment_expression.into()
            }
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

    pub fn get_node(&self, node: &impl HasID) -> Option<&T> {
        self.map.get(&node.get_id())
    }
    pub fn insert_node(&mut self, node: &impl HasID, value: T) {
        self.map.insert(node.get_id(), value);
    }
}

impl<T> Deref for PerNodeData<T> {
    type Target = HashMap<NodeID, T>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl<T> DerefMut for PerNodeData<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}
