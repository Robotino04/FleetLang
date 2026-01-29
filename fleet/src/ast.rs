use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::{
    passes::runtime_type::RuntimeType,
    tokenizer::{FileName, Token},
};

#[derive(Clone, Debug)]
#[expect(clippy::large_enum_variant)]
pub enum AstNode {
    Program(Program),
    FunctionDefinition(FunctionDefinition),
    TypeAlias(TypeAlias),

    ExternFunctionBody(ExternFunctionBody),
    StatementFunctionBody(StatementFunctionBody),

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
    GPUExecutor(GPUExecutor),

    LiteralExpression(LiteralExpression),
    ArrayExpression(ArrayExpression),
    StructExpression(StructExpression),
    FunctionCallExpression(FunctionCallExpression),
    CompilerExpression(CompilerExpression),
    ArrayIndexExpression(ArrayIndexExpression),
    StructAccessExpression(StructAccessExpression),
    GroupingExpression(GroupingExpression),
    VariableAccessExpression(VariableAccessExpression),
    UnaryExpression(UnaryExpression),
    CastExpression(CastExpression),
    BinaryExpression(BinaryExpression),
    VariableAssignmentExpression(VariableAssignmentExpression),

    VariableLValue(VariableLValue),
    ArrayIndexLValue(ArrayIndexLValue),
    StructAccessLValue(StructAccessLValue),
    GroupingLValue(GroupingLValue),

    SimpleType(SimpleType),
    UnitType(UnitType),
    IdkType(IdkType),
    ArrayType(ArrayType),
    StructType(StructType),
    AliasType(AliasType),
}

impl AstNode {
    pub fn visit(&mut self, visitor: &mut impl AstVisitor) {
        match self {
            AstNode::Program(_program) => {
                unimplemented!("AstNode::visit isn't implemented for AstNode::Program")
            }
            AstNode::FunctionDefinition(function_definition) => {
                visitor.visit_function_definition(function_definition);
            }
            AstNode::TypeAlias(type_alias) => {
                visitor.visit_type_alias(type_alias);
            }
            AstNode::ExternFunctionBody(extern_function_body) => {
                visitor.visit_extern_function_body(extern_function_body);
            }
            AstNode::StatementFunctionBody(statement_function_body) => {
                visitor.visit_statement_function_body(statement_function_body);
            }
            AstNode::SimpleBinding(simple_binding) => {
                visitor.visit_simple_binding(simple_binding);
            }
            AstNode::ExpressionStatement(expression_statement) => {
                visitor.visit_expression_statement(expression_statement);
            }
            AstNode::OnStatement(on_statement) => {
                visitor.visit_on_statement(on_statement);
            }
            AstNode::BlockStatement(block_statement) => {
                visitor.visit_block_statement(block_statement);
            }
            AstNode::ReturnStatement(return_statement) => {
                visitor.visit_return_statement(return_statement);
            }
            AstNode::VariableDefinitionStatement(variable_definition_statement) => {
                visitor.visit_variable_definition_statement(variable_definition_statement);
            }
            AstNode::IfStatement(if_statement) => {
                visitor.visit_if_statement(if_statement);
            }
            AstNode::WhileLoopStatement(while_loop_statement) => {
                visitor.visit_while_loop_statement(while_loop_statement);
            }
            AstNode::ForLoopStatement(for_loop_statement) => {
                visitor.visit_for_loop_statement(for_loop_statement);
            }
            AstNode::BreakStatement(break_statement) => {
                visitor.visit_break_statement(break_statement);
            }
            AstNode::SkipStatement(skip_statement) => {
                visitor.visit_skip_statement(skip_statement);
            }
            AstNode::SelfExecutorHost(self_executor_host) => {
                visitor.visit_self_executor_host(self_executor_host);
            }
            AstNode::ThreadExecutor(thread_executor) => {
                visitor.visit_thread_executor(thread_executor);
            }
            AstNode::GPUExecutor(gpuexecutor) => {
                visitor.visit_gpu_executor(gpuexecutor);
            }
            AstNode::LiteralExpression(literal_expression) => {
                visitor.visit_literal_expression(literal_expression);
            }
            AstNode::ArrayExpression(array_expression) => {
                visitor.visit_array_expression(array_expression);
            }
            AstNode::StructExpression(struct_expression) => {
                visitor.visit_struct_expression(struct_expression);
            }
            AstNode::FunctionCallExpression(function_call_expression) => {
                visitor.visit_function_call_expression(function_call_expression);
            }
            AstNode::CompilerExpression(compiler_expression) => {
                visitor.visit_compiler_expression(compiler_expression);
            }
            AstNode::ArrayIndexExpression(array_index_expression) => {
                visitor.visit_array_index_expression(array_index_expression);
            }
            AstNode::StructAccessExpression(struct_access_expression) => {
                visitor.visit_struct_access_expression(struct_access_expression);
            }
            AstNode::GroupingExpression(grouping_expression) => {
                visitor.visit_grouping_expression(grouping_expression);
            }
            AstNode::VariableAccessExpression(variable_access_expression) => {
                visitor.visit_variable_access_expression(variable_access_expression);
            }
            AstNode::UnaryExpression(unary_expression) => {
                visitor.visit_unary_expression(unary_expression);
            }
            AstNode::CastExpression(cast_expression) => {
                visitor.visit_cast_expression(cast_expression);
            }
            AstNode::BinaryExpression(binary_expression) => {
                visitor.visit_binary_expression(binary_expression);
            }
            AstNode::VariableAssignmentExpression(variable_assignment_expression) => {
                visitor.visit_variable_assignment_expression(variable_assignment_expression);
            }
            AstNode::VariableLValue(variable_lvalue) => {
                visitor.visit_variable_lvalue(variable_lvalue);
            }
            AstNode::ArrayIndexLValue(array_index_lvalue) => {
                visitor.visit_array_index_lvalue(array_index_lvalue);
            }
            AstNode::StructAccessLValue(struct_access_lvalue) => {
                visitor.visit_struct_access_lvalue(struct_access_lvalue);
            }
            AstNode::GroupingLValue(grouping_lvalue) => {
                visitor.visit_grouping_lvalue(grouping_lvalue);
            }
            AstNode::SimpleType(simple_type) => {
                visitor.visit_simple_type(simple_type);
            }
            AstNode::UnitType(unit_type) => {
                visitor.visit_unit_type(unit_type);
            }
            AstNode::IdkType(idk_type) => {
                visitor.visit_idk_type(idk_type);
            }
            AstNode::ArrayType(array_type) => {
                visitor.visit_array_type(array_type);
            }
            AstNode::StructType(struct_type) => {
                visitor.visit_struct_type(struct_type);
            }
            AstNode::AliasType(alias_type) => {
                visitor.visit_alias_type(alias_type);
            }
        }
    }
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
            AstNode::TypeAlias(type_alias) => type_alias.get_id(),

            AstNode::ExternFunctionBody(extern_function_body) => extern_function_body.get_id(),
            AstNode::StatementFunctionBody(statement_function_body) => {
                statement_function_body.get_id()
            }

            AstNode::SimpleBinding(simple_binding) => simple_binding.get_id(),

            AstNode::SelfExecutorHost(executor_host) => executor_host.get_id(),
            AstNode::ThreadExecutor(executor) => executor.get_id(),
            AstNode::GPUExecutor(executor) => executor.get_id(),

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

            AstNode::LiteralExpression(literal_expression) => literal_expression.get_id(),
            AstNode::ArrayExpression(array_expression) => array_expression.get_id(),
            AstNode::StructExpression(struct_expression) => struct_expression.get_id(),
            AstNode::FunctionCallExpression(function_call_expression) => {
                function_call_expression.get_id()
            }
            AstNode::CompilerExpression(compiler_expression) => compiler_expression.get_id(),
            AstNode::ArrayIndexExpression(array_index_expression) => {
                array_index_expression.get_id()
            }
            AstNode::StructAccessExpression(struct_access_expression) => {
                struct_access_expression.get_id()
            }
            AstNode::GroupingExpression(grouping_expression) => grouping_expression.get_id(),
            AstNode::VariableAccessExpression(variable_access_expression) => {
                variable_access_expression.get_id()
            }
            AstNode::UnaryExpression(unary_expression) => unary_expression.get_id(),
            AstNode::CastExpression(cast_expression) => cast_expression.get_id(),
            AstNode::BinaryExpression(binary_expression) => binary_expression.get_id(),
            AstNode::VariableAssignmentExpression(variable_assignment_expression) => {
                variable_assignment_expression.get_id()
            }

            AstNode::VariableLValue(var_lvalue) => var_lvalue.get_id(),
            AstNode::ArrayIndexLValue(array_lvalue) => array_lvalue.get_id(),
            AstNode::StructAccessLValue(struct_lvalue) => struct_lvalue.get_id(),
            AstNode::GroupingLValue(grouping_lvalue) => grouping_lvalue.get_id(),

            AstNode::SimpleType(simple_type) => simple_type.get_id(),
            AstNode::UnitType(unit_type) => unit_type.get_id(),
            AstNode::IdkType(idk_type) => idk_type.get_id(),
            AstNode::ArrayType(array_type) => array_type.get_id(),
            AstNode::StructType(struct_type) => struct_type.get_id(),
            AstNode::AliasType(alias_type) => alias_type.get_id(),
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
    type TopLevelOutput;
    type FunctionBodyOutput;
    type SimpleBindingOutput;
    type StatementOutput;
    type ExecutorHostOutput;
    type ExecutorOutput;
    type ExpressionOutput;
    type LValueOutput;
    type TypeOutput;

    fn visit_program(self, program: &mut Program) -> Self::ProgramOutput;
    fn visit_top_level_statement(&mut self, tls: &mut TopLevelStatement) -> Self::TopLevelOutput {
        match tls {
            TopLevelStatement::Function(function_definition) => {
                self.visit_function_definition(function_definition)
            }
            TopLevelStatement::TypeAlias(type_alias) => self.visit_type_alias(type_alias),
        }
    }

    fn visit_function_definition(
        &mut self,
        function_definition: &mut FunctionDefinition,
    ) -> Self::TopLevelOutput;
    fn visit_type_alias(&mut self, type_alias: &mut TypeAlias) -> Self::TopLevelOutput;

    fn visit_function_body(
        &mut self,
        function_body: &mut FunctionBody,
    ) -> Self::FunctionBodyOutput {
        match function_body {
            FunctionBody::Statement(statement_function_body) => {
                self.visit_statement_function_body(statement_function_body)
            }
            FunctionBody::Extern(extern_function_body) => {
                self.visit_extern_function_body(extern_function_body)
            }
        }
    }
    fn visit_statement_function_body(
        &mut self,
        statement_function_body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput;
    fn visit_extern_function_body(
        &mut self,
        extern_function_body: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput;

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
            Executor::GPU(gpu_executor) => self.visit_gpu_executor(gpu_executor),
        }
    }
    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput;
    fn visit_gpu_executor(&mut self, executor: &mut GPUExecutor) -> Self::ExecutorOutput;

    // expressions
    fn visit_expression(&mut self, expression: &mut Expression) -> Self::ExpressionOutput {
        match expression {
            Expression::Literal(literal_expression) => {
                self.visit_literal_expression(literal_expression)
            }
            Expression::Array(array_expression) => self.visit_array_expression(array_expression),
            Expression::Struct(struct_expression) => {
                self.visit_struct_expression(struct_expression)
            }
            Expression::FunctionCall(function_call_expression) => {
                self.visit_function_call_expression(function_call_expression)
            }
            Expression::CompilerExpression(compiler_expression) => {
                self.visit_compiler_expression(compiler_expression)
            }
            Expression::ArrayIndex(array_index_expression) => {
                self.visit_array_index_expression(array_index_expression)
            }
            Expression::StructAccess(struct_access_expression) => {
                self.visit_struct_access_expression(struct_access_expression)
            }
            Expression::Grouping(grouping_expression) => {
                self.visit_grouping_expression(grouping_expression)
            }
            Expression::VariableAccess(variable_access_expression) => {
                self.visit_variable_access_expression(variable_access_expression)
            }
            Expression::Cast(cast_expression) => self.visit_cast_expression(cast_expression),
            Expression::Unary(unary_expression) => self.visit_unary_expression(unary_expression),
            Expression::Binary(binary_expression) => {
                self.visit_binary_expression(binary_expression)
            }
            Expression::VariableAssignment(variable_assignment_expression) => {
                self.visit_variable_assignment_expression(variable_assignment_expression)
            }
        }
    }
    fn visit_literal_expression(
        &mut self,
        expression: &mut LiteralExpression,
    ) -> Self::ExpressionOutput;
    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput;
    fn visit_struct_expression(
        &mut self,
        expression: &mut StructExpression,
    ) -> Self::ExpressionOutput;
    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput;
    fn visit_compiler_expression(
        &mut self,
        expression: &mut CompilerExpression,
    ) -> Self::ExpressionOutput;
    fn visit_array_index_expression(
        &mut self,
        expression: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput;
    fn visit_struct_access_expression(
        &mut self,
        expression: &mut StructAccessExpression,
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
    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput;
    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput;
    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput;

    fn visit_lvalue(&mut self, lvalue: &mut LValue) -> Self::LValueOutput {
        match lvalue {
            LValue::Variable(var_lvalue) => self.visit_variable_lvalue(var_lvalue),
            LValue::ArrayIndex(array_index_lvalue) => {
                self.visit_array_index_lvalue(array_index_lvalue)
            }
            LValue::StructAccess(struct_access_lvalue) => {
                self.visit_struct_access_lvalue(struct_access_lvalue)
            }
            LValue::Grouping(grouping_lvalue) => self.visit_grouping_lvalue(grouping_lvalue),
        }
    }
    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput;
    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput;
    fn visit_struct_access_lvalue(&mut self, lvalue: &mut StructAccessLValue)
    -> Self::LValueOutput;
    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput;

    // types
    fn visit_type(&mut self, type_: &mut Type) -> Self::TypeOutput {
        match type_ {
            Type::Simple(simple_type) => self.visit_simple_type(simple_type),
            Type::Unit(unit_type) => self.visit_unit_type(unit_type),
            Type::Idk(idk_type) => self.visit_idk_type(idk_type),
            Type::Array(array_type) => self.visit_array_type(array_type),
            Type::Struct(struct_type) => self.visit_struct_type(struct_type),
            Type::Alias(alias_type) => self.visit_alias_type(alias_type),
        }
    }

    fn visit_simple_type(&mut self, simple_type: &mut SimpleType) -> Self::TypeOutput;
    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput;
    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput;
    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput;
    fn visit_struct_type(&mut self, struct_type: &mut StructType) -> Self::TypeOutput;
    fn visit_alias_type(&mut self, alias_type: &mut AliasType) -> Self::TypeOutput;
}

#[derive(Clone, Debug)]
pub struct Program {
    pub top_level_statements: Vec<TopLevelStatement>,
    pub id: NodeID,

    pub file_name: FileName,
}
generate_ast_requirements!(Program, unwrap_program);

#[derive(Clone, Debug)]
#[expect(clippy::large_enum_variant)]
pub enum TopLevelStatement {
    Function(FunctionDefinition),
    TypeAlias(TypeAlias),
}

impl From<TopLevelStatement> for AstNode {
    fn from(value: TopLevelStatement) -> Self {
        match value {
            TopLevelStatement::Function(function_definition) => function_definition.into(),
            TopLevelStatement::TypeAlias(type_alias) => type_alias.into(),
        }
    }
}

impl HasID for TopLevelStatement {
    fn get_id(&self) -> NodeID {
        match self {
            TopLevelStatement::Function(function) => function.get_id(),
            TopLevelStatement::TypeAlias(alias) => alias.get_id(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SimpleBinding {
    pub name_token: Token,
    pub name: String,
    pub type_: Option<(Token, Type)>,
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
    pub return_type: Option<Type>,

    pub body: Box<FunctionBody>,

    pub id: NodeID,
}
generate_ast_requirements!(FunctionDefinition, unwrap_function_definition);

#[derive(Clone, Debug)]
pub struct TypeAlias {
    pub let_token: Token,
    pub name: String,
    pub name_token: Token,
    pub equal_token: Token,
    pub type_: Type,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(TypeAlias, unwrap_type_alias);

#[derive(Clone, Debug)]
pub struct ExternFunctionBody {
    pub at_token: Token,
    pub extern_token: Token,
    pub symbol: String,
    pub symbol_token: Token,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ExternFunctionBody, unwrap_extern_function_body);

#[derive(Clone, Debug)]
pub struct StatementFunctionBody {
    pub statement: Statement,
    pub id: NodeID,
}
generate_ast_requirements!(StatementFunctionBody, unwrap_statement_function_body);

#[derive(Clone, Debug)]
pub enum FunctionBody {
    Statement(StatementFunctionBody),
    Extern(ExternFunctionBody),
}

impl From<FunctionBody> for AstNode {
    fn from(value: FunctionBody) -> Self {
        match value {
            FunctionBody::Statement(statement) => statement.into(),
            FunctionBody::Extern(extern_function_body) => extern_function_body.into(),
        }
    }
}

impl HasID for FunctionBody {
    fn get_id(&self) -> NodeID {
        match self {
            FunctionBody::Statement(statement) => statement.get_id(),
            FunctionBody::Extern(extern_function_body) => extern_function_body.get_id(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SimpleType {
    pub token: Token,
    pub type_: RuntimeType,
    pub id: NodeID,
}

generate_ast_requirements!(SimpleType, unwrap_simple_type);

#[derive(Clone, Debug)]
pub struct UnitType {
    pub open_paren_token: Token,
    pub close_paren_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(UnitType, unwrap_unit_type);

#[derive(Clone, Debug)]
pub struct IdkType {
    pub token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(IdkType, unwrap_idk_type);

#[derive(Clone, Debug)]
pub struct ArrayType {
    pub subtype: Box<Type>,
    pub open_bracket_token: Token,
    pub size: Option<Box<Expression>>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(ArrayType, unwrap_array_type);

#[derive(Clone, Debug)]
pub struct StructMemberDefinition {
    pub name: String,
    pub name_token: Token,
    pub colon_token: Token,
    pub type_: Type,
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub struct_token: Token,
    pub open_brace_token: Token,
    pub members: Vec<(StructMemberDefinition, Option<Token>)>,
    pub close_brace_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(StructType, unwrap_struct_type);

#[derive(Clone, Debug)]
pub struct AliasType {
    pub name: String,
    pub name_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(AliasType, unwrap_alias_type);

#[derive(Clone, Debug)]
pub enum Type {
    Simple(SimpleType),
    Unit(UnitType),
    Idk(IdkType),
    Array(ArrayType),
    Struct(StructType),
    Alias(AliasType),
}

impl From<Type> for AstNode {
    fn from(value: Type) -> Self {
        match value {
            Type::Simple(simple_type) => simple_type.into(),
            Type::Unit(unit_type) => unit_type.into(),
            Type::Idk(idk_type) => idk_type.into(),
            Type::Array(array_type) => array_type.into(),
            Type::Struct(struct_type) => struct_type.into(),
            Type::Alias(alias_type) => alias_type.into(),
        }
    }
}

impl HasID for Type {
    fn get_id(&self) -> NodeID {
        match self {
            Type::Simple(simple_type) => simple_type.get_id(),
            Type::Unit(unit_type) => unit_type.get_id(),
            Type::Idk(idk_type) => idk_type.get_id(),
            Type::Array(array_type) => array_type.get_id(),
            Type::Struct(struct_type) => struct_type.get_id(),
            Type::Alias(alias_type) => alias_type.get_id(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ExpressionStatement, unwrap_expression_statement);

#[derive(Clone, Debug)]
pub struct OnStatementIterator {
    pub open_bracket_token: Token,
    pub binding: SimpleBinding,
    pub equal_token: Token,
    pub max_value: Box<Expression>,
    pub close_bracket_token: Token,
}

#[derive(Clone, Debug)]
pub struct OnStatement {
    pub on_token: Token,
    pub executor: Box<Executor>,
    pub iterators: Vec<OnStatementIterator>,
    pub open_paren_token: Token,
    pub bindings: Vec<(LValue, Option<Token>)>,
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
    pub value: Option<Box<Expression>>,
    pub semicolon_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ReturnStatement, unwrap_return_statement);

#[derive(Clone, Debug)]
pub struct VariableDefinitionStatement {
    pub let_token: Token,
    pub binding: Box<SimpleBinding>,
    pub equals_token: Token,
    pub value: Box<Expression>,
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
    pub condition: Box<Expression>,
    pub if_body: Box<Statement>,
    pub elifs: Vec<(Token, Expression, Statement)>,
    pub else_: Option<(Token, Box<Statement>)>,
    pub id: NodeID,
}
generate_ast_requirements!(IfStatement, unwrap_if_statement);

#[derive(Clone, Debug)]
pub struct WhileLoopStatement {
    pub while_token: Token,
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
    pub id: NodeID,
}
generate_ast_requirements!(WhileLoopStatement, unwrap_while_loop_statement);

#[derive(Clone, Debug)]
pub struct ForLoopStatement {
    pub for_token: Token,
    pub open_paren_token: Token,
    pub initializer: Box<Statement>,
    pub condition: Option<Box<Expression>>,
    pub second_semicolon_token: Token,
    pub incrementer: Option<Box<Expression>>,
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
    pub index: Box<Expression>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ThreadExecutor, unwrap_thread_executor);

#[derive(Clone, Debug)]
pub struct GPUExecutor {
    pub host: ExecutorHost,
    pub dot_token: Token,
    pub gpus_token: Token,
    pub open_bracket_token: Token,
    pub gpu_index: Box<Expression>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(GPUExecutor, unwrap_gpu_executor);

#[derive(Clone, Debug)]
pub enum Executor {
    Thread(ThreadExecutor),
    GPU(GPUExecutor),
}

impl HasID for Executor {
    fn get_id(&self) -> NodeID {
        match self {
            Executor::Thread(executor) => executor.get_id(),
            Executor::GPU(gpuexecutor) => gpuexecutor.get_id(),
        }
    }
}

impl From<Executor> for AstNode {
    fn from(value: Executor) -> Self {
        match value {
            Executor::Thread(thread_executor) => thread_executor.into(),
            Executor::GPU(gpuexecutor) => gpuexecutor.into(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperation {
    BitwiseNot,
    LogicalNot,
    Negate,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LiteralKind {
    Number(u64),
    Char(char),
    Float(f64),
    Bool(bool),
}
#[derive(Clone, Debug)]
pub struct LiteralExpression {
    pub value: LiteralKind,
    pub token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(LiteralExpression, unwrap_literal_expression);

#[derive(Clone, Debug)]
pub struct ArrayExpression {
    pub open_bracket_token: Token,
    pub elements: Vec<(Expression, Option<Token>)>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(ArrayExpression, unwrap_array_expression);

#[derive(Clone, Debug)]
pub struct StructMemberValue {
    pub name: String,
    pub name_token: Token,
    pub colon_token: Token,
    pub value: Box<Expression>,
}

#[derive(Clone, Debug)]
pub struct StructExpression {
    pub type_: Type,
    pub open_brace_token: Token,
    pub members: Vec<(StructMemberValue, Option<Token>)>,
    pub close_brace_token: Token,
    pub id: NodeID,
}
generate_ast_requirements!(StructExpression, unwrap_struct_expression);

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
pub struct CompilerExpression {
    pub at_token: Token,
    pub name: String,
    pub name_token: Token,
    pub open_paren_token: Token,
    pub arguments: Vec<(Expression, Option<Token>)>,
    pub close_paren_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(CompilerExpression, unwrap_compiler_expression);

#[derive(Clone, Debug)]
pub struct ArrayIndexExpression {
    pub array: Box<Expression>,
    pub open_bracket_token: Token,
    pub index: Box<Expression>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(ArrayIndexExpression, unwrap_array_index_expression);

#[derive(Clone, Debug)]
pub struct StructAccessExpression {
    pub value: Box<Expression>,
    pub dot_token: Token,
    pub member_name: String,
    pub member_name_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(StructAccessExpression, unwrap_struct_access_expression);

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
pub struct CastExpression {
    pub operand: Box<Expression>,
    pub as_token: Token,
    pub type_: Type,
    pub id: NodeID,
}

generate_ast_requirements!(CastExpression, unwrap_cast_expression);

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
    pub lvalue: LValue,
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
    Literal(LiteralExpression),
    Array(ArrayExpression),
    Struct(StructExpression),
    FunctionCall(FunctionCallExpression),
    CompilerExpression(CompilerExpression),
    ArrayIndex(ArrayIndexExpression),
    StructAccess(StructAccessExpression),
    Grouping(GroupingExpression),
    VariableAccess(VariableAccessExpression),
    Unary(UnaryExpression),
    Cast(CastExpression),
    Binary(BinaryExpression),
    VariableAssignment(VariableAssignmentExpression),
}

impl Expression {
    pub const TOP_PRECEDENCE: usize = usize::MAX;
    pub fn get_precedence(&self) -> usize {
        use BinaryOperation::*;
        match self {
            Expression::Literal { .. } => 0,
            Expression::VariableAccess { .. } => 0,
            Expression::Grouping { .. } => 0,

            Expression::Array { .. } => 0,
            Expression::Struct { .. } => 0,

            Expression::FunctionCall { .. } => 0,
            Expression::CompilerExpression { .. } => 0,

            Expression::ArrayIndex { .. } => 1,
            Expression::StructAccess { .. } => 1,

            Expression::Unary { .. } => 2,
            Expression::Cast { .. } => 3,
            Expression::Binary(BinaryExpression {
                operation: Multiply | Divide | Modulo,
                ..
            }) => 4,
            Expression::Binary(BinaryExpression {
                operation: Add | Subtract,
                ..
            }) => 5,
            Expression::Binary(BinaryExpression {
                operation: LessThan | LessThanOrEqual | GreaterThan | GreaterThanOrEqual,
                ..
            }) => 6,
            Expression::Binary(BinaryExpression {
                operation: Equal | NotEqual,
                ..
            }) => 7,
            Expression::Binary(BinaryExpression {
                operation: LogicalAnd,
                ..
            }) => 8,
            Expression::Binary(BinaryExpression {
                operation: LogicalOr,
                ..
            }) => 9,

            Expression::VariableAssignment { .. } => 10,
        }
    }
    pub fn get_associativity(&self) -> Associativity {
        use BinaryOperation::*;
        match self {
            Expression::Literal { .. } => Associativity::Both,
            Expression::Array { .. } => Associativity::Both,
            Expression::ArrayIndex { .. } => Associativity::Left,
            Expression::StructAccess { .. } => Associativity::Left,
            Expression::Struct { .. } => Associativity::Left,
            Expression::FunctionCall { .. } => Associativity::Both,
            Expression::CompilerExpression { .. } => Associativity::Both,
            Expression::Grouping { .. } => Associativity::Both,
            Expression::VariableAccess { .. } => Associativity::Both,

            Expression::Unary { .. } => Associativity::Left,
            Expression::Cast { .. } => Associativity::Left,
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
            Expression::Literal(expr) => expr.get_id(),
            Expression::Array(expr) => expr.get_id(),
            Expression::Struct(expr) => expr.get_id(),
            Expression::FunctionCall(expr) => expr.get_id(),
            Expression::CompilerExpression(expr) => expr.get_id(),
            Expression::ArrayIndex(expr) => expr.get_id(),
            Expression::StructAccess(expr) => expr.get_id(),
            Expression::Grouping(expr) => expr.get_id(),
            Expression::VariableAccess(expr) => expr.get_id(),
            Expression::Unary(expr) => expr.get_id(),
            Expression::Cast(expr) => expr.get_id(),
            Expression::Binary(expr) => expr.get_id(),
            Expression::VariableAssignment(expr) => expr.get_id(),
        }
    }
}

impl From<Expression> for AstNode {
    fn from(value: Expression) -> Self {
        match value {
            Expression::Literal(literal_expression) => literal_expression.into(),
            Expression::Array(array_expression) => array_expression.into(),
            Expression::Struct(struct_expression) => struct_expression.into(),
            Expression::FunctionCall(function_call_expression) => function_call_expression.into(),
            Expression::CompilerExpression(compiler_expression) => compiler_expression.into(),
            Expression::ArrayIndex(array_index_expression) => array_index_expression.into(),
            Expression::StructAccess(struct_access_expression) => struct_access_expression.into(),
            Expression::Grouping(grouping_expression) => grouping_expression.into(),
            Expression::VariableAccess(variable_access_expression) => {
                variable_access_expression.into()
            }
            Expression::Unary(unary_expression) => unary_expression.into(),
            Expression::Cast(cast_expression) => cast_expression.into(),
            Expression::Binary(binary_expression) => binary_expression.into(),
            Expression::VariableAssignment(variable_assignment_expression) => {
                variable_assignment_expression.into()
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableLValue {
    pub name: String,
    pub name_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(VariableLValue, unwrap_variable_lvalue);

#[derive(Clone, Debug)]
pub struct ArrayIndexLValue {
    pub array: Box<LValue>,
    pub open_bracket_token: Token,
    pub index: Box<Expression>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(ArrayIndexLValue, unwrap_array_index_lvalue);

#[derive(Clone, Debug)]
pub struct StructAccessLValue {
    pub value: Box<LValue>,
    pub dot_token: Token,
    pub member_name: String,
    pub member_name_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(StructAccessLValue, unwrap_struct_access_lvalue);

#[derive(Clone, Debug)]
pub struct GroupingLValue {
    pub open_paren_token: Token,
    pub sublvalue: Box<LValue>,
    pub close_paren_token: Token,
    pub id: NodeID,
}

generate_ast_requirements!(GroupingLValue, unwrap_grouping_lvalue);

#[derive(Clone, Debug)]
pub enum LValue {
    Variable(VariableLValue),
    ArrayIndex(ArrayIndexLValue),
    StructAccess(StructAccessLValue),
    Grouping(GroupingLValue),
}

impl LValue {
    pub const TOP_PRECEDENCE: usize = usize::MAX;
    pub fn get_precedence(&self) -> usize {
        match self {
            LValue::Variable(..) => 0,
            LValue::ArrayIndex(..) => 0,
            LValue::StructAccess(..) => 0,
            LValue::Grouping(..) => 0,
        }
    }
    pub fn get_associativity(&self) -> Associativity {
        match self {
            LValue::Variable(..) => Associativity::Both,
            LValue::ArrayIndex(..) => Associativity::Left,
            LValue::StructAccess(..) => Associativity::Left,
            LValue::Grouping(..) => Associativity::Both,
        }
    }
}
impl HasID for LValue {
    fn get_id(&self) -> NodeID {
        match self {
            LValue::Variable(variable_lvalue) => variable_lvalue.get_id(),
            LValue::ArrayIndex(array_index_lvalue) => array_index_lvalue.get_id(),
            LValue::StructAccess(struct_access_lvalue) => struct_access_lvalue.get_id(),
            LValue::Grouping(grouping_lvalue) => grouping_lvalue.get_id(),
        }
    }
}

impl From<LValue> for AstNode {
    fn from(value: LValue) -> Self {
        match value {
            LValue::Variable(variable_lvalue) => variable_lvalue.into(),
            LValue::ArrayIndex(array_index_lvalue) => array_index_lvalue.into(),
            LValue::StructAccess(struct_access_lvalue) => struct_access_lvalue.into(),
            LValue::Grouping(grouping_lvalue) => grouping_lvalue.into(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PerNodeData<T> {
    map: HashMap<NodeID, T>,
}

impl<T> Default for PerNodeData<T> {
    fn default() -> Self {
        Self {
            map: HashMap::new(),
        }
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
