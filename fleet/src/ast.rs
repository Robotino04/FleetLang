use std::{
    collections::HashMap,
    fmt::Debug,
    ops::{Deref, DerefMut},
};

use crate::{
    passes::runtime_type::RuntimeType,
    tokenizer::{FileName, NamedSourceRange, SourceRange, Token},
};

#[derive(Clone, Debug)]
#[expect(clippy::large_enum_variant)]
pub enum AstNode {
    Program(Program),
    TopLevelStatement(TopLevelStatement),
    FunctionBody(FunctionBody),
    SimpleBinding(SimpleBinding),
    Statement(Statement),
    ExecutorHost(ExecutorHost),
    Executor(Executor),
    Expression(Expression),
    LValue(LValue),
    Type(Type),
}

#[derive(Copy, Clone, Debug)]
pub enum AstNodeRef<'a> {
    Program(&'a Program),
    TopLevelStatement(&'a TopLevelStatement),
    FunctionBody(&'a FunctionBody),
    SimpleBinding(&'a SimpleBinding),
    Statement(&'a Statement),
    ExecutorHost(&'a ExecutorHost),
    Executor(&'a Executor),
    Expression(&'a Expression),
    LValue(&'a LValue),
    Type(&'a Type),
}

#[derive(Debug)]
pub enum AstNodeRefMut<'a> {
    Program(&'a mut Program),
    TopLevelStatement(&'a mut TopLevelStatement),
    FunctionBody(&'a mut FunctionBody),
    SimpleBinding(&'a mut SimpleBinding),
    Statement(&'a mut Statement),
    ExecutorHost(&'a mut ExecutorHost),
    Executor(&'a mut Executor),
    Expression(&'a mut Expression),
    LValue(&'a mut LValue),
    Type(&'a mut Type),
}

impl<'a> From<&'a mut AstNode> for AstNodeRef<'a> {
    fn from(value: &'a mut AstNode) -> Self {
        (&*value).into()
    }
}

impl<'a> From<&'a AstNode> for AstNodeRef<'a> {
    fn from(value: &'a AstNode) -> Self {
        match value {
            AstNode::Program(program) => program.into(),
            AstNode::TopLevelStatement(top_level_statement) => top_level_statement.into(),
            AstNode::FunctionBody(function_body) => function_body.into(),
            AstNode::SimpleBinding(simple_binding) => simple_binding.into(),
            AstNode::Statement(statement) => statement.into(),
            AstNode::ExecutorHost(executor_host) => executor_host.into(),
            AstNode::Executor(executor) => executor.into(),
            AstNode::Expression(expression) => expression.into(),
            AstNode::LValue(lvalue) => lvalue.into(),
            AstNode::Type(ty) => ty.into(),
        }
    }
}

impl<'a> From<&'a mut AstNode> for AstNodeRefMut<'a> {
    fn from(value: &'a mut AstNode) -> Self {
        match value {
            AstNode::Program(program) => program.into(),
            AstNode::TopLevelStatement(top_level_statement) => top_level_statement.into(),
            AstNode::FunctionBody(function_body) => function_body.into(),
            AstNode::SimpleBinding(simple_binding) => simple_binding.into(),
            AstNode::Statement(statement) => statement.into(),
            AstNode::ExecutorHost(executor_host) => executor_host.into(),
            AstNode::Executor(executor) => executor.into(),
            AstNode::Expression(expression) => expression.into(),
            AstNode::LValue(lvalue) => lvalue.into(),
            AstNode::Type(ty) => ty.into(),
        }
    }
}

impl<'a> From<AstNodeRefMut<'a>> for AstNodeRef<'a> {
    fn from(value: AstNodeRefMut<'a>) -> Self {
        match value {
            AstNodeRefMut::Program(program) => AstNodeRef::Program(program),
            AstNodeRefMut::TopLevelStatement(top_level_statement) => {
                AstNodeRef::TopLevelStatement(top_level_statement)
            }
            AstNodeRefMut::FunctionBody(function_body) => AstNodeRef::FunctionBody(function_body),
            AstNodeRefMut::SimpleBinding(simple_binding) => {
                AstNodeRef::SimpleBinding(simple_binding)
            }
            AstNodeRefMut::Statement(statement) => AstNodeRef::Statement(statement),
            AstNodeRefMut::ExecutorHost(executor_host) => AstNodeRef::ExecutorHost(executor_host),
            AstNodeRefMut::Executor(executor) => AstNodeRef::Executor(executor),
            AstNodeRefMut::Expression(expression) => AstNodeRef::Expression(expression),
            AstNodeRefMut::LValue(lvalue) => AstNodeRef::LValue(lvalue),
            AstNodeRefMut::Type(ty) => AstNodeRef::Type(ty),
        }
    }
}
impl<'a, 'b> From<&'b AstNodeRefMut<'a>> for AstNodeRef<'a>
where
    'b: 'a,
{
    fn from(value: &'b AstNodeRefMut<'a>) -> Self {
        match value {
            AstNodeRefMut::Program(program) => AstNodeRef::Program(program),
            AstNodeRefMut::TopLevelStatement(top_level_statement) => {
                AstNodeRef::TopLevelStatement(top_level_statement)
            }
            AstNodeRefMut::FunctionBody(function_body) => AstNodeRef::FunctionBody(function_body),
            AstNodeRefMut::SimpleBinding(simple_binding) => {
                AstNodeRef::SimpleBinding(simple_binding)
            }
            AstNodeRefMut::Statement(statement) => AstNodeRef::Statement(statement),
            AstNodeRefMut::ExecutorHost(executor_host) => AstNodeRef::ExecutorHost(executor_host),
            AstNodeRefMut::Executor(executor) => AstNodeRef::Executor(executor),
            AstNodeRefMut::Expression(expression) => AstNodeRef::Expression(expression),
            AstNodeRefMut::LValue(lvalue) => AstNodeRef::LValue(lvalue),
            AstNodeRefMut::Type(ty) => AstNodeRef::Type(ty),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct NodeID(pub u64);

pub trait HasSourceRange {
    fn get_source_range(&self) -> NamedSourceRange;
}

impl HasSourceRange for AstNode {
    fn get_source_range(&self) -> NamedSourceRange {
        AstNodeRef::from(self).get_source_range()
    }
}

impl<T> HasSourceRange for Box<T>
where
    T: HasSourceRange,
{
    fn get_source_range(&self) -> NamedSourceRange {
        self.as_ref().get_source_range()
    }
}

impl<T> HasSourceRange for &T
where
    T: HasSourceRange,
{
    fn get_source_range(&self) -> NamedSourceRange {
        (*self).get_source_range()
    }
}
impl<T> HasSourceRange for &mut T
where
    T: HasSourceRange,
{
    fn get_source_range(&self) -> NamedSourceRange {
        (**self).get_source_range()
    }
}

impl<'a> HasSourceRange for AstNodeRef<'a> {
    fn get_source_range(&self) -> NamedSourceRange {
        match self {
            AstNodeRef::Program(program) => program.get_source_range(),
            AstNodeRef::TopLevelStatement(top_level_statement) => {
                top_level_statement.get_source_range()
            }
            AstNodeRef::FunctionBody(function_body) => function_body.get_source_range(),
            AstNodeRef::SimpleBinding(simple_binding) => simple_binding.get_source_range(),
            AstNodeRef::Statement(statement) => statement.get_source_range(),
            AstNodeRef::ExecutorHost(executor_host) => executor_host.get_source_range(),
            AstNodeRef::Executor(executor) => executor.get_source_range(),
            AstNodeRef::Expression(expression) => expression.get_source_range(),
            AstNodeRef::LValue(lvalue) => lvalue.get_source_range(),
            AstNodeRef::Type(ty) => ty.get_source_range(),
        }
    }
}

impl<'a> HasSourceRange for AstNodeRefMut<'a> {
    fn get_source_range(&self) -> NamedSourceRange {
        AstNodeRef::from(self).get_source_range()
    }
}

pub trait HasID {
    fn get_id(&self) -> NodeID;
}

impl HasID for NodeID {
    fn get_id(&self) -> NodeID {
        *self
    }
}

impl HasID for AstNode {
    fn get_id(&self) -> NodeID {
        AstNodeRef::from(self).get_id()
    }
}

impl<'a> HasID for AstNodeRef<'a> {
    fn get_id(&self) -> NodeID {
        match self {
            AstNodeRef::Program(program) => program.get_id(),
            AstNodeRef::TopLevelStatement(top_level_statement) => top_level_statement.get_id(),
            AstNodeRef::FunctionBody(function_body) => function_body.get_id(),
            AstNodeRef::SimpleBinding(simple_binding) => simple_binding.get_id(),
            AstNodeRef::Statement(statement) => statement.get_id(),
            AstNodeRef::ExecutorHost(executor_host) => executor_host.get_id(),
            AstNodeRef::Executor(executor) => executor.get_id(),
            AstNodeRef::Expression(expression) => expression.get_id(),
            AstNodeRef::LValue(lvalue) => lvalue.get_id(),
            AstNodeRef::Type(ty) => ty.get_id(),
        }
    }
}

impl<'a> HasID for AstNodeRefMut<'a> {
    fn get_id(&self) -> NodeID {
        AstNodeRef::from(self).get_id()
    }
}

macro_rules! impl_enum_node {
    {
        $(
            #[$($attr:tt)*]
        )*
        $visib:vis enum $Self:ident {
            $(
                $variant:ident($Subtype:ty),
            )*
        }
    } => {
        $(
            #[$($attr)*]
        )*
        $visib enum $Self {
            $(
                $variant($Subtype),
            )*
        }

        $(
            impl From<$Subtype> for $Self {
                fn from(value: $Subtype) -> $Self {
                    $Self::$variant(value)
                }
            }
        )*

        $(
            impl From<$Subtype> for AstNode {
                fn from(value: $Subtype) -> AstNode {
                    AstNode::$Self($Self::$variant(value))
                }
            }
        )*

        impl HasID for $Self {
            fn get_id(&self) -> NodeID {
                match self {
                    $(
                        $Self::$variant (value) => value.get_id(),
                    )*
                }
            }
        }

        impl HasSourceRange for $Self {
            fn get_source_range(&self) -> NamedSourceRange {
                match self {
                    $(
                        $Self::$variant (value) => value.get_source_range(),
                    )*
                }
            }
        }

        generate_ast_requirements!($Self);
    };
}
macro_rules! impl_struct_node {
    { $Self:tt, $unwrap_name:ident} => {
        impl HasID for $Self {
            fn get_id(&self) -> NodeID {
                self.id
            }
        }

        generate_ast_requirements!($Self);
    }
}

macro_rules! impl_enum_variant {
    { $Self:tt } => {
        impl HasID for $Self {
            fn get_id(&self) -> NodeID {
                self.id
            }
        }
    }
}

macro_rules! generate_ast_requirements {
    ($Self:tt) => {
        impl From<$Self> for AstNode {
            fn from(value: $Self) -> Self {
                AstNode::$Self(value)
            }
        }

        impl From<Box<$Self>> for AstNode {
            fn from(value: Box<$Self>) -> Self {
                (*value).into()
            }
        }

        impl<'a> From<&'a $Self> for AstNodeRef<'a> {
            fn from(value: &'a $Self) -> Self {
                AstNodeRef::$Self(value)
            }
        }

        impl<'a> From<&'a mut $Self> for AstNodeRef<'a> {
            fn from(value: &'a mut $Self) -> Self {
                (&*value).into()
            }
        }

        impl<'a> From<&'a Box<$Self>> for AstNodeRef<'a> {
            fn from(value: &'a Box<$Self>) -> Self {
                value.as_ref().into()
            }
        }

        impl<'a> From<&'a mut Box<$Self>> for AstNodeRef<'a> {
            fn from(value: &'a mut Box<$Self>) -> Self {
                AstNodeRef::$Self(value)
            }
        }

        impl<'a> From<&'a mut $Self> for AstNodeRefMut<'a> {
            fn from(value: &'a mut $Self) -> Self {
                AstNodeRefMut::$Self(value)
            }
        }

        impl<'a> From<&'a mut Box<$Self>> for AstNodeRefMut<'a> {
            fn from(value: &'a mut Box<$Self>) -> Self {
                value.as_mut().into()
            }
        }

        // impl AstNode {
        //     pub fn $unwrap_name(self) -> $Self {
        //         if let AstNode::$Self(contents) = self {
        //             contents
        //         } else {
        //             panic!("Expected AstNode::{}, found {:#?}", stringify!($Self), self)
        //         }
        //     }
        // }
        //
        // impl<'a> AstNodeRef<'a> {
        //     pub fn $unwrap_name(self) -> &'a $Self {
        //         if let AstNodeRef::$Self(contents) = self {
        //             contents
        //         } else {
        //             panic!(
        //                 "Expected AstNodeRef::{}, found {:#?}",
        //                 stringify!($Self),
        //                 self
        //             )
        //         }
        //     }
        // }
        //
        // impl<'a> AstNodeRefMut<'a> {
        //     pub fn $unwrap_name(self) -> &'a $Self {
        //         if let AstNodeRefMut::$Self(contents) = self {
        //             contents
        //         } else {
        //             panic!(
        //                 "Expected AstNodeRefMut::{}, found {:#?}",
        //                 stringify!($Self),
        //                 self
        //             )
        //         }
        //     }
        // }
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
            TopLevelStatement::FunctionDefinition(function_definition) => {
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
impl_struct_node!(Program, unwrap_program);

impl HasSourceRange for Program {
    fn get_source_range(&self) -> NamedSourceRange {
        let Program {
            top_level_statements,
            id: _,
            file_name,
        } = self;
        top_level_statements
            .iter()
            .map(HasSourceRange::get_source_range)
            .reduce(|a, b| a.extend_with(b))
            .unwrap_or(SourceRange::empty_start().named(file_name.clone()))
    }
}

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
impl_enum_variant!(FunctionDefinition);

impl HasSourceRange for FunctionDefinition {
    fn get_source_range(&self) -> NamedSourceRange {
        let FunctionDefinition {
            let_token,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters: _,
            close_paren_token: _,
            right_arrow_token: _,
            return_type: _,
            body,
            id: _,
        } = self;
        let_token.range.clone().extend_with(body.get_source_range())
    }
}

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
impl_enum_variant!(TypeAlias);

impl HasSourceRange for TypeAlias {
    fn get_source_range(&self) -> NamedSourceRange {
        let TypeAlias {
            let_token,
            name: _,
            name_token: _,
            equal_token: _,
            type_: _,
            semicolon_token,
            id: _,
        } = self;
        let_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone())
    }
}

impl_enum_node! {
    #[derive(Clone, Debug)]
    #[expect(clippy::large_enum_variant)]
    pub enum TopLevelStatement {
        FunctionDefinition(FunctionDefinition),
        TypeAlias(TypeAlias),
    }
}

#[derive(Clone, Debug)]
pub struct SimpleBinding {
    pub name_token: Token,
    pub name: String,
    pub type_: Option<(Token, Type)>,
    pub id: NodeID,
}
impl_struct_node!(SimpleBinding, unwrap_simple_binding);

impl HasSourceRange for SimpleBinding {
    fn get_source_range(&self) -> NamedSourceRange {
        let SimpleBinding {
            name_token,
            name: _,
            type_,
            id: _,
        } = self;

        name_token.range.clone().maybe_extend(
            type_
                .as_ref()
                .map(|(_colon, type_)| type_.get_source_range()),
        )
    }
}

#[derive(Clone, Debug)]
pub struct ExternFunctionBody {
    pub at_token: Token,
    pub extern_token: Token,
    pub symbol: String,
    pub symbol_token: Token,
    pub semicolon_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(ExternFunctionBody);

impl HasSourceRange for ExternFunctionBody {
    fn get_source_range(&self) -> NamedSourceRange {
        let ExternFunctionBody {
            at_token,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token,
            id: _,
        } = self;
        at_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct StatementFunctionBody {
    pub statement: Statement,
    pub id: NodeID,
}
impl_enum_variant!(StatementFunctionBody);

impl HasSourceRange for StatementFunctionBody {
    fn get_source_range(&self) -> NamedSourceRange {
        let StatementFunctionBody { statement, id: _ } = self;

        statement.get_source_range()
    }
}

impl_enum_node! {
    #[derive(Clone, Debug)]
    pub enum FunctionBody {
        Statement(StatementFunctionBody),
        Extern(ExternFunctionBody),
    }
}

#[derive(Clone, Debug)]
pub struct SimpleType {
    pub token: Token,
    pub type_: RuntimeType,
    pub id: NodeID,
}

impl_enum_variant!(SimpleType);

impl HasSourceRange for SimpleType {
    fn get_source_range(&self) -> NamedSourceRange {
        let SimpleType {
            token,
            type_: _,
            id: _,
        } = self;
        token.range.clone().extend_with(token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct UnitType {
    pub open_paren_token: Token,
    pub close_paren_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(UnitType);

impl HasSourceRange for UnitType {
    fn get_source_range(&self) -> NamedSourceRange {
        let UnitType {
            open_paren_token,
            close_paren_token,
            id: _,
        } = self;
        open_paren_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct IdkType {
    pub token: Token,
    pub id: NodeID,
}

impl_enum_variant!(IdkType);

impl HasSourceRange for IdkType {
    fn get_source_range(&self) -> NamedSourceRange {
        let IdkType { token, id: _ } = self;
        token.range.clone()
    }
}

#[derive(Clone, Debug)]
pub struct ArrayType {
    pub subtype: Box<Type>,
    pub open_bracket_token: Token,
    pub size: Option<Box<Expression>>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(ArrayType);

impl HasSourceRange for ArrayType {
    fn get_source_range(&self) -> NamedSourceRange {
        let ArrayType {
            subtype,
            open_bracket_token: _,
            size: _,
            close_bracket_token,
            id: _,
        } = self;
        close_bracket_token
            .range
            .clone()
            .extend_with(subtype.get_source_range())
    }
}

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

impl_enum_variant!(StructType);

impl HasSourceRange for StructType {
    fn get_source_range(&self) -> NamedSourceRange {
        let StructType {
            struct_token,
            open_brace_token: _,
            members: _,
            close_brace_token,
            id: _,
        } = self;
        struct_token
            .range
            .clone()
            .extend_with(close_brace_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct AliasType {
    pub name: String,
    pub name_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(AliasType);

impl HasSourceRange for AliasType {
    fn get_source_range(&self) -> NamedSourceRange {
        let AliasType {
            name: _,
            name_token,
            id: _,
        } = self;
        name_token.range.clone()
    }
}

impl_enum_node! {
    #[derive(Clone, Debug)]
    pub enum Type {
        Simple(SimpleType),
        Unit(UnitType),
        Idk(IdkType),
        Array(ArrayType),
        Struct(StructType),
        Alias(AliasType),
    }
}

#[derive(Clone, Debug)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
    pub semicolon_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(ExpressionStatement);
impl HasSourceRange for ExpressionStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let ExpressionStatement {
            expression,
            semicolon_token,
            id: _,
        } = self;
        semicolon_token
            .range
            .clone()
            .extend_with(expression.get_source_range())
    }
}

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
impl_enum_variant!(OnStatement);
impl HasSourceRange for OnStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let OnStatement {
            on_token,
            executor: _,
            iterators: _,
            open_paren_token: _,
            bindings: _,
            close_paren_token: _,
            body,
            id: _,
        } = self;
        on_token.range.clone().extend_with(body.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct BlockStatement {
    pub open_brace_token: Token,
    pub body: Vec<Statement>,
    pub close_brace_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(BlockStatement);
impl HasSourceRange for BlockStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let BlockStatement {
            open_brace_token,
            body: _,
            close_brace_token,
            id: _,
        } = self;
        open_brace_token
            .range
            .clone()
            .extend_with(close_brace_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct ReturnStatement {
    pub return_token: Token,
    pub value: Option<Box<Expression>>,
    pub semicolon_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(ReturnStatement);
impl HasSourceRange for ReturnStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let ReturnStatement {
            return_token,
            value: _,
            semicolon_token,
            id: _,
        } = self;
        return_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct VariableDefinitionStatement {
    pub let_token: Token,
    pub binding: Box<SimpleBinding>,
    pub equals_token: Token,
    pub value: Box<Expression>,
    pub semicolon_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(VariableDefinitionStatement);
impl HasSourceRange for VariableDefinitionStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let VariableDefinitionStatement {
            let_token,
            binding: _,
            equals_token: _,
            value: _,
            semicolon_token,
            id: _,
        } = self;
        let_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct IfStatement {
    pub if_token: Token,
    pub condition: Box<Expression>,
    pub if_body: Box<Statement>,
    pub elifs: Vec<(Token, Expression, Statement)>,
    pub else_: Option<(Token, Box<Statement>)>,
    pub id: NodeID,
}
impl_enum_variant!(IfStatement);
impl HasSourceRange for IfStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let IfStatement {
            if_token,
            condition: _,
            if_body,
            elifs,
            else_,
            id: _,
        } = self;
        if_token
            .range
            .clone()
            .extend_with(if_body.get_source_range())
            .maybe_extend(
                else_
                    .as_ref()
                    .map(|(_else_token, else_body)| else_body.get_source_range()),
            )
            .maybe_extend(
                elifs
                    .last()
                    .map(|(_elif_token, _condition, elif_body)| elif_body.get_source_range()),
            )
    }
}

#[derive(Clone, Debug)]
pub struct WhileLoopStatement {
    pub while_token: Token,
    pub condition: Box<Expression>,
    pub body: Box<Statement>,
    pub id: NodeID,
}
impl_enum_variant!(WhileLoopStatement);
impl HasSourceRange for WhileLoopStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let WhileLoopStatement {
            while_token,
            condition: _,
            body,
            id: _,
        } = self;
        while_token
            .range
            .clone()
            .extend_with(body.get_source_range())
    }
}

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
impl_enum_variant!(ForLoopStatement);
impl HasSourceRange for ForLoopStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let ForLoopStatement {
            for_token,
            open_paren_token: _,
            initializer: _,
            condition: _,
            second_semicolon_token: _,
            incrementer: _,
            close_paren_token: _,
            body,
            id: _,
        } = self;
        for_token.range.clone().extend_with(body.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct BreakStatement {
    pub break_token: Token,
    pub semicolon_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(BreakStatement);
impl HasSourceRange for BreakStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let BreakStatement {
            break_token,
            semicolon_token,
            id: _,
        } = self;
        break_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct SkipStatement {
    pub skip_token: Token,
    pub semicolon_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(SkipStatement);
impl HasSourceRange for SkipStatement {
    fn get_source_range(&self) -> NamedSourceRange {
        let SkipStatement {
            skip_token,
            semicolon_token,
            id: _,
        } = self;
        skip_token
            .range
            .clone()
            .extend_with(semicolon_token.range.clone())
    }
}

impl_enum_node! {
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
}

#[derive(Clone, Debug)]
pub struct SelfExecutorHost {
    pub token: Token,
    pub id: NodeID,
}
impl_enum_variant!(SelfExecutorHost);
impl HasSourceRange for SelfExecutorHost {
    fn get_source_range(&self) -> NamedSourceRange {
        let SelfExecutorHost { token, id: _ } = self;
        token.range.clone()
    }
}

impl_enum_node! {
    #[derive(Clone, Debug)]
    pub enum ExecutorHost {
        Self_(SelfExecutorHost),
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
impl_enum_variant!(ThreadExecutor);
impl HasSourceRange for ThreadExecutor {
    fn get_source_range(&self) -> NamedSourceRange {
        let ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index: _,
            close_bracket_token,
            id: _,
        } = self;
        close_bracket_token
            .range
            .clone()
            .extend_with(host.get_source_range())
    }
}

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
impl_enum_variant!(GPUExecutor);
impl HasSourceRange for GPUExecutor {
    fn get_source_range(&self) -> NamedSourceRange {
        let GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index: _,
            close_bracket_token,
            id: _,
        } = self;
        close_bracket_token
            .range
            .clone()
            .extend_with(host.get_source_range())
    }
}

impl_enum_node! {
    #[derive(Clone, Debug)]
    pub enum Executor {
        Thread(ThreadExecutor),
        GPU(GPUExecutor),
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
impl_enum_variant!(LiteralExpression);
impl HasSourceRange for LiteralExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let LiteralExpression {
            value: _,
            token,
            id: _,
        } = self;
        token.range.clone()
    }
}

#[derive(Clone, Debug)]
pub struct ArrayExpression {
    pub open_bracket_token: Token,
    pub elements: Vec<(Expression, Option<Token>)>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}
impl_enum_variant!(ArrayExpression);
impl HasSourceRange for ArrayExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let ArrayExpression {
            open_bracket_token,
            elements: _,
            close_bracket_token,
            id: _,
        } = self;
        open_bracket_token
            .range
            .clone()
            .extend_with(close_bracket_token.range.clone())
    }
}

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
impl_enum_variant!(StructExpression);
impl HasSourceRange for StructExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let StructExpression {
            type_,
            open_brace_token: _,
            members: _,
            close_brace_token,
            id: _,
        } = self;
        close_brace_token
            .range
            .clone()
            .extend_with(type_.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpression {
    pub name: String,
    pub name_token: Token,
    pub open_paren_token: Token,
    pub arguments: Vec<(Expression, Option<Token>)>,
    pub close_paren_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(FunctionCallExpression);
impl HasSourceRange for FunctionCallExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let FunctionCallExpression {
            name: _,
            name_token,
            open_paren_token: _,
            arguments: _,
            close_paren_token,
            id: _,
        } = self;
        name_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone())
    }
}

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

impl_enum_variant!(CompilerExpression);
impl HasSourceRange for CompilerExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let CompilerExpression {
            at_token,
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments: _,
            close_paren_token,
            id: _,
        } = self;
        at_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct ArrayIndexExpression {
    pub array: Box<Expression>,
    pub open_bracket_token: Token,
    pub index: Box<Expression>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(ArrayIndexExpression);
impl HasSourceRange for ArrayIndexExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index: _,
            close_bracket_token,
            id: _,
        } = self;
        close_bracket_token
            .range
            .clone()
            .extend_with(array.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct StructAccessExpression {
    pub value: Box<Expression>,
    pub dot_token: Token,
    pub member_name: String,
    pub member_name_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(StructAccessExpression);
impl HasSourceRange for StructAccessExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let StructAccessExpression {
            value,
            dot_token: _,
            member_name: _,
            member_name_token,
            id: _,
        } = self;
        member_name_token
            .range
            .clone()
            .extend_with(value.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct GroupingExpression {
    pub open_paren_token: Token,
    pub subexpression: Box<Expression>,
    pub close_paren_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(GroupingExpression);
impl HasSourceRange for GroupingExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let GroupingExpression {
            open_paren_token,
            subexpression: _,
            close_paren_token,
            id: _,
        } = self;
        open_paren_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone())
    }
}

#[derive(Clone, Debug)]
pub struct VariableAccessExpression {
    pub name: String,
    pub name_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(VariableAccessExpression);
impl HasSourceRange for VariableAccessExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let VariableAccessExpression {
            name: _,
            name_token,
            id: _,
        } = self;
        name_token.range.clone()
    }
}

#[derive(Clone, Debug)]
pub struct UnaryExpression {
    pub operator_token: Token,
    pub operation: UnaryOperation,
    pub operand: Box<Expression>,
    pub id: NodeID,
}

impl_enum_variant!(UnaryExpression);
impl HasSourceRange for UnaryExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let UnaryExpression {
            operator_token,
            operation: _,
            operand,
            id: _,
        } = self;
        operator_token
            .range
            .clone()
            .extend_with(operand.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct CastExpression {
    pub operand: Box<Expression>,
    pub as_token: Token,
    pub type_: Type,
    pub id: NodeID,
}

impl_enum_variant!(CastExpression);
impl HasSourceRange for CastExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let CastExpression {
            operand,
            as_token: _,
            type_,
            id: _,
        } = self;
        operand
            .get_source_range()
            .extend_with(type_.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator_token: Token,
    pub operation: BinaryOperation,
    pub right: Box<Expression>,
    pub id: NodeID,
}
impl_enum_variant!(BinaryExpression);
impl HasSourceRange for BinaryExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id: _,
        } = self;
        left.get_source_range()
            .extend_with(right.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct VariableAssignmentExpression {
    pub lvalue: LValue,
    pub equal_token: Token,
    pub right: Box<Expression>,
    pub id: NodeID,
}

impl_enum_variant!(VariableAssignmentExpression);
impl HasSourceRange for VariableAssignmentExpression {
    fn get_source_range(&self) -> NamedSourceRange {
        let VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id: _,
        } = self;
        lvalue
            .get_source_range()
            .extend_with(right.get_source_range())
    }
}

impl_enum_node! {
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
}

impl Expression {
    pub const TOP_PRECEDENCE: usize = usize::MAX;
    pub fn get_precedence(&self) -> usize {
        use BinaryOperation as Bo;
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
                operation: Bo::Multiply | Bo::Divide | Bo::Modulo,
                ..
            }) => 4,
            Expression::Binary(BinaryExpression {
                operation: Bo::Add | Bo::Subtract,
                ..
            }) => 5,
            Expression::Binary(BinaryExpression {
                operation:
                    Bo::LessThan | Bo::LessThanOrEqual | Bo::GreaterThan | Bo::GreaterThanOrEqual,
                ..
            }) => 6,
            Expression::Binary(BinaryExpression {
                operation: Bo::Equal | Bo::NotEqual,
                ..
            }) => 7,
            Expression::Binary(BinaryExpression {
                operation: Bo::LogicalAnd,
                ..
            }) => 8,
            Expression::Binary(BinaryExpression {
                operation: Bo::LogicalOr,
                ..
            }) => 9,

            Expression::VariableAssignment { .. } => 10,
        }
    }
    pub fn get_associativity(&self) -> Associativity {
        use BinaryOperation as Bo;
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
                operation: Bo::Add | Bo::Multiply,
                ..
            }) => Associativity::Both,
            Expression::Binary(BinaryExpression {
                operation:
                    Bo::Subtract
                    | Bo::Divide
                    | Bo::Modulo
                    | Bo::GreaterThan
                    | Bo::GreaterThanOrEqual
                    | Bo::LessThan
                    | Bo::LessThanOrEqual
                    | Bo::Equal
                    | Bo::NotEqual
                    | Bo::LogicalAnd
                    | Bo::LogicalOr,
                ..
            }) => Associativity::Left,

            Expression::VariableAssignment { .. } => Associativity::Right,
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariableLValue {
    pub name: String,
    pub name_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(VariableLValue);
impl HasSourceRange for VariableLValue {
    fn get_source_range(&self) -> NamedSourceRange {
        let VariableLValue {
            name: _,
            name_token,
            id: _,
        } = self;
        name_token.range.clone()
    }
}

#[derive(Clone, Debug)]
pub struct ArrayIndexLValue {
    pub array: Box<LValue>,
    pub open_bracket_token: Token,
    pub index: Box<Expression>,
    pub close_bracket_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(ArrayIndexLValue);
impl HasSourceRange for ArrayIndexLValue {
    fn get_source_range(&self) -> NamedSourceRange {
        let ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index: _,
            close_bracket_token,
            id: _,
        } = self;
        close_bracket_token
            .range
            .clone()
            .extend_with(array.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct StructAccessLValue {
    pub value: Box<LValue>,
    pub dot_token: Token,
    pub member_name: String,
    pub member_name_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(StructAccessLValue);
impl HasSourceRange for StructAccessLValue {
    fn get_source_range(&self) -> NamedSourceRange {
        let StructAccessLValue {
            value,
            dot_token: _,
            member_name: _,
            member_name_token,
            id: _,
        } = self;
        member_name_token
            .range
            .clone()
            .extend_with(value.get_source_range())
    }
}

#[derive(Clone, Debug)]
pub struct GroupingLValue {
    pub open_paren_token: Token,
    pub sublvalue: Box<LValue>,
    pub close_paren_token: Token,
    pub id: NodeID,
}

impl_enum_variant!(GroupingLValue);
impl HasSourceRange for GroupingLValue {
    fn get_source_range(&self) -> NamedSourceRange {
        let GroupingLValue {
            open_paren_token,
            sublvalue: _,
            close_paren_token,
            id: _,
        } = self;
        open_paren_token
            .range
            .clone()
            .extend_with(close_paren_token.range.clone())
    }
}

impl_enum_node! {
    #[derive(Clone, Debug)]
    pub enum LValue {
        Variable(VariableLValue),
        ArrayIndex(ArrayIndexLValue),
        StructAccess(StructAccessLValue),
        Grouping(GroupingLValue),
    }
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
