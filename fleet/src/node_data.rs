use std::{
    any::{TypeId, type_name},
    collections::HashMap,
    fmt::Debug,
    ops::{ControlFlow, Deref, DerefMut},
};

use thiserror::Error;

use crate::{
    ast::{
        AstNodeRefMut, Executor, ExecutorHost, Expression, FunctionBody, HasID, LValue, NodeID,
        Program, Statement, TopLevelStatement, Type,
    },
    passes::{find_node_by_id::find_node_by_id, partial_visitor::PartialAstVisitor},
};

pub trait HasData<T: NodeDataSpec>: HasID {
    type Data;

    fn extract_data_enum(data: &NodeDataEnum<T>) -> Option<&Self::Data>;
    fn extract_data_enum_mut(data: &mut NodeDataEnum<T>) -> Option<&mut Self::Data>;
}
pub trait CanStoreData<T: NodeDataSpec>: HasData<T> {
    fn build_data_enum(&self, data: Self::Data) -> NodeDataEnum<T>;
}

pub struct PerNodeData<Data: NodeDataSpec> {
    map: HashMap<NodeID, NodeDataEnum<Data>>,
}

impl<Data: NodeDataSpec + Debug> Debug for PerNodeData<Data>
where
    NodeDataEnum<Data>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PerNodeData")
            .field("map", &self.map)
            .finish()
    }
}

impl<Data: NodeDataSpec> Clone for PerNodeData<Data>
where
    NodeDataEnum<Data>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            map: self.map.clone(),
        }
    }
}

impl<Data: NodeDataSpec> Default for PerNodeData<Data> {
    fn default() -> Self {
        Self {
            map: Default::default(),
        }
    }
}

impl<Data: NodeDataSpec> Deref for PerNodeData<Data> {
    type Target = HashMap<NodeID, NodeDataEnum<Data>>;

    fn deref(&self) -> &Self::Target {
        &self.map
    }
}
impl<Data: NodeDataSpec> DerefMut for PerNodeData<Data> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.map
    }
}

#[derive(Clone, Debug, Error)]
#[error("{msg}")]
pub struct PerNodeDataFinishError {
    nodes: Vec<NodeID>,
    msg: String,
}

impl<Data: NodeDataSpec> PerNodeData<Data> {
    pub fn insert<Node: CanStoreData<Data>>(
        &mut self,
        node: &Node,
        data: Node::Data,
    ) -> Option<NodeDataEnum<Data>> {
        self.map.insert(node.get_id(), node.build_data_enum(data))
    }

    pub fn get<Node: HasData<Data>>(&self, node: &Node) -> Option<&Node::Data> {
        Node::extract_data_enum(self.map.get(&node.get_id())?)
    }

    pub fn get_mut<Node: HasData<Data>>(&mut self, node: &Node) -> Option<&mut Node::Data> {
        Node::extract_data_enum_mut(self.map.get_mut(&node.get_id())?)
    }

    pub fn finish(
        mut self,
        program: &mut Program,
    ) -> Result<FinalPerNodeData<Data>, PerNodeDataFinishError> {
        let mut finisher = PerNodeDataFinisher {
            errors: Vec::new(),
            map: &mut self, // will insert missing ()
        };
        let _ = finisher.visit_program(program);

        if finisher.errors.is_empty() {
            Ok(FinalPerNodeData { map: self })
        } else {
            let nodes_to_print = 5;

            let mut msg = format!(
                "The following nodes (and {} more) didn't get data filled in for {}:",
                finisher.errors.len().saturating_sub(nodes_to_print),
                type_name::<Data>()
            );

            msg.extend(finisher.errors.iter().take(nodes_to_print).map(|node| {
                // very expensive, but we're already on the cold path anyways
                let node = find_node_by_id(&mut *program, *node)
                    .expect("We got this NodeID from this program");
                format!("\n{node:?}")
            }));

            Err(PerNodeDataFinishError {
                nodes: finisher.errors,
                msg,
            })
        }
    }
}

pub struct FinalPerNodeData<Data: NodeDataSpec> {
    map: PerNodeData<Data>,
}

impl<Data: NodeDataSpec + Debug> Debug for FinalPerNodeData<Data>
where
    NodeDataEnum<Data>: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PerNodeData")
            .field("map", &self.map)
            .finish()
    }
}

impl<Data: NodeDataSpec> Clone for FinalPerNodeData<Data>
where
    NodeDataEnum<Data>: Clone,
{
    fn clone(&self) -> Self {
        Self {
            map: self.map.clone(),
        }
    }
}

impl<Data: NodeDataSpec> FinalPerNodeData<Data> {
    // not a Deref impl so it doesn't get used by accident
    pub fn inner(&self) -> &HashMap<NodeID, NodeDataEnum<Data>> {
        &self.map
    }

    pub fn get<Node: HasData<Data>>(&self, node: &Node) -> &Node::Data {
        self.map
            .get(node)
            .expect("FinalPerNodeData should contain correct data for all nodes by construction")
    }

    pub fn get_mut<Node: HasData<Data>>(&mut self, node: &Node) -> &mut Node::Data {
        self.map
            .get_mut(node)
            .expect("FinalPerNodeData should contain correct data for all nodes by construction")
    }
}

struct PerNodeDataFinisher<'a, Data: NodeDataSpec> {
    errors: Vec<NodeID>,
    map: &'a mut PerNodeData<Data>,
}

impl<Data: NodeDataSpec> PerNodeDataFinisher<'_, Data> {
    fn ensure_exists<Node: CanStoreData<Data, Data: 'static>>(&mut self, node: &Node) {
        if TypeId::of::<Node::Data>() == TypeId::of::<()>() {
            // just to be sure
            debug_assert_eq!(size_of::<Node::Data>(), size_of::<()>());

            // SAFETY: Node::Data: 'static and we just verified that Node::Data == ()
            let unit_as_data = unsafe { std::mem::transmute_copy::<(), Node::Data>(&()) };

            self.map
                .entry(node.get_id())
                .or_insert(node.build_data_enum(unit_as_data));
        }

        match self.map.get(node) {
            Some(_) => {}
            None => {
                self.errors.push(node.get_id());
            }
        }
    }
}

impl<Data: NodeDataSpec> PartialAstVisitor for PerNodeDataFinisher<'_, Data> {
    type Break<'a> = ();

    fn visit_pre<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        match node {
            AstNodeRefMut::Program(node) => self.ensure_exists(node),
            AstNodeRefMut::TopLevelStatement(node) => match node {
                TopLevelStatement::FunctionDefinition(node) => self.ensure_exists(node),
                TopLevelStatement::TypeAlias(node) => self.ensure_exists(node),
            },
            AstNodeRefMut::FunctionBody(node) => match node {
                FunctionBody::Statement(node) => self.ensure_exists(node),
                FunctionBody::Extern(node) => self.ensure_exists(node),
            },
            AstNodeRefMut::SimpleBinding(node) => self.ensure_exists(node),
            AstNodeRefMut::Statement(node) => match node {
                Statement::Expression(node) => self.ensure_exists(node),
                Statement::On(node) => self.ensure_exists(node),
                Statement::Block(node) => self.ensure_exists(node),
                Statement::Return(node) => self.ensure_exists(node),
                Statement::VariableDefinition(node) => self.ensure_exists(node),
                Statement::If(node) => self.ensure_exists(node),
                Statement::WhileLoop(node) => self.ensure_exists(node),
                Statement::ForLoop(node) => self.ensure_exists(node),
                Statement::Break(node) => self.ensure_exists(node),
                Statement::Skip(node) => self.ensure_exists(node),
            },
            AstNodeRefMut::ExecutorHost(node) => match node {
                ExecutorHost::Self_(node) => self.ensure_exists(node),
            },
            AstNodeRefMut::Executor(node) => match node {
                Executor::Thread(node) => self.ensure_exists(node),
                Executor::GPU(node) => self.ensure_exists(node),
            },
            AstNodeRefMut::Expression(node) => match node {
                Expression::Literal(node) => self.ensure_exists(node),
                Expression::Array(node) => self.ensure_exists(node),
                Expression::Struct(node) => self.ensure_exists(node),
                Expression::FunctionCall(node) => self.ensure_exists(node),
                Expression::CompilerExpression(node) => self.ensure_exists(node),
                Expression::ArrayIndex(node) => self.ensure_exists(node),
                Expression::StructAccess(node) => self.ensure_exists(node),
                Expression::Grouping(node) => self.ensure_exists(node),
                Expression::VariableAccess(node) => self.ensure_exists(node),
                Expression::Unary(node) => self.ensure_exists(node),
                Expression::Cast(node) => self.ensure_exists(node),
                Expression::Binary(node) => self.ensure_exists(node),
                Expression::VariableAssignment(node) => self.ensure_exists(node),
            },
            AstNodeRefMut::LValue(node) => match node {
                LValue::Variable(node) => self.ensure_exists(node),
                LValue::ArrayIndex(node) => self.ensure_exists(node),
                LValue::StructAccess(node) => self.ensure_exists(node),
                LValue::Grouping(node) => self.ensure_exists(node),
            },
            AstNodeRefMut::Type(node) => match node {
                Type::Simple(node) => self.ensure_exists(node),
                Type::Unit(node) => self.ensure_exists(node),
                Type::Idk(node) => self.ensure_exists(node),
                Type::Array(node) => self.ensure_exists(node),
                Type::Struct(node) => self.ensure_exists(node),
                Type::Alias(node) => self.ensure_exists(node),
            },
        }
        ControlFlow::Continue(())
    }
}

pub trait NodeDataSpecGroups {
    type Program: 'static;
    type TopLevelStatement: 'static;
    type FunctionBody: 'static;
    type SimpleBinding: 'static;
    type Statement: 'static;
    type ExecutorHost: 'static;
    type Executor: 'static;
    type Expression: 'static;
    type LValue: 'static;
    type Type: 'static;
}

impl<T> NodeDataSpec for T
where
    T: NodeDataSpecGroups,
{
    type Program = T::Program;
    type FunctionDefinition = T::TopLevelStatement;
    type TypeAlias = T::TopLevelStatement;

    type ExternFunctionBody = T::FunctionBody;
    type StatementFunctionBody = T::FunctionBody;

    type SimpleBinding = T::SimpleBinding;

    type ExpressionStatement = T::Statement;
    type OnStatement = T::Statement;
    type BlockStatement = T::Statement;
    type ReturnStatement = T::Statement;
    type VariableDefinitionStatement = T::Statement;
    type IfStatement = T::Statement;
    type WhileLoopStatement = T::Statement;
    type ForLoopStatement = T::Statement;
    type BreakStatement = T::Statement;
    type SkipStatement = T::Statement;

    type SelfExecutorHost = T::ExecutorHost;

    type ThreadExecutor = T::Executor;
    type GPUExecutor = T::Executor;

    type LiteralExpression = T::Expression;
    type ArrayExpression = T::Expression;
    type StructExpression = T::Expression;
    type FunctionCallExpression = T::Expression;
    type CompilerExpression = T::Expression;
    type ArrayIndexExpression = T::Expression;
    type StructAccessExpression = T::Expression;
    type GroupingExpression = T::Expression;
    type VariableAccessExpression = T::Expression;
    type UnaryExpression = T::Expression;
    type CastExpression = T::Expression;
    type BinaryExpression = T::Expression;
    type VariableAssignmentExpression = T::Expression;

    type VariableLValue = T::LValue;
    type ArrayIndexLValue = T::LValue;
    type StructAccessLValue = T::LValue;
    type GroupingLValue = T::LValue;

    type SimpleType = T::Type;
    type UnitType = T::Type;
    type IdkType = T::Type;
    type ArrayType = T::Type;
    type StructType = T::Type;
    type AliasType = T::Type;
}

macro_rules! with_data_enum_variants {
    ( $($rules:tt)* ) => {
        macro_rules! __emit__ { $($rules)* }

        __emit__! {
            Program
            FunctionDefinition
            TypeAlias
            ExternFunctionBody
            StatementFunctionBody
            SimpleBinding
            ExpressionStatement
            OnStatement
            BlockStatement
            ReturnStatement
            VariableDefinitionStatement
            IfStatement
            WhileLoopStatement
            ForLoopStatement
            BreakStatement
            SkipStatement
            SelfExecutorHost
            ThreadExecutor
            GPUExecutor
            LiteralExpression
            ArrayExpression
            StructExpression
            FunctionCallExpression
            CompilerExpression
            ArrayIndexExpression
            StructAccessExpression
            GroupingExpression
            VariableAccessExpression
            UnaryExpression
            CastExpression
            BinaryExpression
            VariableAssignmentExpression
            VariableLValue
            ArrayIndexLValue
            StructAccessLValue
            GroupingLValue
            SimpleType
            UnitType
            IdkType
            ArrayType
            StructType
            AliasType
        }
    };
}

with_data_enum_variants! { ( $($Variant:ident)* ) => {

pub trait NodeDataSpec {
    $(type $Variant: 'static = ();)*
}

#[derive(Copy, Debug)]
pub enum NodeDataEnum<T: NodeDataSpec> {
    $($Variant(T::$Variant),)*
}

impl<T: NodeDataSpec> Clone for NodeDataEnum<T>
where
    $(T::$Variant: Clone),*
{
    fn clone(&self) -> Self {
        match self {
            $(
                Self::$Variant(x) => Self::$Variant(x.clone()),
            )*
        }
    }
}

impl<T: NodeDataSpec> PartialEq for NodeDataEnum<T>
where
    $(T::$Variant: PartialEq),*
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            $(
                (Self::$Variant(s), Self::$Variant(o)) => s.eq(o),
            )*
            _ => false,
        }
    }
}

impl<T: NodeDataSpec> Eq for NodeDataEnum<T>
where
    $(T::$Variant: Eq),*
{}

impl<T, U> NodeDataEnum<T>
where
    T: NodeDataSpec<
        $($Variant = U,)*
    >,
{
    #[must_use]
    /// Extracts the contained value if all nodes store the same type
    pub fn extract(self) -> U {
        match self {
            $(
                Self::$Variant(x) => x,
            )*
        }
    }
    #[must_use]
    /// Extracts the contained value if all nodes store the same type
    pub fn extract_ref(&self) -> &U {
        match self {
            $(
                Self::$Variant(x) => x,
            )*
        }
    }
    #[must_use]
    /// Extracts the contained value if all nodes store the same type
    pub fn extract_mut(&mut self) -> &mut U {
        match self {
            $(
                Self::$Variant(x) => x,
            )*
        }
    }
}

}}
