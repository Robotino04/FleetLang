use std::{
    any::type_name,
    boxed::Box,
    cell::RefCell,
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::Debug,
    marker::PhantomData,
    rc::Rc,
};

use itertools::Itertools;
use log::{info, warn};
use thiserror::Error;

use crate::{
    ast::NodeID,
    node_data::{FinalPerNodeData, NodeDataSpec, NodeDataSpecGroups},
    passes::{
        global_state::{FetchError, GlobalState},
        runtime_type::{ConcreteRuntimeType, RuntimeType},
        scope_analysis::{
            ConcreteFunction, ConcreteVariable, ConcreteVariableScope, Function, FunctionID,
            Variable, VariableScope,
        },
        stat_tracker::NodeStats,
        union_find_set::{UnionFindSet, UnionFindSetPtr},
    },
    tokenizer::FileName,
};

#[macro_export]
macro_rules! NewtypeDerefNoDefault {
    ($vis:vis $name:ident, $contents:ty $(, $($derives:ident),*)?) => {
        #[derive(Debug $(, $($derives),*)?)]
        $vis struct $name(pub $contents);

        impl ::core::ops::Deref for $name {
            type Target = $contents;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        impl ::core::ops::DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
        impl ::core::borrow::Borrow<$contents> for $name {
            fn borrow(&self) -> &$contents {
                &self.0
            }
        }
        impl ::core::borrow::BorrowMut<$contents> for $name {
            fn borrow_mut(&mut self) -> &mut $contents {
                &mut self.0
            }
        }
        impl ::core::convert::From<$contents> for $name {
            fn from(contents: $contents) -> Self {
                Self(contents)
            }
        }
        impl ::core::convert::From<$name > for $contents {
            fn from(wrapper: $name) -> Self {
                wrapper.0
            }
        }
    };
}

#[macro_export]
macro_rules! NewtypeDeref {
    ($vis:vis $name:ident, $contents:ty $(, $($derives:ident),*)?) => {
        $crate::NewtypeDerefNoDefault!($vis $name, $contents $(, $($derives),* )?);

        impl ::core::default::Default for $name {
            fn default() -> Self {
                $name(<$contents as ::core::default::Default>::default())
            }
        }
    };
}

pub use NewtypeDeref;
pub use NewtypeDerefNoDefault;

// TODO: distribute these to their generating pass
impl NodeDataSpecGroups for TypeData {
    type Program = ();
    type TopLevelStatement = ();
    type FunctionBody = ();
    type SimpleBinding = UnionFindSetPtr<RuntimeType>;
    type Statement = ();
    type ExecutorHost = ();
    type Executor = ();
    type Expression = UnionFindSetPtr<RuntimeType>;
    type LValue = UnionFindSetPtr<RuntimeType>;
    type Type = UnionFindSetPtr<RuntimeType>;
}
NewtypeDerefNoDefault!(pub TypeData, FinalPerNodeData<TypeData>);

impl NodeDataSpec for VariableData {
    // TODO: will become incorrect once we have more complex patterns
    type VariableDefinitionStatement = Rc<RefCell<Variable>>;

    type SimpleBinding = Rc<RefCell<Variable>>;
    type VariableAccessExpression = Rc<RefCell<Variable>>;
    type VariableLValue = Rc<RefCell<Variable>>;
}
NewtypeDerefNoDefault!(pub VariableData, FinalPerNodeData<VariableData>);

impl NodeDataSpecGroups for ScopeData {
    type Program = Rc<RefCell<VariableScope>>;
    type TopLevelStatement = Rc<RefCell<VariableScope>>;
    type FunctionBody = Rc<RefCell<VariableScope>>;
    type SimpleBinding = Rc<RefCell<VariableScope>>;
    type Statement = Rc<RefCell<VariableScope>>;
    type ExecutorHost = Rc<RefCell<VariableScope>>;
    type Executor = Rc<RefCell<VariableScope>>;
    type Expression = Rc<RefCell<VariableScope>>;
    type LValue = Rc<RefCell<VariableScope>>;
    type Type = Rc<RefCell<VariableScope>>;
}
NewtypeDerefNoDefault!(pub ScopeData, FinalPerNodeData<ScopeData>);

impl NodeDataSpec for FunctionData {
    type FunctionDefinition = Rc<RefCell<Function>>;
    type FunctionCallExpression = Rc<RefCell<Function>>;
    type ExternFunctionBody = Rc<RefCell<Function>>;
}
NewtypeDerefNoDefault!(pub FunctionData, FinalPerNodeData<FunctionData>);

NewtypeDeref!(pub TypeSets, UnionFindSet<RuntimeType>);

impl NodeDataSpecGroups for ConcreteTypeData {
    type Program = ();
    type TopLevelStatement = ();
    type FunctionBody = ();
    type SimpleBinding = ConcreteRuntimeType;
    type Statement = ();
    type ExecutorHost = ();
    type Executor = ();
    type Expression = ConcreteRuntimeType;
    type LValue = ConcreteRuntimeType;
    type Type = ConcreteRuntimeType;
}
NewtypeDerefNoDefault!(pub ConcreteTypeData, FinalPerNodeData<ConcreteTypeData>);

impl NodeDataSpec for ConcreteVariableData {
    // TODO: will become incorrect once we have more complex patterns
    type VariableDefinitionStatement = Rc<RefCell<ConcreteVariable>>;

    type SimpleBinding = Rc<RefCell<ConcreteVariable>>;
    type VariableAccessExpression = Rc<RefCell<ConcreteVariable>>;
    type VariableLValue = Rc<RefCell<ConcreteVariable>>;
}
NewtypeDerefNoDefault!(pub ConcreteVariableData, FinalPerNodeData<ConcreteVariableData>);

impl NodeDataSpecGroups for ConcreteScopeData {
    type Program = Rc<RefCell<ConcreteVariableScope>>;
    type TopLevelStatement = Rc<RefCell<ConcreteVariableScope>>;
    type FunctionBody = Rc<RefCell<ConcreteVariableScope>>;
    type SimpleBinding = Rc<RefCell<ConcreteVariableScope>>;
    type Statement = Rc<RefCell<ConcreteVariableScope>>;
    type ExecutorHost = Rc<RefCell<ConcreteVariableScope>>;
    type Executor = Rc<RefCell<ConcreteVariableScope>>;
    type Expression = Rc<RefCell<ConcreteVariableScope>>;
    type LValue = Rc<RefCell<ConcreteVariableScope>>;
    type Type = Rc<RefCell<ConcreteVariableScope>>;
}
NewtypeDerefNoDefault!(pub ConcreteScopeData, FinalPerNodeData<ConcreteScopeData>);

impl NodeDataSpec for ConcreteFunctionData {
    type FunctionDefinition = Rc<RefCell<ConcreteFunction>>;
    type FunctionCallExpression = Rc<RefCell<ConcreteFunction>>;
    type ExternFunctionBody = Rc<RefCell<ConcreteFunction>>;
}
NewtypeDerefNoDefault!(pub ConcreteFunctionData, FinalPerNodeData<ConcreteFunctionData>);

NewtypeDeref!(pub StatData, HashMap<NodeID, NodeStats>);
NewtypeDeref!(pub StructAliasMap, HashMap<String, (usize, String)>, Clone);
NewtypeDeref!(pub PrecompiledGlslFunctions, (HashMap<FunctionID, (String, String)>, StructAliasMap, u64), Clone);
NewtypeDeref!(pub CCodeOutput, String);

#[derive(Debug, Clone)]
pub struct InputSource {
    pub source: String,
    pub file_name: FileName,
}

pub trait PassFactory {
    type Output<'state>: Pass + 'state;
    type Params: Clone + 'static;

    fn try_new<'state>(
        state: &'state mut GlobalState,
        params: Self::Params,
    ) -> Result<Self::Output<'state>, FetchError>
    where
        Self: Sized;
}

#[derive(Error, Debug)]
pub enum PassError {
    #[error("[{producing_pass}] input program was invalid: {source}")]
    InvalidInput {
        producing_pass: String,
        #[source]
        source: Box<dyn Error>,
    },
    #[error("[{producing_pass}] compiler failure: {source}")]
    CompilerError {
        producing_pass: String,
        #[source]
        source: Box<dyn Error>,
    },
    #[error("Pass manager stalled with {passes_left:#?} left to execute")]
    PassManagerStall { passes_left: Vec<String> },
}

pub type PassResult = Result<(), PassError>;

pub trait Pass {
    fn name() -> String
    where
        Self: Sized,
    {
        type_name::<Self>().to_string()
    }

    fn run(self: Box<Self>) -> PassResult;
}

pub trait DynPassFactory {
    fn name(&self) -> String;
    fn try_new_dyn<'state>(
        &self,
        state: &'state mut GlobalState,
    ) -> Result<Box<dyn Pass + 'state>, FetchError>;
}

pub struct PassFactoryWrapper<T: PassFactory> {
    _marker: PhantomData<T>,
    params: T::Params,
}

impl<T> DynPassFactory for PassFactoryWrapper<T>
where
    T: PassFactory,
{
    fn name(&self) -> String {
        T::Output::name()
    }

    fn try_new_dyn<'state>(
        &self,
        state: &'state mut GlobalState,
    ) -> Result<Box<dyn Pass + 'state>, FetchError> {
        T::try_new(state, self.params.clone()).map(|p| Box::new(p) as Box<dyn Pass + 'state>)
    }
}

#[derive(Default)]
pub struct PassManager {
    pub state: GlobalState,
    factories: VecDeque<Box<dyn DynPassFactory>>,
}

impl PassManager {
    pub fn insert<P>(&mut self)
    where
        P: PassFactory<Params = ()> + 'static,
    {
        self.factories.push_back(Box::new(PassFactoryWrapper::<P> {
            _marker: PhantomData,
            params: (),
        }));
    }
    pub fn insert_params<P>(&mut self, params: P::Params)
    where
        P: PassFactory + 'static,
    {
        self.factories.push_back(Box::new(PassFactoryWrapper::<P> {
            _marker: PhantomData,
            params,
        }));
    }

    pub fn run(&mut self) -> PassResult {
        let mut failed_passes = VecDeque::new();

        while let Some(factory) = self.factories.pop_front() {
            let pass = factory.try_new_dyn(&mut self.state);
            match pass {
                Ok(pass) => {
                    info!("{:-^80}", format!("| Running {:?} |", factory.name()));
                    let res = pass.run();
                    info!("{:-^80}", "");
                    res?;

                    // retry all previously failed passes
                    for failed_pass in failed_passes.drain(..).rev() {
                        self.factories.push_front(failed_pass);
                    }
                }
                Err(missing_field) => {
                    warn!(
                        "Pass {:?} failed to construct: missing {missing_field:?}",
                        factory.name()
                    );
                    failed_passes.push_back(factory);
                }
            }
        }

        if !failed_passes.is_empty() {
            return Err(PassError::PassManagerStall {
                passes_left: failed_passes
                    .into_iter()
                    .map(|pass| pass.name())
                    .collect_vec(),
            });
        }

        Ok(())
    }
}
