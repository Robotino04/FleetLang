use std::{
    any::{Any, TypeId, type_name},
    boxed::Box,
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, VecDeque},
    error::Error,
    fmt::Debug,
    marker::PhantomData,
    option::Option,
    rc::Rc,
};

use itertools::Itertools;
use log::{info, warn};
use thiserror::Error;

use crate::{
    ast::PerNodeData,
    infra::FleetError,
    passes::{
        runtime_type::RuntimeType,
        scope_analysis::{Function, FunctionID, Variable, VariableScope},
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

NewtypeDeref!(pub Errors, Vec<FleetError>, Clone);
NewtypeDeref!(pub VariableData, PerNodeData<Rc<RefCell<Variable>>>);
NewtypeDeref!(pub FunctionData, PerNodeData<Rc<RefCell<Function>>>);
NewtypeDeref!(pub TypeData, PerNodeData<UnionFindSetPtr<RuntimeType>>);
NewtypeDeref!(pub TypeSets, UnionFindSet<RuntimeType>);
NewtypeDeref!(pub ScopeData, PerNodeData<Rc<RefCell<VariableScope>>>);
NewtypeDeref!(pub StatData, PerNodeData<NodeStats>);
NewtypeDeref!(pub PrecompiledGlslFunctions, HashMap<FunctionID, (String, String)>);
NewtypeDeref!(pub CCodeOutput, String);

#[derive(Debug, Clone)]
pub struct InputSource {
    pub source: String,
    pub file_name: FileName,
}

pub use NewtypeDeref;
pub use NewtypeDerefNoDefault;

#[derive(Debug, Default)]
pub struct GlobalState {
    entries: HashMap<TypeId, Box<dyn Any>>,
}

impl GlobalState {
    pub fn insert<T>(&mut self, t: T) -> CheckedEntry<T>
    where
        T: Any,
    {
        let existing_entry = self.entries.insert(t.type_id(), Box::new(RefCell::new(t)));
        assert!(
            existing_entry.is_none(),
            "Tried inserting {:?} but it was already present",
            type_name::<T>()
        );
        CheckedEntry(PhantomData)
    }

    pub fn insert_default<T>(&mut self) -> CheckedEntry<T>
    where
        T: Any + Default,
    {
        self.insert(T::default())
    }

    pub fn get<T>(&self) -> Option<Ref<'_, T>>
    where
        T: Any,
    {
        Some(
            self.entries
                .get(&TypeId::of::<T>())?
                .downcast_ref::<RefCell<T>>()
                .expect("Type id matched, but downcast failed")
                .borrow(),
        )
    }

    pub fn get_mut<T>(&self) -> Option<RefMut<'_, T>>
    where
        T: Any,
    {
        Some(
            self.entries
                .get(&TypeId::of::<T>())?
                .downcast_ref::<RefCell<T>>()
                .expect("Type id matched, but downcast failed")
                .borrow_mut(),
        )
    }

    pub fn get_named<T>(&self) -> Result<Ref<'_, T>, String>
    where
        T: Any,
    {
        self.get().ok_or(type_name::<T>().to_string())
    }

    pub fn get_mut_named<T>(&self) -> Result<RefMut<'_, T>, String>
    where
        T: Any,
    {
        self.get_mut().ok_or(type_name::<T>().to_string())
    }

    pub fn check<T>(&self) -> Option<CheckedEntry<T>>
    where
        T: Any,
    {
        self.get::<T>()?;
        Some(CheckedEntry(PhantomData))
    }

    pub fn check_named<T>(&self) -> Result<CheckedEntry<T>, String>
    where
        T: Any,
    {
        self.check().ok_or(type_name::<T>().to_string())
    }

    pub fn check_empty<T>(&self) -> Option<EmptyEntry<T>>
    where
        T: Any,
    {
        match self.get::<T>() {
            Some(_) => None,
            None => Some(EmptyEntry(PhantomData)),
        }
    }

    pub fn check_empty_named<T>(&self) -> Result<EmptyEntry<T>, String>
    where
        T: Any,
    {
        self.check_empty().ok_or(type_name::<T>().to_string())
    }
}

pub struct CheckedEntry<T>(PhantomData<T>);

impl<T> CheckedEntry<T> {
    pub fn get(self, state: &GlobalState) -> Ref<'_, T>
    where
        T: Any,
    {
        state.get().expect(
            "Entry must exist because CheckedEntry can only be obtained via get_token or insert",
        )
    }

    pub fn get_mut(self, state: &GlobalState) -> RefMut<'_, T>
    where
        T: Any,
    {
        state.get_mut().expect(
            "Entry must exist because CheckedEntry can only be obtained via get_token or insert",
        )
    }
}

pub struct EmptyEntry<T>(PhantomData<T>);

impl<T> EmptyEntry<T> {
    pub fn set(self, state: &mut GlobalState, t: T) -> CheckedEntry<T>
    where
        T: Any,
    {
        state.insert::<T>(t);
        state
            .check::<T>()
            .expect("This element should have just been inserted")
    }
}

pub trait PassFactory {
    type Output<'state>: Pass + 'state;
    type Params: Clone + 'static;

    fn try_new<'state>(
        state: &'state mut GlobalState,
        params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
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
    ) -> Result<Box<dyn Pass + 'state>, String>;
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
    ) -> Result<Box<dyn Pass + 'state>, String> {
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
