use std::{
    any::type_name, boxed::Box, collections::VecDeque, error::Error, fmt::Debug,
    marker::PhantomData,
};

use itertools::Itertools;
use log::{info, warn};
use thiserror::Error;

use super::prelude::*;

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
