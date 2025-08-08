use std::{
    any::TypeId,
    borrow::Borrow,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::passes::pass_manager::{CheckedEntry, EmptyEntry, Pass, PassFactory};

use super::pass_manager::{GlobalState, PassResult};

/// Copies the data in `From` to `To`
///
/// Note that you can only store into wrappers of `T`, not a `T` itself.
pub struct StorePass<From, To, T = <To as Deref>::Target>
where
    From: Borrow<T>,
    To: DerefMut<Target = T> + ::core::convert::From<T>,
    T: Clone,
{
    _marker: PhantomData<(From, To, T)>,
}

pub struct StorePassImpl<'state, From, To, T>
where
    From: Borrow<T>,
    To: DerefMut<Target = T> + ::core::convert::From<T>,
    T: Clone,
{
    from: CheckedEntry<From>,
    to: EmptyEntry<To>,
    state: &'state mut GlobalState,
    _marker: PhantomData<T>,
}

impl<From, To, T> PassFactory for StorePass<From, To, T>
where
    From: Borrow<T> + 'static,
    To: DerefMut<Target = T> + ::core::convert::From<T> + 'static,
    T: Clone + 'static,
{
    type Output<'state> = StorePassImpl<'state, From, To, T>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        assert_ne!(
            TypeId::of::<From>(),
            TypeId::of::<To>(),
            "Cannot store a value into itself",
        );

        let from = state.check_named::<From>()?;
        let to = state.check_empty_named::<To>()?;

        Ok(Self::Output {
            from,
            to,
            state,
            _marker: PhantomData,
        })
    }
}

impl<From, To, T> Pass for StorePassImpl<'_, From, To, T>
where
    From: Borrow<T> + 'static,
    To: DerefMut<Target = T> + ::core::convert::From<T> + 'static,
    T: Clone,
{
    fn run(self: Box<Self>) -> PassResult {
        let from = self.from.get(self.state).deref().borrow().clone();
        self.to.set(self.state, from.into());

        Ok(())
    }
}
