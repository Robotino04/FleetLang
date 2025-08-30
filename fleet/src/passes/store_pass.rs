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
pub struct StorePass<From, To>
where
    To: DerefMut<Target: Sized> + ::core::convert::From<<To as Deref>::Target>,
    From: Borrow<<To as Deref>::Target>,
{
    _marker: PhantomData<(From, To)>,
}

pub struct StorePassImpl<'state, From, To>
where
    To: DerefMut<Target: Sized> + ::core::convert::From<<To as Deref>::Target>,
    From: Borrow<<To as Deref>::Target>,
{
    from: CheckedEntry<From>,
    to: EmptyEntry<To>,
    state: &'state mut GlobalState,
}

impl<From, To> PassFactory for StorePass<From, To>
where
    To: DerefMut<Target: Sized + Clone> + ::core::convert::From<<To as Deref>::Target> + 'static,
    From: Borrow<<To as Deref>::Target> + 'static,
{
    type Output<'state> = StorePassImpl<'state, From, To>;
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

        Ok(Self::Output { from, to, state })
    }
}

impl<From, To> Pass for StorePassImpl<'_, From, To>
where
    To: DerefMut<Target: Sized + Clone> + ::core::convert::From<<To as Deref>::Target> + 'static,
    From: Borrow<<To as Deref>::Target> + 'static,
{
    fn run(self: Box<Self>) -> PassResult {
        let from = self.from.get(self.state).deref().borrow().clone();
        self.to.set(self.state, from.into());

        Ok(())
    }
}
