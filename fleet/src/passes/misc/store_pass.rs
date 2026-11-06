use std::{
    any::TypeId,
    borrow::Borrow,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::passes::prelude::*;

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
    from: &'state From,
    to: Empty<'state, To>,
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
    ) -> Result<Self::Output<'state>, FetchError>
    where
        Self: Sized,
    {
        assert_ne!(
            TypeId::of::<From>(),
            TypeId::of::<To>(),
            "Cannot store a value into itself",
        );

        let Fetched {
            read: (from,),
            write: (),
            empty: (to,),
        } = state.fetch()?;

        Ok(Self::Output { from, to })
    }
}

impl<From, To> Pass for StorePassImpl<'_, From, To>
where
    To: DerefMut<Target: Sized + Clone> + ::core::convert::From<<To as Deref>::Target> + 'static,
    From: Borrow<<To as Deref>::Target> + 'static,
{
    fn run(self: Box<Self>) -> PassResult {
        self.to.insert(self.from.borrow().clone().into());

        Ok(())
    }
}
