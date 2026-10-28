use std::{
    any::TypeId,
    borrow::BorrowMut,
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

use crate::passes::{
    global_state::{FetchError, Fetched, GlobalState},
    pass_manager::{Pass, PassFactory},
};

use super::pass_manager::PassResult;

/// Swaps the data in `From` and `To`
///
/// Note that, if you are swapping a `T` with a wrapper of `T`, you should always provide the
/// wrapper second to automatically infer `T`.
pub struct SwapPass<From, To>
where
    To: DerefMut,
    From: BorrowMut<<To as Deref>::Target>,
{
    _marker: PhantomData<(From, To)>,
}

pub struct SwapPassImpl<'state, From, To>
where
    To: DerefMut,
    From: BorrowMut<<To as Deref>::Target>,
{
    from: &'state mut From,
    to: &'state mut To,
}

impl<From, To> PassFactory for SwapPass<From, To>
where
    To: DerefMut<Target: Sized> + 'static,
    From: BorrowMut<<To as Deref>::Target> + 'static,
{
    type Output<'state> = SwapPassImpl<'state, From, To>;
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
            "Cannot swap a single value with itself"
        );

        let Fetched {
            read: (),
            write: (from, to),
            empty: (),
        } = state.fetch()?;

        Ok(Self::Output { from, to })
    }
}

impl<From, To> Pass for SwapPassImpl<'_, From, To>
where
    To: DerefMut<Target: Sized>,
    From: BorrowMut<<To as Deref>::Target>,
{
    fn run(self: Box<Self>) -> PassResult {
        let to = self.to.deref_mut();
        let from = self.from.borrow_mut();
        std::mem::swap(to, from);

        Ok(())
    }
}
