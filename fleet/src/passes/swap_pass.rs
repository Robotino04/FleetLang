use std::{
    any::TypeId,
    borrow::BorrowMut,
    cell::RefMut,
    marker::PhantomData,
    mem::swap,
    ops::{Deref, DerefMut},
};

use crate::passes::pass_manager::{Pass, PassFactory};

use super::pass_manager::{GlobalState, PassResult};

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
    from: RefMut<'state, From>,
    to: RefMut<'state, To>,
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
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        assert_ne!(
            TypeId::of::<From>(),
            TypeId::of::<To>(),
            "Cannot swap a single value with itself"
        );

        let from = state.check_named::<From>()?;
        let to = state.check_named::<To>()?;

        Ok(Self::Output {
            from: from.get_mut(state),
            to: to.get_mut(state),
        })
    }
}

impl<From, To> Pass for SwapPassImpl<'_, From, To>
where
    To: DerefMut<Target: Sized>,
    From: BorrowMut<<To as Deref>::Target>,
{
    fn run(mut self: Box<Self>) -> PassResult {
        let to = self.to.deref_mut().deref_mut();
        let from = self.from.deref_mut().borrow_mut();
        swap(to, from);

        Ok(())
    }
}
