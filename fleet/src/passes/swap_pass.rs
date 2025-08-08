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
pub struct SwapPass<From, To, T = <To as Deref>::Target>
where
    From: BorrowMut<T>,
    To: DerefMut<Target = T>,
{
    _marker: PhantomData<(From, To, T)>,
}

pub struct SwapPassImpl<'state, From, To, T>
where
    From: BorrowMut<T>,
    To: DerefMut<Target = T>,
{
    from: RefMut<'state, From>,
    to: RefMut<'state, To>,
    _marker: PhantomData<T>,
}

impl<From, To, T> PassFactory for SwapPass<From, To, T>
where
    From: BorrowMut<T> + 'static,
    To: DerefMut<Target = T> + 'static,
    T: 'static,
{
    type Output<'state> = SwapPassImpl<'state, From, To, T>;
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
            _marker: PhantomData,
        })
    }
}

impl<From, To, T> Pass for SwapPassImpl<'_, From, To, T>
where
    From: BorrowMut<T>,
    To: DerefMut<Target = T>,
{
    fn run(mut self: Box<Self>) -> PassResult {
        let to = self.to.deref_mut().deref_mut();
        let from = self.from.deref_mut().borrow_mut();
        swap(to, from);

        Ok(())
    }
}
