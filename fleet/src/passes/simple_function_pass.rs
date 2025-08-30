use std::{borrow::Borrow, marker::PhantomData};

use crate::passes::pass_manager::{CheckedEntry, Pass, PassFactory};

use super::pass_manager::{GlobalState, PassResult};

pub struct SingleFunctionPass<T, F>
where
    F: FnOnce(&T) -> PassResult,
{
    _marker: PhantomData<(T, F)>,
}

pub struct SingleFunctionPassImpl<'state, T, F>
where
    F: FnOnce(&T) -> PassResult + Clone,
{
    from: CheckedEntry<T>,
    function: F,
    state: &'state mut GlobalState,
    _marker: PhantomData<T>,
}

impl<T, F> PassFactory for SingleFunctionPass<T, F>
where
    T: 'static,
    F: FnOnce(&T) -> PassResult + Clone + 'static,
{
    type Output<'state> = SingleFunctionPassImpl<'state, T, F>;
    type Params = F;

    fn try_new<'state>(
        state: &'state mut GlobalState,
        params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        let from = state.check_named::<T>()?;

        Ok(Self::Output {
            from,
            function: params,
            state,
            _marker: PhantomData,
        })
    }
}

impl<T, F> Pass for SingleFunctionPassImpl<'_, T, F>
where
    T: 'static,
    F: FnOnce(&T) -> PassResult + Clone,
{
    fn run(self: Box<Self>) -> PassResult {
        let binding = self.from.get(self.state);
        (self.function)(binding.borrow())
    }
}
