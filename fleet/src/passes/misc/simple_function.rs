use std::marker::PhantomData;

use crate::passes::prelude::*;

pub struct SimpleFunctionPass<T, F>
where
    F: FnOnce(&T) -> PassResult,
{
    _marker: PhantomData<(T, F)>,
}

pub struct SimpleFunctionPassImpl<'state, T, F>
where
    F: FnOnce(&T) -> PassResult + Clone,
{
    input: &'state T,
    function: F,
    _marker: PhantomData<T>,
}

impl<T, F> PassFactory for SimpleFunctionPass<T, F>
where
    T: 'static,
    F: FnOnce(&T) -> PassResult + Clone + 'static,
{
    type Output<'state> = SimpleFunctionPassImpl<'state, T, F>;
    type Params = F;

    fn try_new<'state>(
        state: &'state mut GlobalState,
        params: Self::Params,
    ) -> Result<Self::Output<'state>, FetchError>
    where
        Self: Sized,
    {
        let Fetched {
            read: (input,),
            write: (),
            empty: (),
        } = state.fetch()?;

        Ok(Self::Output {
            input,
            function: params,
            _marker: PhantomData,
        })
    }
}

impl<T, F> Pass for SimpleFunctionPassImpl<'_, T, F>
where
    T: 'static,
    F: FnOnce(&T) -> PassResult + Clone,
{
    fn run(self: Box<Self>) -> PassResult {
        (self.function)(self.input)
    }
}
