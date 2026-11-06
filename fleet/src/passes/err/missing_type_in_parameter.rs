use std::ops::ControlFlow;

use crate::{
    ast::{
        FunctionDefinition, Program,
        partial_visitor::{AstWalker, PartialAstVisitor},
    },
    error_reporting::{ErrorKind, Errors, SymbolDefinition},
    passes::prelude::*,
};

pub struct ErrMissingTypeInParam<'state> {
    errors: &'state mut Errors,
    program: Option<&'state mut Program>,
}

impl PassFactory for ErrMissingTypeInParam<'_> {
    type Output<'state> = ErrMissingTypeInParam<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, FetchError>
    where
        Self: Sized,
    {
        let Fetched {
            read: (),
            write: (errors, program),
            empty: (),
        } = state.fetch()?;

        Ok(Self::Output {
            errors,
            program: Some(program),
        })
    }
}
impl Pass for ErrMissingTypeInParam<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = self.program.take().unwrap();

        let _ = self.visit_program(program);

        Ok(())
    }
}

impl PartialAstVisitor for ErrMissingTypeInParam<'_> {
    type Break<'a> = ();

    // may get lifted in the future if lambdas are implemented
    fn visit_function_definition(&mut self, fdef: &mut FunctionDefinition) -> ControlFlow<()> {
        for (param, _comma) in &fdef.parameters {
            if param.type_.is_none() {
                self.errors.push(ErrorKind::FunctionParameterUntyped {
                    function: SymbolDefinition::from_token(fdef.name.clone(), &fdef.name_token),
                    parameter: SymbolDefinition::from_token(param.name.clone(), &param.name_token),
                });
            }
        }

        self.walk_function_definition(fdef)
    }
}
