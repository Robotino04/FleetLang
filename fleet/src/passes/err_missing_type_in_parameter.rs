use std::{cell::RefMut, ops::ControlFlow};

use crate::{
    ast::{FunctionDefinition, Program},
    error_reporting::{ErrorKind, Errors, SymbolDefinition},
    passes::{
        partial_visitor::AstWalker,
        pass_manager::{Pass, PassFactory, PassResult},
    },
};

use super::{partial_visitor::PartialAstVisitor, pass_manager::GlobalState};

pub struct ErrMissingTypeInParam<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,
}

impl PassFactory for ErrMissingTypeInParam<'_> {
    type Output<'state> = ErrMissingTypeInParam<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        Ok(Self::Output {
            errors: state.get_mut_named::<Errors>()?,
            program: Some(state.get_mut_named::<Program>()?),
        })
    }
}
impl Pass for ErrMissingTypeInParam<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = &mut self.program.take().unwrap();

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
