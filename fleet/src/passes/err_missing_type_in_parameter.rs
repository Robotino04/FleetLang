use std::cell::RefMut;

use crate::{
    ast::{AstVisitor, FunctionDefinition, Program},
    infra::{ErrorSeverity, FleetError},
    passes::pass_manager::{Errors, Pass, PassFactory, PassResult},
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

        self.visit_program(program);

        Ok(())
    }
}

impl PartialAstVisitor for ErrMissingTypeInParam<'_> {
    // may get lifted in the future if lambdas are implemented
    fn partial_visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) {
        for (param, _comma) in parameters {
            self.visit_simple_binding(param);
            if param.type_.is_none() {
                self.errors.push(FleetError::from_node(
                    param,
                    "Function parameters must always have a type",
                    ErrorSeverity::Error,
                ));
            }
        }

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }

        self.visit_function_body(body);
    }
}
