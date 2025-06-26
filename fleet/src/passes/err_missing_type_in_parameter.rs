use crate::{
    ast::{AstVisitor, FunctionDefinition},
    infra::{ErrorSeverity, FleetError},
};

use super::partial_visitor::PartialAstVisitor;

pub struct ErrMissingTypeInParam<'a> {
    errors: &'a mut Vec<FleetError>,
}

impl<'a> ErrMissingTypeInParam<'a> {
    pub fn new(errors: &'a mut Vec<FleetError>) -> Self {
        Self { errors }
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
                    param.clone(),
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
