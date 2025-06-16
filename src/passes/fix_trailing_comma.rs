use crate::{
    ast::{ArrayExpression, AstVisitor, FunctionCallExpression},
    infra::FleetError,
    parser::IdGenerator,
};

use super::{add_trailing_trivia_pass::AddTrailingTriviaPass, partial_visitor::PartialAstVisitor};

pub struct FixTrailingComma<'a> {
    _errors: &'a mut Vec<FleetError>,
    _id_generator: &'a mut IdGenerator,
}

impl<'a> FixTrailingComma<'a> {
    pub fn new(errors: &'a mut Vec<FleetError>, id_generator: &'a mut IdGenerator) -> Self {
        Self {
            _errors: errors,
            _id_generator: id_generator,
        }
    }
}

impl PartialAstVisitor for FixTrailingComma<'_> {
    fn partial_visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }: &mut FunctionCallExpression,
    ) {
        for (arg, _comma) in &mut *arguments {
            self.visit_expression(arg);
        }

        if let Some((arg, comma)) = arguments.last_mut() {
            if let Some(mut token) = comma.clone() {
                token.leading_trivia.append(&mut token.trailing_trivia);
                AddTrailingTriviaPass::new(token.leading_trivia).visit_expression(arg);
            }
            *comma = None;
        }
    }
    fn partial_visit_array_expression(
        &mut self,
        ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayExpression,
    ) {
        for (item, _comma) in &mut *elements {
            self.visit_expression(item);
        }

        if let Some((item, comma)) = elements.last_mut() {
            if let Some(mut token) = comma.clone() {
                token.leading_trivia.append(&mut token.trailing_trivia);
                AddTrailingTriviaPass::new(token.leading_trivia).visit_expression(item);
            }
            *comma = None;
        }
    }
}
