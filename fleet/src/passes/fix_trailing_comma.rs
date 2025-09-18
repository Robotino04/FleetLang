use std::cell::RefMut;

use crate::{
    ast::{ArrayExpression, AstVisitor, FunctionCallExpression, Program},
    passes::{
        last_token_mapper::LastTokenMapper,
        pass_manager::{GlobalState, Pass, PassFactory, PassResult},
    },
};

use super::partial_visitor::PartialAstVisitor;

pub struct FixTrailingComma<'state> {
    program: Option<RefMut<'state, Program>>,
}
impl PassFactory for FixTrailingComma<'_> {
    type Output<'state> = FixTrailingComma<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        Ok(Self::Output {
            program: Some(state.get_mut_named::<Program>()?),
        })
    }
}
impl Pass for FixTrailingComma<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        self.visit_program(&mut program);

        Ok(())
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
            if let Some(mut comma) = comma.clone() {
                LastTokenMapper::new(|token| {
                    token.trailing_trivia.append(&mut comma.leading_trivia);
                    token.trailing_trivia.append(&mut comma.trailing_trivia);
                })
                .visit_expression(arg);
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
            if let Some(mut comma) = comma.clone() {
                LastTokenMapper::new(|token| {
                    token.trailing_trivia.append(&mut comma.leading_trivia);
                    token.trailing_trivia.append(&mut comma.trailing_trivia);
                })
                .visit_expression(item);
            }
            *comma = None;
        }
    }
}
