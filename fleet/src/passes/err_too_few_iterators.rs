use std::{cell::RefMut, ops::ControlFlow};

use crate::{
    ast::{OnStatement, Program},
    error_reporting::{ErrorKind, Errors},
    passes::{
        partial_visitor::AstWalker,
        pass_manager::{GlobalState, Pass, PassFactory, PassResult},
    },
};

use super::partial_visitor::PartialAstVisitor;

pub struct ErrTooFewIterators<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,
}
impl PassFactory for ErrTooFewIterators<'_> {
    type Output<'state> = ErrTooFewIterators<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        Ok(Self::Output {
            errors: state.get_mut_named::<Errors>()?,
            program: Some(state.get_mut_named::<Program>()?),
        })
    }
}
impl Pass for ErrTooFewIterators<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();

        let _ = self.visit_program(&mut program);

        Ok(())
    }
}

impl PartialAstVisitor for ErrTooFewIterators<'_> {
    type Break<'a> = ();

    fn visit_on_statement(&mut self, stmt: &mut OnStatement) -> ControlFlow<()> {
        if stmt.iterators.is_empty() {
            self.errors.push(ErrorKind::OnStatementMissingIterator {
                on_range: stmt.on_token.range.clone(),
            });
        }

        self.walk_on_statement(stmt)
    }
}
