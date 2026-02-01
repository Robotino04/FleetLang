use std::cell::RefMut;

use crate::{
    ast::{AstVisitor, OnStatement, Program},
    infra::ErrorKind,
    passes::pass_manager::{Errors, GlobalState, Pass, PassFactory, PassResult},
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
        self.visit_program(&mut program);

        Ok(())
    }
}

impl PartialAstVisitor for ErrTooFewIterators<'_> {
    fn partial_visit_on_statement(&mut self, stmt: &mut OnStatement) {
        let OnStatement {
            on_token: _,
            executor: _,
            iterators,
            open_paren_token: _,
            bindings: _,
            close_paren_token: _,
            body: _,
            id: _,
        } = stmt;

        if iterators.is_empty() {
            self.errors.push(ErrorKind::OnStatementMissingIterator {
                on_range: stmt.on_token.range.clone(),
            });
        }
    }
}
