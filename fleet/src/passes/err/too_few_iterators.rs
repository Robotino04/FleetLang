use std::ops::ControlFlow;

use crate::{
    ast::{
        OnStatement, Program,
        partial_visitor::{AstWalker, PartialAstVisitor},
    },
    error_reporting::{ErrorKind, Errors},
    passes::prelude::*,
};

pub struct ErrTooFewIterators<'state> {
    errors: &'state mut Errors,
    program: Option<&'state mut Program>,
}
impl PassFactory for ErrTooFewIterators<'_> {
    type Output<'state> = ErrTooFewIterators<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, FetchError> {
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
impl Pass for ErrTooFewIterators<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = self.program.take().unwrap();

        let _ = self.visit_program(program);

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
