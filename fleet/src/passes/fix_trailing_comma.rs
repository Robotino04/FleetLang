use std::{cell::RefMut, ops::ControlFlow};

use crate::{
    ast::{
        ArrayExpression, FunctionCallExpression, Program, StructExpression, StructMemberDefinition,
        StructMemberValue, StructType,
    },
    passes::{
        partial_visitor::AstWalker,
        pass_manager::{GlobalState, Pass, PassFactory, PassResult},
        token_accessors::get_last_token,
    },
    tokenizer::{Token, TokenType},
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

        let _ = self.visit_program(&mut program);

        Ok(())
    }
}

impl PartialAstVisitor for FixTrailingComma<'_> {
    type Break<'a> = ();

    fn visit_function_call_expression(
        &mut self,
        fcall: &mut FunctionCallExpression,
    ) -> ControlFlow<()> {
        if let Some((arg, comma)) = fcall.arguments.last_mut() {
            if let Some(mut comma) = comma.clone()
                && let Some(token) = get_last_token(arg)
            {
                token.trailing_trivia.append(&mut comma.leading_trivia);
                token.trailing_trivia.append(&mut comma.trailing_trivia);
            }
            *comma = None;
        }

        self.walk_function_call_expression(fcall)
    }
    fn visit_array_expression(&mut self, expr: &mut ArrayExpression) -> ControlFlow<()> {
        if let Some((item, comma)) = expr.elements.last_mut() {
            if let Some(mut comma) = comma.clone()
                && let Some(token) = get_last_token(item)
            {
                token.trailing_trivia.append(&mut comma.leading_trivia);
                token.trailing_trivia.append(&mut comma.trailing_trivia);
            }
            *comma = None;
        }

        self.walk_array_expression(expr)
    }
    fn visit_struct_type(&mut self, ty: &mut StructType) -> ControlFlow<()> {
        if let Some((
            StructMemberDefinition {
                name: _,
                name_token: _,
                colon_token: _,
                type_,
            },
            comma @ None,
        )) = ty.members.last_mut()
        {
            let (trivia, token_end) = get_last_token(type_)
                .map(|token| {
                    (
                        std::mem::take(&mut token.trailing_trivia),
                        token.range.end(),
                    )
                })
                .unwrap();

            *comma = Some(Token {
                type_: TokenType::Comma,
                range: token_end.clone().until(token_end),
                leading_trivia: vec![],
                trailing_trivia: trivia,
            });
        }

        self.walk_struct_type(ty)
    }
    fn visit_struct_expression(&mut self, expr: &mut StructExpression) -> ControlFlow<()> {
        if let Some((
            StructMemberValue {
                name: _,
                name_token: _,
                colon_token: _,
                value,
            },
            comma @ None,
        )) = expr.members.last_mut()
        {
            let (trivia, token_end) = get_last_token(value)
                .map(|token| {
                    (
                        std::mem::take(&mut token.trailing_trivia),
                        token.range.end(),
                    )
                })
                .unwrap();

            *comma = Some(Token {
                type_: TokenType::Comma,
                range: token_end.clone().until(token_end),
                leading_trivia: vec![],
                trailing_trivia: trivia,
            });
        }

        self.walk_struct_expression(expr)
    }
}
