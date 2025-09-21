use std::cell::RefMut;

use crate::{
    ast::{
        ArrayExpression, AstVisitor, FunctionCallExpression, Program, StructExpression,
        StructMemberDefinition, StructMemberValue, StructType,
    },
    passes::{
        last_token_mapper::LastTokenMapper,
        pass_manager::{GlobalState, Pass, PassFactory, PassResult},
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
    fn partial_visit_struct_type(
        &mut self,
        StructType {
            struct_token: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        }: &mut StructType,
    ) {
        for (
            StructMemberDefinition {
                name: _,
                name_token: _,
                colon_token: _,
                type_,
            },
            _comma,
        ) in &mut *members
        {
            self.visit_type(type_);
        }

        if let Some((
            StructMemberDefinition {
                name: _,
                name_token: _,
                colon_token: _,
                type_,
            },
            comma @ None,
        )) = members.last_mut()
        {
            let mut replace_pass = LastTokenMapper::new(|token| {
                (
                    std::mem::take(&mut token.trailing_trivia),
                    token.range.end,
                    token.file_name.clone(),
                )
            });
            replace_pass.visit_type(type_);
            let (trivia, token_end, file_name) = replace_pass.result().unwrap();

            *comma = Some(Token {
                type_: TokenType::Comma,
                range: token_end.until(token_end),
                leading_trivia: vec![],
                trailing_trivia: trivia,
                file_name,
            });
        }
    }
    fn partial_visit_struct_expression(
        &mut self,
        StructExpression {
            type_,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        }: &mut StructExpression,
    ) {
        self.visit_type(type_);
        for (
            StructMemberValue {
                name: _,
                name_token: _,
                colon_token: _,
                value,
            },
            _comma,
        ) in &mut *members
        {
            self.visit_expression(value);
        }

        if let Some((
            StructMemberValue {
                name: _,
                name_token: _,
                colon_token: _,
                value,
            },
            comma @ None,
        )) = members.last_mut()
        {
            let mut replace_pass = LastTokenMapper::new(|token| {
                (
                    std::mem::take(&mut token.trailing_trivia),
                    token.range.end,
                    token.file_name.clone(),
                )
            });
            replace_pass.visit_expression(value);
            let (trivia, token_end, file_name) = replace_pass.result().unwrap();

            *comma = Some(Token {
                type_: TokenType::Comma,
                range: token_end.until(token_end),
                leading_trivia: vec![],
                trailing_trivia: trivia,
                file_name,
            });
        }
    }
}
