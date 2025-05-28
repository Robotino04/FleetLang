use crate::{
    ast::{
        AstVisitor, BlockStatement, Executor, ExecutorHost, Expression, ExpressionStatement,
        FunctionDefinition, IfStatement, OnStatement, Program, ReturnStatement, Type,
        VariableDefinitionStatement,
    },
    tokenizer::{Token, Trivia},
};

pub struct AddTrailingTriviaPass {
    new_trivia: Vec<Trivia>,
}

impl AddTrailingTriviaPass {
    pub fn new(new_trivia: Vec<Trivia>) -> Self {
        Self { new_trivia }
    }

    fn add_trailing_trivia_to_token(&mut self, token: &mut Token) {
        token.trailing_trivia.extend(self.new_trivia.clone());
        self.new_trivia.clear();
    }
}

impl AstVisitor for AddTrailingTriviaPass {
    type SubOutput = ();
    type Output = ();

    fn visit_program(mut self, program: &mut Program) {
        if let Some(f) = program.functions.last_mut() {
            self.visit_function_definition(f);
        }
    }

    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.visit_statement(&mut function_definition.body);
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            semicolon_token, ..
        }: &mut ExpressionStatement,
    ) -> Self::SubOutput {
        self.add_trailing_trivia_to_token(semicolon_token);
    }

    fn visit_on_statement(
        &mut self,
        OnStatement { body, .. }: &mut OnStatement,
    ) -> Self::SubOutput {
        self.visit_statement(body);
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            close_brace_token, ..
        }: &mut BlockStatement,
    ) -> Self::SubOutput {
        self.add_trailing_trivia_to_token(close_brace_token);
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement {
            semicolon_token, ..
        }: &mut ReturnStatement,
    ) -> Self::SubOutput {
        self.add_trailing_trivia_to_token(semicolon_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            semicolon_token, ..
        }: &mut VariableDefinitionStatement,
    ) -> Self::SubOutput {
        self.add_trailing_trivia_to_token(semicolon_token);
    }

    fn visit_if_statement(
        &mut self,
        IfStatement {
            if_body,
            elifs,
            else_,
            ..
        }: &mut IfStatement,
    ) -> Self::SubOutput {
        if let Some((_, else_body)) = else_ {
            self.visit_statement(else_body);
        } else if let Some((_, _, body)) = elifs.last_mut() {
            self.visit_statement(body);
        } else {
            self.visit_statement(if_body);
        }
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_ { token, id: _ } => {
                self.add_trailing_trivia_to_token(token);
            }
        }
    }

    fn visit_executor(&mut self, executor: &mut Executor) {
        match executor {
            Executor::Thread {
                host: _,
                dot_token: _,
                thread_token: _,
                open_bracket_token: _,
                index: _,
                close_bracket_token,
                id: _,
            } => {
                self.add_trailing_trivia_to_token(close_bracket_token);
            }
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        match expression {
            Expression::Number { token, .. } => {
                self.add_trailing_trivia_to_token(token);
            }
            Expression::VariableAccess { name_token, .. } => {
                self.add_trailing_trivia_to_token(name_token);
            }
            Expression::FunctionCall {
                close_paren_token, ..
            } => {
                self.add_trailing_trivia_to_token(close_paren_token);
            }
            Expression::Grouping {
                close_paren_token, ..
            } => {
                self.add_trailing_trivia_to_token(close_paren_token);
            }
            Expression::Unary { operand, .. } => {
                self.visit_expression(&mut *operand);
            }
            Expression::Binary { right, .. } => {
                self.visit_expression(&mut *right);
            }
            Expression::VariableAssignment { right, .. } => {
                self.visit_expression(&mut *right);
            }
        }
    }

    fn visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { token, id: _ } => {
                self.add_trailing_trivia_to_token(token);
            }
        }
    }
}
