use crate::{
    ast::{
        AstVisitor, Executor, ExecutorHost, Expression, FunctionDefinition, Program, Statement,
        Type,
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
    fn visit_program(&mut self, program: &mut Program) {
        if let Some(f) = program.functions.last_mut() {
            self.visit_function_definition(f);
        }
    }

    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.visit_statement(&mut function_definition.body);
    }

    fn visit_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Expression {
                expression: _,
                semicolon_token,
            } => {
                self.add_trailing_trivia_to_token(semicolon_token);
            }
            Statement::On {
                on_token: _,
                open_paren_token: _,
                executor: _,
                close_paren_token: _,
                body,
            } => {
                self.visit_statement(body);
            }
            Statement::Block {
                close_brace_token, ..
            } => {
                self.add_trailing_trivia_to_token(close_brace_token);
            }
            Statement::Return {
                semicolon_token, ..
            } => {
                self.add_trailing_trivia_to_token(semicolon_token);
            }
        }
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_ { token } => {
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
            Expression::FunctionCall {
                name: _,
                name_token: _,
                open_paren_token: _,
                arguments: _,
                close_paren_token,
            } => {
                self.add_trailing_trivia_to_token(close_paren_token);
            }
            Expression::Grouping {
                close_paren_token, ..
            } => {
                self.add_trailing_trivia_to_token(close_paren_token);
            }
            Expression::Unary {
                operator_token: _,
                operation: _,
                operand,
            } => {
                self.visit_expression(&mut *operand);
            }
            Expression::Binary { right, .. } => {
                self.visit_expression(&mut *right);
            }
        }
    }

    fn visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { token } => {
                self.add_trailing_trivia_to_token(token);
            }
        }
    }
}
