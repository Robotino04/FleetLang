use crate::{
    ast::{
        AstVisitor, Executor, ExecutorHost, Expression, FunctionDefinition, Program, Statement,
        Type,
    },
    tokenizer::{Token, Trivia},
};

pub struct AddLeadingTriviaPass {
    new_trivia: Vec<Trivia>,
}

impl AddLeadingTriviaPass {
    pub fn new(new_trivia: Vec<Trivia>) -> Self {
        Self { new_trivia }
    }

    fn add_leading_trivia_to_token(&mut self, token: &mut Token) {
        self.new_trivia.extend(token.leading_trivia.clone());
        token.leading_trivia = self.new_trivia.clone();
        self.new_trivia.clear();
    }
}

impl AstVisitor for AddLeadingTriviaPass {
    type Output = ();

    fn visit_program(&mut self, program: &mut Program) {
        if let Some(f) = program.functions.first_mut() {
            self.visit_function_definition(f);
        }
    }

    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.add_leading_trivia_to_token(&mut function_definition.name_token);
    }

    fn visit_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Expression { expression, .. } => {
                self.visit_expression(expression);
            }
            Statement::On { on_token, .. } => {
                self.add_leading_trivia_to_token(on_token);
            }
            Statement::Block {
                open_brace_token, ..
            } => {
                self.add_leading_trivia_to_token(open_brace_token);
            }
            Statement::Return { return_token, .. } => {
                self.add_leading_trivia_to_token(return_token);
            }
            Statement::VariableDefinition { let_token, .. } => {
                self.add_leading_trivia_to_token(let_token);
            }
        }
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_ { token } => {
                self.add_leading_trivia_to_token(token);
            }
        }
    }

    fn visit_executor(&mut self, executor: &mut Executor) {
        match executor {
            Executor::Thread { host, .. } => {
                self.visit_executor_host(host);
            }
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        match expression {
            Expression::Number { token, .. } => {
                self.add_leading_trivia_to_token(token);
            }
            Expression::FunctionCall { name_token, .. } => {
                self.add_leading_trivia_to_token(name_token);
            }
            Expression::Grouping {
                open_paren_token, ..
            } => {
                self.add_leading_trivia_to_token(open_paren_token);
            }
            Expression::VariableAccess { name_token, .. } => {
                self.add_leading_trivia_to_token(name_token);
            }
            Expression::Unary { operator_token, .. } => {
                self.add_leading_trivia_to_token(operator_token);
            }
            Expression::Binary { left, .. } => {
                self.visit_expression(&mut *left);
            }
            Expression::VariableAssignment { name_token, .. } => {
                self.add_leading_trivia_to_token(name_token);
            }
        }
    }

    fn visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { token } => {
                self.add_leading_trivia_to_token(token);
            }
        }
    }
}
