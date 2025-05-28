use crate::{
    ast::{
        AstVisitor, BlockStatement, Executor, ExecutorHost, Expression, ExpressionStatement,
        FunctionDefinition, IfStatement, OnStatement, Program, ReturnStatement, Type,
        VariableDefinitionStatement,
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
    type SubOutput = ();

    fn visit_program(mut self, program: &mut Program) {
        if let Some(f) = program.functions.first_mut() {
            self.visit_function_definition(f);
        }
    }

    fn visit_function_definition(&mut self, function_definition: &mut FunctionDefinition) {
        self.add_leading_trivia_to_token(&mut function_definition.name_token);
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement { expression, .. }: &mut ExpressionStatement,
    ) -> Self::SubOutput {
        self.visit_expression(expression);
    }

    fn visit_on_statement(
        &mut self,
        OnStatement { on_token, .. }: &mut OnStatement,
    ) -> Self::SubOutput {
        self.add_leading_trivia_to_token(on_token);
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token, ..
        }: &mut BlockStatement,
    ) -> Self::SubOutput {
        self.add_leading_trivia_to_token(open_brace_token);
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement { return_token, .. }: &mut ReturnStatement,
    ) -> Self::SubOutput {
        self.add_leading_trivia_to_token(return_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement { let_token, .. }: &mut VariableDefinitionStatement,
    ) -> Self::SubOutput {
        self.add_leading_trivia_to_token(let_token);
    }

    fn visit_if_statement(
        &mut self,
        IfStatement { if_token, .. }: &mut IfStatement,
    ) -> Self::SubOutput {
        self.add_leading_trivia_to_token(if_token);
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_ { token, id: _ } => {
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
            Type::I32 { token, id: _ } => {
                self.add_leading_trivia_to_token(token);
            }
        }
    }
}
