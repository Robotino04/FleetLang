use crate::{
    ast::{
        AstVisitor, Executor, ExecutorHost, Expression, FunctionDefinition, PerNodeData, Program,
        Statement, Type,
    },
    infra::{ErrorSeverity, FleetError},
    tokenizer::SourceLocation,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FunctionTermination {
    Terminates,
    DoesntTerminate,
    MaybeTerminates,
}
impl FunctionTermination {
    fn or(&self, other: FunctionTermination) -> FunctionTermination {
        match (self, other) {
            (FunctionTermination::Terminates, _) => FunctionTermination::Terminates,
            (_, FunctionTermination::Terminates) => FunctionTermination::Terminates,
            (FunctionTermination::DoesntTerminate, FunctionTermination::DoesntTerminate) => {
                FunctionTermination::DoesntTerminate
            }
            _ => FunctionTermination::MaybeTerminates,
        }
    }
    fn and(&self, other: FunctionTermination) -> FunctionTermination {
        match (self, other) {
            (FunctionTermination::DoesntTerminate, FunctionTermination::DoesntTerminate) => {
                FunctionTermination::DoesntTerminate
            }
            (FunctionTermination::Terminates, FunctionTermination::Terminates) => {
                FunctionTermination::Terminates
            }
            _ => FunctionTermination::MaybeTerminates,
        }
    }
}

pub struct FunctionTerminationAnalyzer<'errors> {
    termination: PerNodeData<FunctionTermination>,
    errors: &'errors mut Vec<FleetError>,
}

impl<'errors> FunctionTerminationAnalyzer<'errors> {
    pub fn new(error_output: &'errors mut Vec<FleetError>) -> Self {
        Self {
            termination: PerNodeData::new(),
            errors: error_output,
        }
    }
}

impl<'errors> AstVisitor for FunctionTerminationAnalyzer<'errors> {
    type SubOutput = FunctionTermination;
    type Output = PerNodeData<FunctionTermination>;

    fn visit_program(mut self, program: &mut Program) -> Self::Output {
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }

        if let Some(main_function) = program.functions.iter().find(|f| f.name == "main") {
            self.termination.insert(
                program,
                *self
                    .termination
                    .get(main_function)
                    .expect("All functions should have been analyzed by now"),
            );
        } else {
            self.errors.push(FleetError {
                start: SourceLocation::start(),
                end: program
                    .functions
                    .first()
                    .map_or(SourceLocation::start(), |f| f.close_paren_token.end),
                message: format!("No main function was found."),
                severity: ErrorSeverity::Error,
            });
        }
        return self.termination;
    }

    fn visit_function_definition(&mut self, function: &mut FunctionDefinition) -> Self::SubOutput {
        let FunctionDefinition {
            return_type, body, ..
        } = function;
        self.visit_type(return_type);
        let body_termination = self.visit_statement(body);
        self.termination.insert(function, body_termination);
        return body_termination;
    }

    fn visit_statement(&mut self, statement: &mut Statement) -> Self::SubOutput {
        match statement {
            Statement::Expression { expression, .. } => {
                let exp_term = self.visit_expression(expression);
                self.termination.insert(statement, exp_term);
                return exp_term;
            }
            Statement::On { executor, body, .. } => {
                let exec_term = self.visit_executor(executor);
                let body_term = self.visit_statement(body);
                if exec_term == FunctionTermination::Terminates {
                    self.errors.push(FleetError::from_node(
                        *body.clone(),
                        "This code is unreachable",
                        ErrorSeverity::Warning,
                    ));
                }

                let term = exec_term.or(body_term);

                self.termination.insert(statement, term);
                return term;
            }
            Statement::Block { body, .. } => {
                let mut body_term = FunctionTermination::DoesntTerminate;
                for stmt in body {
                    if body_term == FunctionTermination::Terminates {
                        self.errors.push(FleetError::from_node(
                            stmt.clone(),
                            "This code is unreachable",
                            ErrorSeverity::Warning,
                        ));
                    }
                    body_term = body_term.or(self.visit_statement(stmt));
                }
                self.termination.insert(statement, body_term);
                return body_term;
            }
            Statement::Return { value, .. } => {
                self.visit_expression(value);
                self.termination
                    .insert(statement, FunctionTermination::Terminates);
                return FunctionTermination::Terminates;
            }
            Statement::VariableDefinition { type_, value, .. } => {
                self.visit_type(type_);
                let term = self.visit_expression(value);
                self.termination.insert(statement, term);
                return term;
            }
            Statement::If {
                condition,
                if_body,
                elifs,
                else_,
                ..
            } => {
                let if_term = self.visit_expression(condition);
                let mut subterms = self.visit_statement(if_body);
                for (_elif_token, elif_condition, elif_body) in elifs {
                    subterms = subterms.and(
                        self.visit_expression(elif_condition)
                            .or(self.visit_statement(elif_body)),
                    );
                }
                if let Some((_else_token, else_body)) = else_ {
                    subterms = subterms.and(self.visit_statement(else_body));
                } else {
                    subterms = subterms.and(FunctionTermination::DoesntTerminate);
                }
                let term = if_term.or(subterms);
                self.termination.insert(statement, term);
                return term;
            }
        }
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) -> Self::SubOutput {
        match executor_host {
            ExecutorHost::Self_ { .. } => {
                self.termination
                    .insert(executor_host, FunctionTermination::DoesntTerminate);
                return FunctionTermination::DoesntTerminate;
            }
        }
    }

    fn visit_executor(&mut self, executor: &mut Executor) -> Self::SubOutput {
        match executor {
            Executor::Thread { host, index, .. } => {
                let host_term = self.visit_executor_host(host);
                let index_term = self.visit_expression(index);
                let term = host_term.or(index_term);
                self.termination.insert(executor, term);
                return term;
            }
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) -> Self::SubOutput {
        match expression {
            Expression::Number { value: _, .. } => {
                self.termination
                    .insert(expression, FunctionTermination::DoesntTerminate);
                return FunctionTermination::DoesntTerminate;
            }
            Expression::VariableAccess { name: _, .. } => {
                self.termination
                    .insert(expression, FunctionTermination::DoesntTerminate);
                return FunctionTermination::DoesntTerminate;
            }
            Expression::FunctionCall {
                name: _, arguments, ..
            } => {
                let mut term = FunctionTermination::DoesntTerminate;
                for arg in arguments {
                    term = term.or(self.visit_expression(arg));
                }
                self.termination.insert(expression, term);
                return term;
            }
            Expression::Grouping { subexpression, .. } => {
                let term = self.visit_expression(subexpression);
                self.termination.insert(expression, term);
                return term;
            }
            Expression::Unary { operand, .. } => {
                let term = self.visit_expression(operand);
                self.termination.insert(expression, term);
                return term;
            }
            Expression::Binary { left, right, .. } => {
                let term = self.visit_expression(left).or(self.visit_expression(right));
                self.termination.insert(expression, term);
                return term;
            }
            Expression::VariableAssignment { name: _, right, .. } => {
                let term = self.visit_expression(right);
                self.termination.insert(expression, term);
                return term;
            }
        }
    }

    fn visit_type(&mut self, type_: &mut Type) -> Self::SubOutput {
        match type_ {
            Type::I32 { .. } => {
                self.termination
                    .insert(type_, FunctionTermination::DoesntTerminate);
                return FunctionTermination::DoesntTerminate;
            }
        }
    }
}
