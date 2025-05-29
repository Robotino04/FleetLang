use crate::{
    ast::{
        AstVisitor, BlockStatement, ExpressionStatement, FunctionDefinition, I32Type, IfStatement,
        OnStatement, PerNodeData, Program, ReturnStatement, SelfExecutorHost, ThreadExecutor,
        VariableDefinitionStatement,
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
    type ProgramOutput = PerNodeData<FunctionTermination>;
    type FunctionDefinitionOutput = FunctionTermination;
    type StatementOutput = FunctionTermination;
    type ExecutorHostOutput = FunctionTermination;
    type ExecutorOutput = FunctionTermination;
    type ExpressionOutput = FunctionTermination;
    type TypeOutput = FunctionTermination;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
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

    fn visit_function_definition(
        &mut self,
        function: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        let FunctionDefinition {
            return_type, body, ..
        } = function;
        self.visit_type(return_type);
        let body_termination = self.visit_statement(body);
        self.termination.insert(function, body_termination);
        return body_termination;
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let exp_term = self.visit_expression(&mut expr_stmt.expression);
        self.termination.insert(expr_stmt, exp_term);
        return exp_term;
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        let exec_term = self.visit_executor(&mut on_stmt.executor);
        let body_term = self.visit_statement(&mut on_stmt.body);
        if exec_term == FunctionTermination::Terminates {
            self.errors.push(FleetError::from_node(
                *on_stmt.body.clone(),
                "This code is unreachable",
                ErrorSeverity::Warning,
            ));
        }

        let term = exec_term.or(body_term);

        self.termination.insert(on_stmt, term);
        return term;
    }

    fn visit_block_statement(&mut self, block_stmt: &mut BlockStatement) -> Self::StatementOutput {
        let mut body_term = FunctionTermination::DoesntTerminate;
        for stmt in &mut block_stmt.body {
            if body_term == FunctionTermination::Terminates {
                self.errors.push(FleetError::from_node(
                    stmt.clone(),
                    "This code is unreachable",
                    ErrorSeverity::Warning,
                ));
            }
            body_term = body_term.or(self.visit_statement(stmt));
        }
        self.termination.insert(block_stmt, body_term);
        return body_term;
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        self.visit_expression(&mut return_stmt.value);
        self.termination
            .insert(return_stmt, FunctionTermination::Terminates);
        return FunctionTermination::Terminates;
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.visit_type(&mut vardef_stmt.type_);
        let term = self.visit_expression(&mut vardef_stmt.value);
        self.termination.insert(vardef_stmt, term);
        return term;
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        let if_term = self.visit_expression(&mut if_stmt.condition);
        let mut subterms = self.visit_statement(&mut if_stmt.if_body);
        for (_elif_token, elif_condition, elif_body) in &mut if_stmt.elifs {
            subterms = subterms.and(
                self.visit_expression(elif_condition)
                    .or(self.visit_statement(elif_body)),
            );
        }
        if let Some((_else_token, else_body)) = &mut if_stmt.else_ {
            subterms = subterms.and(self.visit_statement(else_body));
        } else {
            subterms = subterms.and(FunctionTermination::DoesntTerminate);
        }
        let term = if_term.or(subterms);
        self.termination.insert(if_stmt, term);
        return term;
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.termination
            .insert(executor_host, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        let host_term = self.visit_executor_host(&mut executor.host);
        let index_term = self.visit_expression(&mut executor.index);
        let term = host_term.or(index_term);
        self.termination.insert(executor, term);
        return term;
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut crate::ast::NumberExpression,
    ) -> Self::ExpressionOutput {
        self.termination
            .insert(expression, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut crate::ast::FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let mut term = FunctionTermination::DoesntTerminate;
        for arg in &mut expression.arguments {
            term = term.or(self.visit_expression(arg));
        }
        self.termination.insert(expression, term);
        return term;
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut crate::ast::GroupingExpression,
    ) -> Self::ExpressionOutput {
        let term = self.visit_expression(&mut expression.subexpression);
        self.termination.insert(expression, term);
        return term;
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut crate::ast::VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.termination
            .insert(expression, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut crate::ast::UnaryExpression,
    ) -> Self::ExpressionOutput {
        let term = self.visit_expression(&mut expression.operand);
        self.termination.insert(expression, term);
        return term;
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut crate::ast::BinaryExpression,
    ) -> Self::ExpressionOutput {
        let term = self
            .visit_expression(&mut expression.left)
            .or(self.visit_expression(&mut expression.right));
        self.termination.insert(expression, term);
        return term;
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut crate::ast::VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let term = self.visit_expression(&mut expression.right);
        self.termination.insert(expression, term);
        return term;
    }

    fn visit_i32_type(&mut self, type_: &mut I32Type) -> Self::TypeOutput {
        self.termination
            .insert(type_, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }
}
