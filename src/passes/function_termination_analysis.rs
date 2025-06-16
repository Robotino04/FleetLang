use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BoolExpression, BoolType, BreakStatement, CastExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GroupingExpression, GroupingLValue, IdkType, IfStatement, IntType,
        NumberExpression, OnStatement, PerNodeData, Program, ReturnStatement, SelfExecutorHost,
        SimpleBinding, SkipStatement, StatementFunctionBody, ThreadExecutor, UnaryExpression,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    tokenizer::SourceLocation,
};

use super::find_node_bonds::find_node_bounds;

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
    type FunctionBodyOutput = FunctionTermination;
    type SimpleBindingOutput = FunctionTermination;
    type StatementOutput = FunctionTermination;
    type ExecutorHostOutput = FunctionTermination;
    type ExecutorOutput = FunctionTermination;
    type ExpressionOutput = FunctionTermination;
    type LValueOutput = FunctionTermination;
    type TypeOutput = FunctionTermination;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }

        if let Some(main_function) = program.functions.iter().find(|f| f.name == "main") {
            self.termination.insert_node(
                program,
                *self
                    .termination
                    .get_node(main_function)
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
        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }
        let body_termination = self.visit_function_body(body);
        self.termination.insert_node(function, body_termination);
        return body_termination;
    }

    fn visit_statement_function_body(
        &mut self,
        statement_function_body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        let StatementFunctionBody { statement, id: _ } = statement_function_body;
        let term = self.visit_statement(statement);
        self.termination.insert_node(statement_function_body, term);
        return term;
    }

    fn visit_extern_function_body(
        &mut self,
        extern_function_body: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        let ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token: _,
            id: _,
        } = extern_function_body;

        let term = FunctionTermination::MaybeTerminates;
        self.termination.insert_node(extern_function_body, term);
        return term;
    }

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let term = if let Some((_colon, type_)) = &mut simple_binding.type_ {
            self.visit_type(type_)
        } else {
            FunctionTermination::DoesntTerminate
        };
        self.termination.insert_node(simple_binding, term);
        return term;
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let exp_term = self.visit_expression(&mut expr_stmt.expression);
        self.termination.insert_node(expr_stmt, exp_term);
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

        self.termination.insert_node(on_stmt, term);
        return term;
    }

    fn visit_block_statement(&mut self, block_stmt: &mut BlockStatement) -> Self::StatementOutput {
        let mut body_term = FunctionTermination::DoesntTerminate;
        let mut unreachable_range = None;
        for stmt in &mut block_stmt.body {
            if body_term == FunctionTermination::Terminates {
                if let Some((prev_start, _prev_end)) = unreachable_range {
                    unreachable_range = Some((prev_start, find_node_bounds(stmt.clone()).1));
                } else {
                    unreachable_range = Some(find_node_bounds(stmt.clone()));
                }
            }
            body_term = body_term.or(self.visit_statement(stmt));
        }
        if let Some((start, end)) = unreachable_range {
            self.errors.push(FleetError {
                start,
                end,
                message: "This code is unreachable".to_string(),
                severity: ErrorSeverity::Warning,
            });
        }
        self.termination.insert_node(block_stmt, body_term);
        return body_term;
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        if let Some(retvalue) = &mut return_stmt.value {
            self.visit_expression(retvalue);
        }
        self.termination
            .insert_node(return_stmt, FunctionTermination::Terminates);
        return FunctionTermination::Terminates;
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.visit_simple_binding(&mut vardef_stmt.binding);
        let term = self.visit_expression(&mut vardef_stmt.value);
        self.termination.insert_node(vardef_stmt, term);
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
        subterms = if_stmt
            .else_
            .as_mut()
            .map(|(_else_token, else_body)| subterms.and(self.visit_statement(else_body)))
            .unwrap_or(subterms.and(FunctionTermination::DoesntTerminate));
        let term = if_term.or(subterms);
        self.termination.insert_node(if_stmt, term);
        return term;
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        let con_term = self.visit_expression(&mut while_stmt.condition);
        let body_term = self.visit_statement(&mut while_stmt.body);
        let term = con_term.or(body_term);
        self.termination.insert_node(while_stmt, term);
        return term;
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        let init_term = self.visit_statement(&mut for_stmt.initializer);

        let con_term = for_stmt
            .condition
            .as_mut()
            .map(|condition| self.visit_expression(condition))
            .unwrap_or(FunctionTermination::DoesntTerminate);
        let inc_term = for_stmt
            .incrementer
            .as_mut()
            .map(|incrementer| self.visit_expression(incrementer))
            .unwrap_or(FunctionTermination::DoesntTerminate);

        let body_term = self.visit_statement(&mut for_stmt.body);
        let term = init_term.or(con_term).or(inc_term).or(body_term);
        self.termination.insert_node(for_stmt, term);
        return term;
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        self.termination
            .insert_node(break_stmt, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        self.termination
            .insert_node(skip_stmt, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.termination
            .insert_node(executor_host, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        let host_term = self.visit_executor_host(&mut executor.host);
        let index_term = self.visit_expression(&mut executor.index);
        let term = host_term.or(index_term);
        self.termination.insert_node(executor, term);
        return term;
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        self.termination
            .insert_node(expression, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_bool_expression(&mut self, expression: &mut BoolExpression) -> Self::ExpressionOutput {
        self.termination
            .insert_node(expression, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        let mut term = FunctionTermination::DoesntTerminate;
        for (item, _comma) in &mut expression.elements {
            term = term.or(self.visit_expression(item));
        }

        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let mut term = FunctionTermination::DoesntTerminate;
        for (arg, _comma) in &mut expression.arguments {
            term = term.or(self.visit_expression(arg));
        }
        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_array_index_expression(
        &mut self,
        expression: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        let mut term = self.visit_expression(&mut expression.array);
        term = term.or(self.visit_expression(&mut expression.index));
        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        let term = self.visit_expression(&mut expression.subexpression);
        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        self.termination
            .insert_node(expression, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let term = self.visit_expression(&mut expression.operand);
        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        let operand_term = self.visit_expression(&mut expression.operand);
        let type_term = self.visit_type(&mut expression.type_);
        let term = operand_term.or(type_term);
        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        let term = self
            .visit_expression(&mut expression.left)
            .or(self.visit_expression(&mut expression.right));
        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let term = self
            .visit_lvalue(&mut expression.lvalue)
            .or(self.visit_expression(&mut expression.right));
        self.termination.insert_node(expression, term);
        return term;
    }

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        self.termination
            .insert_node(lvalue, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput {
        let term = self
            .visit_lvalue(&mut lvalue.array)
            .or(self.visit_expression(&mut lvalue.index));
        self.termination.insert_node(lvalue, term);
        return term;
    }

    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput {
        let term = self.visit_lvalue(&mut lvalue.sublvalue);
        self.termination.insert_node(lvalue, term);
        return term;
    }

    fn visit_int_type(&mut self, type_: &mut IntType) -> Self::TypeOutput {
        self.termination
            .insert_node(type_, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        self.termination
            .insert_node(unit_type, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_bool_type(&mut self, bool_type: &mut BoolType) -> Self::TypeOutput {
        self.termination
            .insert_node(bool_type, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        self.termination
            .insert_node(idk_type, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }

    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput {
        self.termination
            .insert_node(array_type, FunctionTermination::DoesntTerminate);
        return FunctionTermination::DoesntTerminate;
    }
}
