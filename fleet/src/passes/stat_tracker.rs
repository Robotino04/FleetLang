use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BoolExpression, BoolType, BreakStatement, CastExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        IntType, NumberExpression, OnStatement, PerNodeData, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SkipStatement, StatementFunctionBody, ThreadExecutor,
        UnaryExpression, UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    passes::type_propagation::{
        Function, RuntimeType, TypeAnalysisData, UnionFindSet, UnionFindSetPtr, Variable,
    },
    tokenizer::SourceLocation,
};

use super::find_node_bonds::find_node_bounds;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FlRuntimeUsage {
    Used,
    NotUsed,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum YesNoMaybe {
    Yes,
    Maybe,
    No,
}
impl YesNoMaybe {
    pub fn serial(&self, other: YesNoMaybe) -> YesNoMaybe {
        match (self, other) {
            (YesNoMaybe::Yes, _) => YesNoMaybe::Yes,
            (_, YesNoMaybe::Yes) => YesNoMaybe::Yes,
            (YesNoMaybe::No, YesNoMaybe::No) => YesNoMaybe::No,
            _ => YesNoMaybe::Maybe,
        }
    }
    pub fn parallel(&self, other: YesNoMaybe) -> YesNoMaybe {
        match (self, other) {
            (YesNoMaybe::No, YesNoMaybe::No) => YesNoMaybe::No,
            (YesNoMaybe::Yes, YesNoMaybe::Yes) => YesNoMaybe::Yes,
            _ => YesNoMaybe::Maybe,
        }
    }

    pub fn at_least_maybe(self) -> bool {
        match self {
            YesNoMaybe::Yes => true,
            YesNoMaybe::Maybe => true,
            YesNoMaybe::No => false,
        }
    }
}

impl From<bool> for YesNoMaybe {
    fn from(value: bool) -> Self {
        match value {
            true => Self::Yes,
            false => Self::No,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct NodeStats {
    pub terminates_function: YesNoMaybe,
    pub uses_runtime: YesNoMaybe,
}

impl NodeStats {
    pub fn nothing() -> Self {
        Self {
            terminates_function: YesNoMaybe::No,
            uses_runtime: YesNoMaybe::No,
        }
    }

    pub fn serial(&self, other: Self) -> Self {
        Self {
            terminates_function: self.terminates_function.serial(other.terminates_function),
            uses_runtime: self.uses_runtime.serial(other.uses_runtime),
        }
    }
    pub fn parallel(&self, other: Self) -> Self {
        Self {
            terminates_function: self.terminates_function.parallel(other.terminates_function),
            uses_runtime: self.uses_runtime.parallel(other.uses_runtime),
        }
    }
}

pub struct StatTracker<'errors, 'inputs> {
    stats: PerNodeData<NodeStats>,
    errors: &'errors mut Vec<FleetError>,

    _type_data: &'inputs PerNodeData<UnionFindSetPtr<RuntimeType>>,
    type_sets: &'inputs UnionFindSet<RuntimeType>,
    _variable_data: &'inputs PerNodeData<Rc<RefCell<Variable>>>,
    function_data: &'inputs PerNodeData<Rc<RefCell<Function>>>,

    current_function: Option<Rc<RefCell<Function>>>,
    loop_count: usize,
}

impl<'errors, 'inputs> StatTracker<'errors, 'inputs> {
    pub fn new(
        error_output: &'errors mut Vec<FleetError>,
        analysis_data: &'inputs TypeAnalysisData,
    ) -> Self {
        Self {
            stats: PerNodeData::default(),
            errors: error_output,

            _type_data: &analysis_data.type_data,
            type_sets: &analysis_data.type_sets,
            _variable_data: &analysis_data.variable_data,
            function_data: &analysis_data.function_data,

            current_function: None,
            loop_count: 0,
        }
    }
}

impl AstVisitor for StatTracker<'_, '_> {
    type ProgramOutput = PerNodeData<NodeStats>;
    type FunctionDefinitionOutput = NodeStats;
    type FunctionBodyOutput = NodeStats;
    type SimpleBindingOutput = NodeStats;
    type StatementOutput = NodeStats;
    type ExecutorHostOutput = NodeStats;
    type ExecutorOutput = NodeStats;
    type ExpressionOutput = NodeStats;
    type LValueOutput = NodeStats;
    type TypeOutput = NodeStats;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        let mut stats = NodeStats::nothing();
        for f in &mut program.functions {
            let NodeStats {
                terminates_function,
                uses_runtime,
            } = self.visit_function_definition(f);

            if f.name == "main" {
                stats.terminates_function = terminates_function;
            }
            stats.uses_runtime = stats.uses_runtime.serial(uses_runtime);
        }

        if let Some(main_function) = program.functions.iter().find(|f| f.name == "main") {
            self.stats.insert_node(
                program,
                *self
                    .stats
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
                message: "No main function was found.".to_string(),
                severity: ErrorSeverity::Error,
            });
        }
        self.stats
    }

    fn visit_function_definition(
        &mut self,
        function: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        let FunctionDefinition {
            return_type,
            body,
            id,
            ..
        } = function;

        self.current_function = Some(
            self.function_data
                .get(id)
                .expect("function data must exist before calling function_termination_analyzer")
                .clone(),
        );

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }
        let body_stat = self.visit_function_body(body);
        self.stats.insert_node(function, body_stat);

        self.current_function = None;

        body_stat
    }

    fn visit_statement_function_body(
        &mut self,
        statement_function_body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        let StatementFunctionBody { statement, id: _ } = statement_function_body;
        let stat = self.visit_statement(statement);

        if stat.terminates_function != YesNoMaybe::Yes {
            let Some(current_function) = &self.current_function else {
                unreachable!(
                    "Function body analyzed for termination without a containing function"
                );
            };

            if *self.type_sets.get(current_function.borrow().return_type) != RuntimeType::Unit {
                self.errors.push(FleetError::from_node(
                    statement_function_body.clone(),
                    "All code paths must return.",
                    ErrorSeverity::Error,
                ));
            }
        }

        self.stats.insert_node(statement_function_body, stat);
        stat
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

        let stat = NodeStats {
            terminates_function: YesNoMaybe::Maybe,
            uses_runtime: YesNoMaybe::No,
        };
        self.stats.insert_node(extern_function_body, stat);
        stat
    }

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let stat = if let Some((_colon, type_)) = &mut simple_binding.type_ {
            self.visit_type(type_)
        } else {
            NodeStats::nothing()
        };
        self.stats.insert_node(simple_binding, stat);
        stat
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let exp_stat = self.visit_expression(&mut expr_stmt.expression);
        self.stats.insert_node(expr_stmt, exp_stat);
        exp_stat
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        let exec_stats = self.visit_executor(&mut on_stmt.executor);
        let body_stats = self.visit_statement(&mut on_stmt.body);
        if exec_stats.terminates_function == YesNoMaybe::Yes {
            self.errors.push(FleetError::from_node(
                *on_stmt.body.clone(),
                "This code is unreachable",
                ErrorSeverity::Warning,
            ));
        }

        let stats = exec_stats.parallel(body_stats);

        self.stats.insert_node(on_stmt, stats);
        stats
    }

    fn visit_block_statement(&mut self, block_stmt: &mut BlockStatement) -> Self::StatementOutput {
        let mut body_stat = NodeStats::nothing();
        let mut unreachable_range = None;
        for stmt in &mut block_stmt.body {
            if body_stat.terminates_function == YesNoMaybe::Yes {
                if let Some((prev_start, _prev_end)) = unreachable_range {
                    unreachable_range = Some((prev_start, find_node_bounds(stmt.clone()).1));
                } else {
                    unreachable_range = Some(find_node_bounds(stmt.clone()));
                }
            }
            body_stat = body_stat.serial(self.visit_statement(stmt));
        }
        if let Some((start, end)) = unreachable_range {
            self.errors.push(FleetError {
                start,
                end,
                message: "This code is unreachable".to_string(),
                severity: ErrorSeverity::Warning,
            });
        }
        self.stats.insert_node(block_stmt, body_stat);
        body_stat
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        if let Some(retvalue) = &mut return_stmt.value {
            self.visit_expression(retvalue);
        }
        let stat = NodeStats {
            terminates_function: YesNoMaybe::Yes,
            uses_runtime: YesNoMaybe::No,
        };
        self.stats.insert_node(return_stmt, stat);
        stat
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.visit_simple_binding(&mut vardef_stmt.binding);
        let stat = self.visit_expression(&mut vardef_stmt.value);
        self.stats.insert_node(vardef_stmt, stat);
        stat
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        let if_stat = self.visit_expression(&mut if_stmt.condition);
        let mut substats = self.visit_statement(&mut if_stmt.if_body);
        for (_elif_token, elif_condition, elif_body) in &mut if_stmt.elifs {
            substats = substats.parallel(
                self.visit_expression(elif_condition)
                    .serial(self.visit_statement(elif_body)),
            );
        }
        substats = if_stmt
            .else_
            .as_mut()
            .map(|(_else_token, else_body)| substats.parallel(self.visit_statement(else_body)))
            .unwrap_or(substats.parallel(NodeStats::nothing()));
        let stat = if_stat.serial(substats);
        self.stats.insert_node(if_stmt, stat);
        stat
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        let con_stat = self.visit_expression(&mut while_stmt.condition);
        self.loop_count += 1;
        let body_stat = self.visit_statement(&mut while_stmt.body);
        self.loop_count -= 1;
        let stat = con_stat.serial(body_stat);
        self.stats.insert_node(while_stmt, stat);
        stat
    }

    fn visit_for_loop_statement(
        &mut self,
        for_stmt: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        let init_stat = self.visit_statement(&mut for_stmt.initializer);

        let con_stat = for_stmt
            .condition
            .as_mut()
            .map(|condition| self.visit_expression(condition))
            .unwrap_or(NodeStats::nothing());
        let inc_stat = for_stmt
            .incrementer
            .as_mut()
            .map(|incrementer| self.visit_expression(incrementer))
            .unwrap_or(NodeStats::nothing());

        self.loop_count += 1;
        let body_stat = self.visit_statement(&mut for_stmt.body);
        self.loop_count -= 1;
        let stat = init_stat
            .parallel(con_stat)
            .parallel(inc_stat)
            .parallel(body_stat);
        self.stats.insert_node(for_stmt, stat);
        stat
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(break_stmt, stat);
        if self.loop_count == 0 {
            self.errors.push(FleetError::from_node(
                break_stmt.clone(),
                "Break statements cannot appear outside loops",
                ErrorSeverity::Error,
            ));
        }

        stat
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(skip_stmt, stat);
        if self.loop_count == 0 {
            self.errors.push(FleetError::from_node(
                skip_stmt.clone(),
                "Skip statements cannot appear outside loops",
                ErrorSeverity::Error,
            ));
        }

        stat
    }

    fn visit_self_executor_host(
        &mut self,
        executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(executor_host, stat);
        stat
    }

    fn visit_thread_executor(&mut self, executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        let host_stat = self.visit_executor_host(&mut executor.host);
        let index_stat = self.visit_expression(&mut executor.index);
        let stat = host_stat.serial(index_stat);
        self.stats.insert_node(executor, stat);
        stat
    }

    fn visit_gpu_executor(
        &mut self,
        GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token_1: _,
            gpu_index,
            close_bracket_token_1: _,
            open_bracket_token_2: _,
            iterator,
            equal_token: _,
            max_value,
            close_bracket_token_2: _,
            id,
        }: &mut GPUExecutor,
    ) -> Self::ExecutorOutput {
        let stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            uses_runtime: YesNoMaybe::Yes,
        }
        .serial(self.visit_executor_host(host))
        .serial(self.visit_expression(gpu_index))
        .serial(self.visit_simple_binding(iterator))
        .serial(self.visit_expression(max_value));
        self.stats.insert(*id, stat);
        stat
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_bool_expression(&mut self, expression: &mut BoolExpression) -> Self::ExpressionOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        let mut stat = NodeStats::nothing();
        for (item, _comma) in &mut expression.elements {
            stat = stat.serial(self.visit_expression(item));
        }

        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let mut stat = NodeStats::nothing();
        for (arg, _comma) in &mut expression.arguments {
            stat = stat.serial(self.visit_expression(arg));
        }
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_array_index_expression(
        &mut self,
        expression: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        let stat = self
            .visit_expression(&mut expression.array)
            .serial(self.visit_expression(&mut expression.index));
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        let stat = self.visit_expression(&mut expression.subexpression);
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let stat = self.visit_expression(&mut expression.operand);
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        let operand_stat = self.visit_expression(&mut expression.operand);
        let type_stat = self.visit_type(&mut expression.type_);
        let stat = operand_stat.serial(type_stat);
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        let stat = self
            .visit_expression(&mut expression.left)
            .serial(self.visit_expression(&mut expression.right));
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let stat = self
            .visit_lvalue(&mut expression.lvalue)
            .serial(self.visit_expression(&mut expression.right));
        self.stats.insert_node(expression, stat);
        stat
    }

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(lvalue, stat);
        stat
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput {
        let stat = self
            .visit_lvalue(&mut lvalue.array)
            .serial(self.visit_expression(&mut lvalue.index));
        self.stats.insert_node(lvalue, stat);
        stat
    }

    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput {
        let stat = self.visit_lvalue(&mut lvalue.sublvalue);
        self.stats.insert_node(lvalue, stat);
        stat
    }

    fn visit_int_type(&mut self, type_: &mut IntType) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(type_, stat);
        stat
    }

    fn visit_unit_type(&mut self, unit_type: &mut UnitType) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(unit_type, stat);
        stat
    }

    fn visit_bool_type(&mut self, bool_type: &mut BoolType) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(bool_type, stat);
        stat
    }

    fn visit_idk_type(&mut self, idk_type: &mut IdkType) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(idk_type, stat);
        stat
    }

    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert_node(array_type, stat);
        stat
    }
}
