use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    rc::Rc,
    vec::Vec,
};

use log::info;

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LiteralExpression, OnStatement, OnStatementIterator, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody,
        ThreadExecutor, UnaryExpression, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    passes::{
        pass_manager::{
            Errors, FunctionData, GlobalState, Pass, PassFactory, PassResult, StatData, TypeSets,
            VariableData,
        },
        runtime_type::RuntimeType,
        scope_analysis::{Function, FunctionID, Variable},
    },
    tokenizer::SourceLocation,
};

use super::find_node_bounds::find_node_bounds;

pub trait MergableStat {
    fn serial(self, other: Self) -> Self;
    fn parallel(self, other: Self) -> Self;
}

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
impl MergableStat for YesNoMaybe {
    fn serial(self, other: YesNoMaybe) -> YesNoMaybe {
        match (self, other) {
            (YesNoMaybe::Yes, _) => YesNoMaybe::Yes,
            (_, YesNoMaybe::Yes) => YesNoMaybe::Yes,
            (YesNoMaybe::No, YesNoMaybe::No) => YesNoMaybe::No,
            _ => YesNoMaybe::Maybe,
        }
    }
    fn parallel(self, other: YesNoMaybe) -> YesNoMaybe {
        match (self, other) {
            (YesNoMaybe::No, YesNoMaybe::No) => YesNoMaybe::No,
            (YesNoMaybe::Yes, YesNoMaybe::Yes) => YesNoMaybe::Yes,
            _ => YesNoMaybe::Maybe,
        }
    }
}

impl YesNoMaybe {
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

#[derive(Clone, Debug, Default)]
pub struct AccessRecord {
    pub functions: Vec<Rc<RefCell<Function>>>,
    pub used_variables: Vec<Rc<RefCell<Variable>>>,
}

impl PartialEq for AccessRecord {
    fn eq(&self, other: &Self) -> bool {
        let mut self_functions = HashSet::new();
        self.functions.iter().for_each(|el| {
            self_functions.insert(el.borrow().id);
        });
        let mut other_functions = HashSet::new();
        other.functions.iter().for_each(|el| {
            other_functions.insert(el.borrow().id);
        });

        let mut self_variables = HashSet::new();
        self.used_variables.iter().for_each(|el| {
            self_variables.insert(el.borrow().id);
        });
        let mut other_variables = HashSet::new();
        other.used_variables.iter().for_each(|el| {
            other_variables.insert(el.borrow().id);
        });

        self_functions == other_functions && self_variables == other_variables
    }
}

impl Eq for AccessRecord {}

impl MergableStat for AccessRecord {
    fn serial(mut self, mut other: Self) -> Self {
        self.functions.append(&mut other.functions);
        let mut seen_functions = HashSet::new();
        self.functions
            .retain(|el| seen_functions.insert(el.borrow().id));

        self.used_variables.append(&mut other.used_variables);
        let mut seen_variables = HashSet::new();
        self.used_variables
            .retain(|el| seen_variables.insert(el.borrow().id));

        self
    }

    fn parallel(self, other: Self) -> Self {
        self.serial(other)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NodeStats {
    pub terminates_function: YesNoMaybe,
    pub uses_gpu: YesNoMaybe,
    pub accessed_items: AccessRecord,
}

impl NodeStats {
    pub fn nothing() -> Self {
        Self {
            terminates_function: YesNoMaybe::No,
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord::default(),
        }
    }
}

impl MergableStat for NodeStats {
    fn serial(self, other: Self) -> Self {
        Self {
            terminates_function: self.terminates_function.serial(other.terminates_function),
            uses_gpu: self.uses_gpu.serial(other.uses_gpu),
            accessed_items: self.accessed_items.serial(other.accessed_items),
        }
    }
    fn parallel(self, other: Self) -> Self {
        Self {
            terminates_function: self.terminates_function.parallel(other.terminates_function),
            uses_gpu: self.uses_gpu.parallel(other.uses_gpu),
            accessed_items: self.accessed_items.parallel(other.accessed_items),
        }
    }
}

pub struct StatTracker<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,

    variable_data: Ref<'state, VariableData>,
    type_sets: Ref<'state, TypeSets>,
    function_data: Ref<'state, FunctionData>,

    stats: RefMut<'state, StatData>,

    function_stats: HashMap<FunctionID, NodeStats>,
    current_function: Option<Rc<RefCell<Function>>>,
    loop_count: usize,
}

impl PassFactory for StatTracker<'_> {
    type Output<'state> = StatTracker<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        let errors = state.check_named()?;
        let program = state.check_named()?;
        let variable_data = state.check_named()?;
        let type_sets = state.check_named()?;
        let function_data = state.check_named()?;

        let stats = state.insert_default::<StatData>();

        Ok(Self::Output {
            errors: errors.get_mut(state),
            program: Some(program.get_mut(state)),

            variable_data: variable_data.get(state),
            type_sets: type_sets.get(state),
            function_data: function_data.get(state),

            stats: stats.get_mut(state),

            function_stats: HashMap::default(),
            current_function: None,
            loop_count: 0,
        })
    }
}
impl Pass for StatTracker<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        self.visit_program(&mut program);

        Ok(())
    }
}

impl AstVisitor for StatTracker<'_> {
    type ProgramOutput = ();
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
        let mut prev_stats = self.stats.clone();

        loop {
            for f in &mut program.functions {
                let NodeStats {
                    terminates_function,
                    uses_gpu,
                    accessed_items: _,
                } = self.visit_function_definition(f);

                if f.name == "main" {
                    stats.terminates_function = terminates_function;
                }
                stats.uses_gpu = stats.uses_gpu.serial(uses_gpu);
            }
            if prev_stats == **self.stats {
                break;
            } else {
                prev_stats = self.stats.clone();
                info!("Stats aren't stable yet")
            }
        }

        if let Some(main_function) = program.functions.iter().find(|f| f.name == "main") {
            let main_stat = self
                .stats
                .get(&main_function.id)
                .expect("All functions should have been analyzed by now")
                .clone();
            self.stats.insert(program.id, main_stat);
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
    }

    fn visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id,
        }: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        let referenced_function = self
            .function_data
            .get(id)
            .expect("function data must exist before calling function_termination_analyzer")
            .clone();
        self.current_function = Some(referenced_function.clone());

        for (param, _comma) in parameters {
            self.visit_simple_binding(param);
        }

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }
        let body_stat = self.visit_function_body(body);
        self.stats.insert(*id, body_stat.clone());
        self.function_stats
            .insert(referenced_function.borrow().id, body_stat.clone());

        self.current_function = None;

        body_stat
    }

    fn visit_statement_function_body(
        &mut self,
        body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        let body_clone = body.clone();
        let StatementFunctionBody { statement, id } = body;
        let stat = self.visit_statement(statement);

        if stat.terminates_function != YesNoMaybe::Yes {
            let current_function = self
                .current_function
                .as_ref()
                .expect("Function body analyzed for termination without a containing function");

            if *self
                .type_sets
                .get(current_function.borrow().return_type.unwrap())
                != RuntimeType::Unit
            {
                self.errors.push(FleetError::from_node(
                    &body_clone,
                    "All code paths must return.",
                    ErrorSeverity::Error,
                ));
            }
        }

        self.stats.insert(*id, stat.clone());
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
            id,
        } = extern_function_body;

        let stat = NodeStats {
            terminates_function: YesNoMaybe::Maybe,
            uses_gpu: YesNoMaybe::Maybe,
            accessed_items: AccessRecord::default(),
        };
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token: _,
            name: _,
            type_,
            id,
        }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let stat = if let Some((_colon, type_)) = type_ {
            self.visit_type(type_)
        } else {
            NodeStats::nothing()
        }
        .serial(NodeStats {
            terminates_function: YesNoMaybe::No,
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord {
                functions: vec![],
                used_variables: vec![],
            },
        });
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let exp_stat = self.visit_expression(expression);
        self.stats.insert(*id, exp_stat.clone());
        exp_stat
    }

    fn visit_on_statement(
        &mut self,
        OnStatement {
            on_token: _,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id,
        }: &mut OnStatement,
    ) -> Self::StatementOutput {
        let exec_stats = self.visit_executor(executor);

        let mut it_stats = NodeStats::nothing();

        for OnStatementIterator {
            open_bracket_token: _,
            binding,
            equal_token: _,
            max_value,
            close_bracket_token: _,
        } in iterators
        {
            it_stats = it_stats
                .serial(self.visit_simple_binding(binding))
                .serial(self.visit_expression(max_value));
        }

        let mut binding_stats = NodeStats::nothing();
        for (binding, _comma) in bindings {
            binding_stats = binding_stats.serial(self.visit_lvalue(binding));
        }

        if exec_stats.terminates_function == YesNoMaybe::Yes
            || binding_stats.terminates_function == YesNoMaybe::Yes
        {
            self.errors.push(FleetError::from_node(
                &**body,
                "This code is unreachable",
                ErrorSeverity::Warning,
            ));
        }
        let body_stats = self.visit_statement(body);

        let stats = exec_stats
            .serial(it_stats)
            .serial(binding_stats)
            .serial(body_stats);

        self.stats.insert(*id, stats.clone());
        stats
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id,
        }: &mut BlockStatement,
    ) -> Self::StatementOutput {
        let mut body_stat = NodeStats::nothing();
        let mut unreachable_range = None;
        for stmt in body {
            if body_stat.terminates_function == YesNoMaybe::Yes {
                if let Some((prev_start, _prev_end)) = unreachable_range {
                    unreachable_range = Some((prev_start, find_node_bounds(stmt).1));
                } else {
                    unreachable_range = Some(find_node_bounds(stmt));
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
        self.stats.insert(*id, body_stat.clone());
        body_stat
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id,
        }: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        let stat = match value {
            Some(retvalue) => self.visit_expression(retvalue),
            None => NodeStats::nothing(),
        }
        .serial(NodeStats {
            terminates_function: YesNoMaybe::Yes,
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord::default(),
        });
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id,
        }: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        self.visit_simple_binding(binding);
        let stat = self.visit_expression(value);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_if_statement(
        &mut self,
        IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id,
        }: &mut IfStatement,
    ) -> Self::StatementOutput {
        let if_stat = self.visit_expression(condition);
        let mut substats = self.visit_statement(if_body);
        for (_elif_token, elif_condition, elif_body) in elifs {
            substats = substats.parallel(
                self.visit_expression(elif_condition)
                    .serial(self.visit_statement(elif_body)),
            );
        }
        substats = if let Some((_else_token, else_body)) = else_ {
            substats.parallel(self.visit_statement(else_body))
        } else {
            substats.parallel(NodeStats::nothing())
        };
        let stat = if_stat.serial(substats);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id,
        }: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        let con_stat = self.visit_expression(condition);
        self.loop_count += 1;
        let body_stat = self.visit_statement(body);
        self.loop_count -= 1;
        let stat = con_stat.serial(body_stat);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_for_loop_statement(
        &mut self,
        ForLoopStatement {
            for_token: _,
            open_paren_token: _,
            initializer,
            condition,
            second_semicolon_token: _,
            incrementer,
            close_paren_token: _,
            body,
            id,
        }: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        let init_stat = self.visit_statement(initializer);

        let con_stat = condition
            .as_mut()
            .map(|condition| self.visit_expression(condition))
            .unwrap_or(NodeStats::nothing());
        let inc_stat = incrementer
            .as_mut()
            .map(|incrementer| self.visit_expression(incrementer))
            .unwrap_or(NodeStats::nothing());

        self.loop_count += 1;
        let body_stat = self.visit_statement(body);
        self.loop_count -= 1;
        let stat = init_stat
            .parallel(con_stat)
            .parallel(inc_stat)
            .parallel(body_stat);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        let stat = NodeStats::nothing();
        self.stats.insert(break_stmt.id, stat.clone());
        if self.loop_count == 0 {
            self.errors.push(FleetError::from_node(
                break_stmt,
                "Break statements cannot appear outside loops",
                ErrorSeverity::Error,
            ));
        }

        stat
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        let stat = NodeStats::nothing();
        self.stats.insert(skip_stmt.id, stat.clone());
        if self.loop_count == 0 {
            self.errors.push(FleetError::from_node(
                skip_stmt,
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
        self.stats.insert(executor_host.id, stat.clone());
        stat
    }

    fn visit_thread_executor(
        &mut self,
        ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        }: &mut ThreadExecutor,
    ) -> Self::ExecutorOutput {
        let host_stat = self.visit_executor_host(host);
        let index_stat = self.visit_expression(index);
        let stat = host_stat.serial(index_stat);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_gpu_executor(
        &mut self,
        GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index,
            close_bracket_token: _,
            id,
        }: &mut GPUExecutor,
    ) -> Self::ExecutorOutput {
        let stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            uses_gpu: YesNoMaybe::Yes,
            accessed_items: AccessRecord::default(),
        }
        .serial(self.visit_executor_host(host))
        .serial(self.visit_expression(gpu_index));
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_literal_expression(
        &mut self,
        LiteralExpression {
            value: _,
            token: _,
            id,
        }: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        let stat = NodeStats::nothing();
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_array_expression(
        &mut self,
        ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id,
        }: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        let mut stat = NodeStats::nothing();
        for (item, _comma) in elements {
            stat = stat.serial(self.visit_expression(item));
        }

        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        }: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let mut stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord {
                functions: self
                    .function_data
                    .get(id)
                    .map_or_else(Vec::new, |fd| vec![fd.clone()]),
                used_variables: vec![],
            },
        };
        if let Some(ref_function) = self.function_data.get(id)
            && let Some(stat2) = self.function_stats.get(&ref_function.borrow().id)
        {
            stat = stat.serial(stat2.clone());
        }
        stat.terminates_function = YesNoMaybe::No;

        for (arg, _comma) in arguments {
            stat = stat.serial(self.visit_expression(arg));
        }
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_compiler_expression(
        &mut self,
        CompilerExpression {
            at_token: _,
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        }: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        let mut stat = NodeStats::nothing();
        for (arg, _comma) in arguments {
            stat = stat.serial(self.visit_expression(arg));
        }
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_array_index_expression(
        &mut self,
        ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        }: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        let stat = self
            .visit_expression(array)
            .serial(self.visit_expression(index));
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_grouping_expression(
        &mut self,
        GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id,
        }: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        let stat = self.visit_expression(subexpression);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_variable_access_expression(
        &mut self,
        VariableAccessExpression {
            name: _,
            name_token: _,
            id,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord {
                functions: vec![],
                used_variables: self
                    .variable_data
                    .get(id)
                    .map_or_else(Vec::new, |vd| vec![vd.clone()]),
            },
        };
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_unary_expression(
        &mut self,
        UnaryExpression {
            operator_token: _,
            operation: _,
            operand,
            id,
        }: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let stat = self.visit_expression(operand);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_cast_expression(
        &mut self,
        CastExpression {
            operand,
            as_token: _,
            type_,
            id,
        }: &mut CastExpression,
    ) -> Self::ExpressionOutput {
        let operand_stat = self.visit_expression(operand);
        let type_stat = self.visit_type(type_);
        let stat = operand_stat.serial(type_stat);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_binary_expression(
        &mut self,
        BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id,
        }: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        let stat = self
            .visit_expression(left)
            .serial(self.visit_expression(right));
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let stat = self
            .visit_lvalue(lvalue)
            .serial(self.visit_expression(right));
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name: _,
            name_token: _,
            id,
        }: &mut VariableLValue,
    ) -> Self::LValueOutput {
        let stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord {
                functions: vec![],
                used_variables: self
                    .variable_data
                    .get(id)
                    .map_or_else(Vec::new, |vd| vec![vd.clone()]),
            },
        };
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        }: &mut ArrayIndexLValue,
    ) -> Self::LValueOutput {
        let stat = self
            .visit_lvalue(array)
            .serial(self.visit_expression(index));
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_grouping_lvalue(
        &mut self,
        GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id,
        }: &mut GroupingLValue,
    ) -> Self::LValueOutput {
        let stat = self.visit_lvalue(sublvalue);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_simple_type(
        &mut self,
        SimpleType {
            token: _,
            type_: _,
            id,
        }: &mut SimpleType,
    ) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id,
        }: &mut UnitType,
    ) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_idk_type(&mut self, IdkType { token: _, id }: &mut IdkType) -> Self::TypeOutput {
        let stat = NodeStats::nothing();
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_array_type(
        &mut self,
        ArrayType {
            subtype,
            open_bracket_token: _,
            size,
            close_bracket_token: _,
            id,
        }: &mut ArrayType,
    ) -> Self::TypeOutput {
        let mut stat = self.visit_type(subtype);
        if let Some(size) = size {
            stat = stat.serial(self.visit_expression(size))
        }
        self.stats.insert(*id, stat.clone());
        stat
    }
}
