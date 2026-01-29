use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    rc::Rc,
    vec::Vec,
};

use itertools::Itertools;
use log::info;

use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, IdkType, IfStatement,
        LiteralExpression, OnStatement, OnStatementIterator, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody,
        StructAccessExpression, StructAccessLValue, StructExpression, StructMemberDefinition,
        StructMemberValue, StructType, ThreadExecutor, TopLevelStatement, TypeAlias,
        UnaryExpression, UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    passes::{
        pass_manager::{
            ConcreteFunctionData, ConcreteVariableData, Errors, GlobalState, Pass, PassFactory,
            PassResult, StatData,
        },
        runtime_type::ConcreteRuntimeType,
        scope_analysis::{ConcreteFunction, ConcreteVariable, FunctionID},
    },
    tokenizer::{NamedSourceRange, SourceLocation},
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
    pub functions: Vec<Rc<RefCell<ConcreteFunction>>>,
    pub used_variables: Vec<Rc<RefCell<ConcreteVariable>>>,
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
    pub non_terminating_ranges: Vec<NamedSourceRange>,
    pub uses_gpu: YesNoMaybe,
    pub accessed_items: AccessRecord,
}

impl NodeStats {
    pub fn default_with_range(ntr: Vec<NamedSourceRange>) -> Self {
        Self {
            terminates_function: YesNoMaybe::No,
            non_terminating_ranges: ntr,
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord::default(),
        }
    }
}

impl MergableStat for NodeStats {
    fn serial(self, other: Self) -> Self {
        Self {
            terminates_function: self.terminates_function.serial(other.terminates_function),
            non_terminating_ranges: if other.terminates_function == YesNoMaybe::Yes
                || self.terminates_function == YesNoMaybe::Yes
            {
                vec![]
            } else if other.non_terminating_ranges.is_empty() {
                self.non_terminating_ranges
            } else {
                other.non_terminating_ranges
            },
            uses_gpu: self.uses_gpu.serial(other.uses_gpu),
            accessed_items: self.accessed_items.serial(other.accessed_items),
        }
    }
    fn parallel(mut self, mut other: Self) -> Self {
        self.non_terminating_ranges
            .append(&mut other.non_terminating_ranges);
        Self {
            terminates_function: self.terminates_function.parallel(other.terminates_function),
            non_terminating_ranges: self.non_terminating_ranges,
            uses_gpu: self.uses_gpu.parallel(other.uses_gpu),
            accessed_items: self.accessed_items.parallel(other.accessed_items),
        }
    }
}

pub struct StatTracker<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,

    variable_data: Ref<'state, ConcreteVariableData>,
    function_data: Ref<'state, ConcreteFunctionData>,

    stats: RefMut<'state, StatData>,

    function_stats: HashMap<FunctionID, NodeStats>,
    current_function: Option<Rc<RefCell<ConcreteFunction>>>,
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
        let function_data = state.check_named()?;

        let stats = state.insert_default::<StatData>();

        Ok(Self::Output {
            errors: errors.get_mut(state),
            program: Some(program.get_mut(state)),

            variable_data: variable_data.get(state),
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
    type TopLevelOutput = NodeStats;
    type FunctionBodyOutput = NodeStats;
    type SimpleBindingOutput = NodeStats;
    type StatementOutput = NodeStats;
    type ExecutorHostOutput = NodeStats;
    type ExecutorOutput = NodeStats;
    type ExpressionOutput = NodeStats;
    type LValueOutput = NodeStats;
    type TypeOutput = NodeStats;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        let mut stats = NodeStats {
            terminates_function: YesNoMaybe::No,
            non_terminating_ranges: vec![],
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord::default(),
        };
        let mut prev_stats = self.stats.clone();

        loop {
            let err_backup = self.errors.clone();

            for tls in &mut program.top_level_statements {
                let NodeStats {
                    terminates_function,
                    non_terminating_ranges: _,
                    uses_gpu,
                    accessed_items: _,
                } = self.visit_top_level_statement(tls);

                if let TopLevelStatement::Function(FunctionDefinition { name, .. }) = tls
                    && name == "main"
                {
                    stats.terminates_function = terminates_function;
                }
                stats.uses_gpu = stats.uses_gpu.serial(uses_gpu);
            }
            if prev_stats == **self.stats {
                break;
            } else {
                prev_stats = self.stats.clone();
                // don't duplicate errors when running multiple times
                *self.errors = err_backup;
                info!("Stats aren't stable yet.")
            }
        }

        if let Some(main_function) = program
            .top_level_statements
            .iter()
            .find_map(|tls| match tls {
                TopLevelStatement::Function(function_definition)
                    if function_definition.name == "main" =>
                {
                    Some(function_definition)
                }
                _ => None,
            })
        {
            let main_stat = self
                .stats
                .get(&main_function.id)
                .expect("All functions should have been analyzed by now")
                .clone();
            self.stats.insert(program.id, main_stat);
        } else {
            self.errors.push(FleetError::from_range(
                SourceLocation::start()
                    .named(program.file_name.clone())
                    .until(
                        program
                            .top_level_statements
                            .first()
                            .map(|tls| find_node_bounds(tls).range.start)
                            .unwrap_or(SourceLocation::start()),
                    ),
                "No main function was found.".to_string(),
                ErrorSeverity::Error,
            ));
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
    ) -> Self::TopLevelOutput {
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

    fn visit_type_alias(
        &mut self,
        TypeAlias {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            type_,
            semicolon_token: _,
            id,
        }: &mut TypeAlias,
    ) -> Self::TopLevelOutput {
        let stats = self.visit_type(type_);
        self.stats.insert(*id, stats.clone());

        stats
    }

    fn visit_statement_function_body(
        &mut self,
        body: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        let StatementFunctionBody { statement, id } = body;
        let stat = self.visit_statement(statement);

        assert_eq!(
            (stat.terminates_function == YesNoMaybe::Yes),
            stat.non_terminating_ranges.is_empty(),
            "non_terminating_ranges must be in agreement about termination with terminates_function"
        );
        if !stat.non_terminating_ranges.is_empty() {
            let current_function = self
                .current_function
                .as_ref()
                .expect("Function body analyzed for termination without a containing function");

            if current_function.borrow().return_type != ConcreteRuntimeType::Unit {
                self.errors.push({
                    FleetError::try_new(
                        stat.non_terminating_ranges
                            .iter()
                            .cloned()
                            .map(|r| (r, ErrorSeverity::Error))
                            .collect_vec(),
                        "All code paths must return.",
                        ErrorSeverity::Error,
                    )
                    .unwrap()
                });
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
            non_terminating_ranges: vec![],
            uses_gpu: YesNoMaybe::Maybe,
            accessed_items: AccessRecord::default(),
        };
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_simple_binding(&mut self, binding: &mut SimpleBinding) -> Self::SimpleBindingOutput {
        let bounds = find_node_bounds(&*binding);
        let SimpleBinding {
            name_token: _,
            name: _,
            type_,
            id,
        } = binding;

        let stat = if let Some((_colon, type_)) = type_ {
            self.visit_type(type_)
        } else {
            NodeStats::default_with_range(vec![])
        }
        .serial(NodeStats {
            terminates_function: YesNoMaybe::No,
            non_terminating_ranges: vec![bounds],
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
        stmt: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let bounds = find_node_bounds(&*stmt);
        let ExpressionStatement {
            expression,
            semicolon_token: _,
            id,
        } = stmt;
        let exp_stat = self
            .visit_expression(expression)
            .serial(NodeStats::default_with_range(vec![bounds]));
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

        let mut it_stats = NodeStats::default_with_range(vec![]);

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

        let mut binding_stats = NodeStats::default_with_range(vec![]);
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

    fn visit_block_statement(&mut self, stmt: &mut BlockStatement) -> Self::StatementOutput {
        let bounds = find_node_bounds(&*stmt);
        let BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token,
            id,
        } = stmt;

        let mut body_stat = NodeStats::default_with_range(vec![bounds]);
        let mut unreachable_range: Option<NamedSourceRange> = None;
        for stmt in body {
            if body_stat.terminates_function == YesNoMaybe::Yes {
                if let Some(prev_range) = unreachable_range {
                    unreachable_range = Some(prev_range.extend_with(find_node_bounds(&*stmt)));
                } else {
                    unreachable_range = Some(find_node_bounds(&*stmt));
                }
            }
            body_stat = body_stat.serial(self.visit_statement(stmt));
        }
        if body_stat.non_terminating_ranges.is_empty()
            && body_stat.terminates_function != YesNoMaybe::Yes
        {
            body_stat = body_stat.serial(NodeStats::default_with_range(vec![
                close_brace_token.range.clone(),
            ]));
        }
        if let Some(range) = unreachable_range {
            self.errors.push(FleetError::from_range(
                range,
                "This code is unreachable".to_string(),
                ErrorSeverity::Warning,
            ));
        }
        self.stats.insert(*id, body_stat.clone());
        body_stat
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement) -> Self::StatementOutput {
        let bounds = find_node_bounds(&*stmt);
        let ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id,
        } = stmt;

        let stat = match value {
            Some(retvalue) => self.visit_expression(retvalue),
            None => NodeStats::default_with_range(vec![]),
        }
        .serial(NodeStats {
            terminates_function: YesNoMaybe::Yes,
            non_terminating_ranges: vec![bounds],
            uses_gpu: YesNoMaybe::No,
            accessed_items: AccessRecord::default(),
        });
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_variable_definition_statement(
        &mut self,
        stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        let bounds = find_node_bounds(&*stmt);
        let VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id,
        } = stmt;
        self.visit_simple_binding(binding);
        let stat = self
            .visit_expression(value)
            .serial(NodeStats::default_with_range(vec![bounds]));
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_if_statement(&mut self, stmt: &mut IfStatement) -> Self::StatementOutput {
        let bounds = find_node_bounds(&*stmt);
        let IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id,
        } = stmt;

        let mut if_stats = self.visit_expression(condition);
        if_stats = if_stats.serial(self.visit_statement(if_body));
        for (_elif_token, elif_condition, elif_body) in elifs {
            if_stats = if_stats.parallel(
                self.visit_expression(elif_condition)
                    .serial(self.visit_statement(elif_body)),
            );
        }

        if let Some((_else_token, else_body)) = else_ {
            if_stats = if_stats.parallel(self.visit_statement(else_body));
        } else {
            // always merge with empty to indicate that this if statement isn't complete
            if_stats = if_stats.parallel(NodeStats::default_with_range(vec![
                bounds.end().prev_inline().until(bounds.end()),
            ]));
        }

        if if_stats.terminates_function == YesNoMaybe::No {
            // if non of the branches return, this if-statement takes all the blame
            if_stats = if_stats.serial(NodeStats::default_with_range(vec![bounds]))
        }

        self.stats.insert(*id, if_stats.clone());
        if_stats
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
            second_semicolon_token,
            incrementer,
            close_paren_token,
            body,
            id,
        }: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        let init_stat = self.visit_statement(initializer);

        let con_stat = condition
            .as_mut()
            .map(|condition| self.visit_expression(condition))
            .unwrap_or(NodeStats::default_with_range(vec![
                second_semicolon_token.range.clone(),
            ]));
        let inc_stat = incrementer
            .as_mut()
            .map(|incrementer| self.visit_expression(incrementer))
            .unwrap_or(NodeStats::default_with_range(vec![
                close_paren_token.range.clone(),
            ]));

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
        let stat = NodeStats::default_with_range(vec![find_node_bounds(&*break_stmt)]);
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
        let stat = NodeStats::default_with_range(vec![find_node_bounds(&*skip_stmt)]);
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
        let stat = NodeStats::default_with_range(vec![find_node_bounds(&*executor_host)]);
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

    fn visit_gpu_executor(&mut self, executor: &mut GPUExecutor) -> Self::ExecutorOutput {
        let bounds = find_node_bounds(&*executor);
        let GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index,
            close_bracket_token: _,
            id,
        } = executor;
        let stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            non_terminating_ranges: vec![bounds],
            uses_gpu: YesNoMaybe::Yes,
            accessed_items: AccessRecord::default(),
        }
        .serial(self.visit_executor_host(host))
        .serial(self.visit_expression(gpu_index));
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_literal_expression(&mut self, expr: &mut LiteralExpression) -> Self::ExpressionOutput {
        let bounds = find_node_bounds(&*expr);
        let LiteralExpression {
            value: _,
            token: _,
            id,
        } = expr;

        let stat = NodeStats::default_with_range(vec![bounds]);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_array_expression(&mut self, expr: &mut ArrayExpression) -> Self::ExpressionOutput {
        let bounds = find_node_bounds(&*expr);
        let ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id,
        } = expr;
        let mut stat = NodeStats::default_with_range(vec![bounds]);
        for (item, _comma) in elements {
            stat = stat.serial(self.visit_expression(item));
        }

        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_struct_expression(&mut self, expr: &mut StructExpression) -> Self::ExpressionOutput {
        let bounds = find_node_bounds(&*expr);
        let StructExpression {
            type_,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id,
        } = expr;
        let mut stat = NodeStats::default_with_range(vec![bounds]);
        stat = stat.serial(self.visit_type(type_));
        for (
            StructMemberValue {
                name: _,
                name_token: _,
                colon_token: _,
                value,
            },
            _comma,
        ) in members
        {
            stat = stat.serial(self.visit_expression(value));
        }

        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_function_call_expression(
        &mut self,
        function_call: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let bounds = find_node_bounds(&*function_call);
        let FunctionCallExpression {
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        } = function_call;
        let mut stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            non_terminating_ranges: vec![bounds],
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
        expr: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        let bounds = find_node_bounds(&*expr);
        let CompilerExpression {
            at_token: _,
            name: _,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        } = expr;

        let mut stat = NodeStats::default_with_range(vec![bounds]);
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

    fn visit_struct_access_expression(
        &mut self,
        StructAccessExpression {
            value,
            dot_token: _,
            member_name: _,
            member_name_token: _,
            id,
        }: &mut StructAccessExpression,
    ) -> Self::ExpressionOutput {
        let stat = self.visit_expression(value);
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
        expr: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let bounds = find_node_bounds(&*expr);
        let VariableAccessExpression {
            name: _,
            name_token: _,
            id,
        } = expr;
        let stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            non_terminating_ranges: vec![bounds],
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

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        let bounds = find_node_bounds(&*lvalue);
        let VariableLValue {
            name: _,
            name_token: _,
            id,
        } = lvalue;
        let stat = NodeStats {
            terminates_function: YesNoMaybe::No,
            non_terminating_ranges: vec![bounds],
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

    fn visit_struct_access_lvalue(
        &mut self,
        StructAccessLValue {
            value,
            dot_token: _,
            member_name: _,
            member_name_token: _,
            id,
        }: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        let stat = self.visit_lvalue(value);
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

    fn visit_simple_type(&mut self, type_: &mut SimpleType) -> Self::TypeOutput {
        let bounds = find_node_bounds(&*type_);
        let SimpleType {
            token: _,
            type_: _,
            id,
        } = type_;
        let stat = NodeStats::default_with_range(vec![bounds]);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_unit_type(&mut self, type_: &mut UnitType) -> Self::TypeOutput {
        let bounds = find_node_bounds(&*type_);
        let UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id,
        } = type_;
        let stat = NodeStats::default_with_range(vec![bounds]);
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_idk_type(&mut self, type_: &mut IdkType) -> Self::TypeOutput {
        let bounds = find_node_bounds(&*type_);
        let IdkType { token: _, id } = type_;
        let stat = NodeStats::default_with_range(vec![bounds]);
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

    fn visit_struct_type(
        &mut self,
        StructType {
            struct_token,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id,
        }: &mut StructType,
    ) -> Self::TypeOutput {
        let mut stat = NodeStats::default_with_range(vec![struct_token.range.clone()]);
        for (
            StructMemberDefinition {
                name: _,
                name_token: _,
                colon_token: _,
                type_,
            },
            _comma,
        ) in members
        {
            stat = stat.serial(self.visit_type(type_));
        }
        self.stats.insert(*id, stat.clone());
        stat
    }

    fn visit_alias_type(
        &mut self,
        AliasType {
            name: _,
            name_token,
            id,
        }: &mut AliasType,
    ) -> Self::TypeOutput {
        let stat = NodeStats::default_with_range(vec![name_token.range.clone()]);
        self.stats.insert(*id, stat.clone());
        stat
    }
}
