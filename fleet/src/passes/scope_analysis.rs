use std::{
    cell::{RefCell, RefMut},
    collections::{HashMap, VecDeque},
    hash::Hash,
    rc::Rc,
};

use itertools::Itertools;
use thiserror::Error;

use crate::{
    NewtypeDeref,
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, HasID, IdkType,
        IfStatement, LiteralExpression, NodeID, OnStatement, OnStatementIterator, PerNodeData,
        Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement,
        StatementFunctionBody, StructAccessExpression, StructAccessLValue, StructExpression,
        StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor, TopLevelStatement,
        TypeAlias, UnaryExpression, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    error_reporting::{
        DuplicateKind, ErrorKind, Errors, NotDefinedKind, SymbolDefinition, UnresolvedSymbol,
    },
    parser::IdGenerator,
    passes::{
        find_node_bounds::find_node_bounds,
        pass_manager::{
            FunctionData, GlobalState, Pass, PassFactory, PassResult, ScopeData, VariableData,
        },
        runtime_type::{ConcreteRuntimeType, RuntimeType},
        union_find_set::UnionFindSetPtr,
    },
};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct VariableID(pub u64);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionID(pub u64);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ScopeID(pub u64);

#[derive(Clone, Debug)]
pub struct Variable {
    pub symbol: SymbolDefinition,
    pub type_: Option<UnionFindSetPtr<RuntimeType>>,
    pub is_constant: bool,
    pub id: VariableID,
    pub definition_node_id: NodeID,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub symbol: SymbolDefinition,
    pub return_type: Option<UnionFindSetPtr<RuntimeType>>,
    pub parameter_types: Option<Vec<Rc<RefCell<Variable>>>>,
    pub id: FunctionID,
    pub definition_node_id: NodeID,
}

#[derive(Clone, Debug)]
pub struct VariableScope {
    pub id: ScopeID,
    pub variable_map: HashMap<String, Rc<RefCell<Variable>>>,
    pub parent_references: HashMap<String, Rc<RefCell<Variable>>>,
    pub copy_from_parent: bool,
    pub is_read_guard: bool,
    pub is_write_guard: bool,
    pub parent: Option<Rc<RefCell<VariableScope>>>,
}

#[derive(Clone, Debug)]
pub struct ConcreteVariable {
    pub symbol: SymbolDefinition,
    pub type_: ConcreteRuntimeType,
    pub is_constant: bool,
    pub id: VariableID,
    pub definition_node_id: NodeID,
}

#[derive(Clone, Debug)]
pub struct ConcreteFunction {
    pub symbol: SymbolDefinition,
    pub return_type: ConcreteRuntimeType,
    pub parameter_types: Vec<Rc<RefCell<ConcreteVariable>>>,
    pub id: FunctionID,
    pub definition_node_id: NodeID,
}

#[derive(Clone, Debug)]
pub struct ConcreteVariableScope {
    pub id: ScopeID,
    pub variable_map: HashMap<String, Rc<RefCell<ConcreteVariable>>>,
    pub parent_references: HashMap<String, Rc<RefCell<ConcreteVariable>>>,
    pub copy_from_parent: bool,
    pub is_read_guard: bool,
    pub is_write_guard: bool,
    pub parent: Option<Rc<RefCell<ConcreteVariableScope>>>,
}

impl VariableScope {
    fn new(id: ScopeID) -> Self {
        Self {
            id,
            parent_references: Default::default(),
            variable_map: Default::default(),
            is_write_guard: Default::default(),
            parent: Default::default(),
            is_read_guard: Default::default(),
            copy_from_parent: Default::default(),
        }
    }

    pub fn vars_and_refs(&self) -> HashMap<String, Rc<RefCell<Variable>>> {
        let mut map = self.variable_map.clone();
        map.extend(self.parent_references.clone().drain());
        map
    }
}

impl ConcreteVariableScope {
    pub fn vars_and_refs(&self) -> HashMap<String, Rc<RefCell<ConcreteVariable>>> {
        let mut map = self.variable_map.clone();
        map.extend(self.parent_references.clone().drain());
        map
    }
}

pub struct VariableScopeStack {
    pub current: Rc<RefCell<VariableScope>>,
}
pub struct ConcreteVariableScopeReader {
    current: Rc<RefCell<ConcreteVariableScope>>,
}

#[derive(Debug, Error)]
#[error(
    "Variable named {:?} was already present.", present_var.borrow().symbol
)]
pub struct ScopeInsertAlreadyPresentError {
    present_var: Rc<RefCell<Variable>>,
}

impl ConcreteVariableScopeReader {
    pub fn from_scope(current: Rc<RefCell<ConcreteVariableScope>>) -> Self {
        Self { current }
    }

    /// access a variable for reading without checking if a parent reference for it exists
    pub fn get_read_noref(&self, name: &str) -> Option<Rc<RefCell<ConcreteVariable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_read_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }
    /// access a variable for writing without checking if a parent reference for it exists
    pub fn get_write_noref(&self, name: &str) -> Option<Rc<RefCell<ConcreteVariable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_write_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }

    pub fn get_read(&self, name: &str) -> Option<Rc<RefCell<ConcreteVariable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if let Some(value) = scope.borrow().parent_references.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_read_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }
    pub fn get_write(&self, name: &str) -> Option<Rc<RefCell<ConcreteVariable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if let Some(value) = scope.borrow().parent_references.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_write_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }
}

impl VariableScopeStack {
    pub fn new(id_generator: &mut IdGenerator) -> Self {
        Self {
            current: Rc::new(RefCell::new(VariableScope::new(
                id_generator.next_scope_id(),
            ))),
        }
    }

    pub fn from_scope(current: Rc<RefCell<VariableScope>>) -> Self {
        Self { current }
    }

    /// access a variable for reading without checking if a parent reference for it exists
    pub fn get_read_noref(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_read_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }
    /// access a variable for writing without checking if a parent reference for it exists
    pub fn get_write_noref(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_write_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }

    pub fn get_read(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if let Some(value) = scope.borrow().parent_references.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_read_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }
    pub fn get_write(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
        let mut copy_from_parent_scopes = vec![];

        let mut current = Some(self.current.clone());

        let mut res = None;
        while let Some(scope) = current {
            if let Some(value) = scope.borrow().variable_map.get(name) {
                res = Some(value.clone());
                break;
            }
            if let Some(value) = scope.borrow().parent_references.get(name) {
                res = Some(value.clone());
                break;
            }
            if scope.borrow().copy_from_parent {
                copy_from_parent_scopes.push(scope.clone());
            }
            if scope.borrow().is_write_guard {
                break;
            }

            current = scope.borrow().parent.clone();
        }

        if let Some(res) = res.clone() {
            for scope in copy_from_parent_scopes {
                scope
                    .borrow_mut()
                    .parent_references
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }

    pub fn try_insert(&mut self, var: Variable) -> Result<(), ScopeInsertAlreadyPresentError> {
        if let Some(present_var) = self.current.borrow().variable_map.get(&var.symbol.name) {
            Err(ScopeInsertAlreadyPresentError {
                present_var: present_var.clone(),
            })
        } else {
            self.current
                .borrow_mut()
                .variable_map
                .insert(var.symbol.name.clone(), Rc::new(RefCell::new(var)));
            Ok(())
        }
    }
    pub fn push_child(&mut self, id_generator: &mut IdGenerator) {
        self.current = Rc::new(RefCell::new(VariableScope {
            parent: Some(self.current.clone()),
            ..VariableScope::new(id_generator.next_scope_id())
        }));
    }
    pub fn pop(&mut self) {
        let tmp = self
            .current
            .borrow()
            .parent
            .as_ref()
            .expect("Tried popping the global scope")
            .clone();

        self.current = tmp;
    }

    pub fn top_mut(&self) -> RefMut<'_, VariableScope> {
        self.current.borrow_mut()
    }
}

#[derive(Default, Debug, PartialEq, Eq)]
pub enum EvaluationTime {
    /// must be evaluated at compile time
    Comptime,
    /// must be evaluated at runtime
    Runtime,
    /// can be evaluated whenever
    #[default]
    Any,
}

NewtypeDeref!(pub ComptimeDeps, PerNodeData<(EvaluationTime, Vec<NodeID>)>);

impl ComptimeDeps {
    pub fn get_or_init(&mut self, key: NodeID) -> &mut (EvaluationTime, Vec<NodeID>) {
        self.entry(key).or_default()
    }
    pub fn add_dependency(&mut self, node: NodeID, new_dep: NodeID) {
        let (_eval_time, deps) = self.get_or_init(node);
        deps.push(new_dep);
        self.update_dependency_states(node);
    }
    pub fn add_dependencies(&mut self, node: NodeID, new_deps: impl IntoIterator<Item = NodeID>) {
        let (_eval_time, deps) = self.get_or_init(node);
        deps.extend(new_deps);
        self.update_dependency_states(node);
    }

    fn update_dependency_states(&mut self, node: NodeID) {
        let (eval_time, _deps) = self.get_or_init(node);
        match eval_time {
            EvaluationTime::Comptime => self.mark_comptime_required(node),
            EvaluationTime::Runtime => todo!(),
            EvaluationTime::Any => {}
        }
    }

    pub fn mark_comptime_required(&mut self, node: NodeID) {
        let mut queue = VecDeque::new();
        queue.push_back(node);

        while let Some(node) = queue.pop_front() {
            let (eval_time, deps) = self.get_or_init(node);
            match eval_time {
                EvaluationTime::Comptime => {}
                EvaluationTime::Runtime => {
                    // TODO: add proper error
                    panic!(
                        "cannot mark node as compile time constant because it depennds on runtime values"
                    )
                }
                EvaluationTime::Any => {
                    *eval_time = EvaluationTime::Comptime;
                    queue.extend(deps.iter());
                }
            }
        }
    }
}

pub struct ScopeAnalyzer<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,
    id_generator: RefMut<'state, IdGenerator>,

    comptime_deps: RefMut<'state, ComptimeDeps>,
    referenced_variable: RefMut<'state, VariableData>,
    referenced_function: RefMut<'state, FunctionData>,
    contained_scope: RefMut<'state, ScopeData>,

    variable_scopes: VariableScopeStack,
    function_list: HashMap<String, Rc<RefCell<Function>>>,
    current_function: Option<Rc<RefCell<Function>>>,
}

impl PassFactory for ScopeAnalyzer<'_> {
    type Output<'state> = ScopeAnalyzer<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        let errors = state.check_named()?;
        let program = state.check_named()?;
        let id_generator = state.check_named()?;

        let comptime_deps = state.insert_default();
        let referenced_variable = state.insert_default();
        let referenced_function = state.insert_default();
        let contained_scope = state.insert_default();

        let mut id_generator = id_generator.get_mut(state);
        let variable_scopes = VariableScopeStack::new(&mut id_generator);

        Ok(Self::Output {
            errors: errors.get_mut(state),
            program: Some(program.get_mut(state)),
            id_generator,

            comptime_deps: comptime_deps.get_mut(state),
            referenced_variable: referenced_variable.get_mut(state),
            referenced_function: referenced_function.get_mut(state),
            contained_scope: contained_scope.get_mut(state),

            variable_scopes,
            function_list: HashMap::new(),
            current_function: None,
        })
    }
}
impl Pass for ScopeAnalyzer<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        self.visit_program(&mut program);

        Ok(())
    }
}

impl<'a> ScopeAnalyzer<'a> {
    fn register_top_level_statement(&mut self, tls: &mut TopLevelStatement) {
        match tls {
            TopLevelStatement::Function(function_definition) => {
                self.register_function(function_definition)
            }
            TopLevelStatement::TypeAlias(_type_alias) => {
                // Type aliases are handled by type propagation.
            }
        }
    }
    fn register_function(&mut self, function: &mut FunctionDefinition) {
        let FunctionDefinition {
            let_token: _,
            name,
            name_token,
            equal_token: _,
            open_paren_token: _,
            parameters: _,
            close_paren_token: _,
            right_arrow_token: _,
            return_type: _,
            body: _,
            id,
        } = function;

        self.variable_scopes.push_child(&mut self.id_generator); // temporary parameter scope

        if let Some(original) = self.function_list.insert(
            name.clone(),
            Rc::new(RefCell::new(Function {
                symbol: SymbolDefinition::from_token(name.clone(), name_token),
                return_type: None,
                parameter_types: None,
                id: self.id_generator.next_function_id(),
                definition_node_id: *id,
            })),
        ) {
            self.errors.push(ErrorKind::Duplicate {
                kind: DuplicateKind::Function,
                original: original.borrow().symbol.clone(),
                new_range: name_token.range.clone(),
            });
        }

        // this is just the registration, so nothing has to be specialized
        // self.require_fully_specialized_scope(stmt_clone);
        self.variable_scopes.pop();
    }
}

impl AstVisitor for ScopeAnalyzer<'_> {
    type ProgramOutput = ();
    type TopLevelOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = NodeID;
    type StatementOutput = NodeID;
    type ExecutorHostOutput = ();
    type ExecutorOutput = NodeID;
    type ExpressionOutput = NodeID;
    type LValueOutput = NodeID;
    type TypeOutput = NodeID;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        for tls in &mut program.top_level_statements {
            self.register_top_level_statement(tls);
        }
        for tls in &mut program.top_level_statements {
            self.visit_top_level_statement(tls);
        }
    }

    fn visit_function_definition(&mut self, fdef: &mut FunctionDefinition) -> Self::TopLevelOutput {
        let FunctionDefinition {
            let_token: _,
            name,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body,
            id,
        } = fdef;

        let this_function = self
            .function_list
            .get(name)
            .expect("All functions should have been registered before traversing the tree")
            .clone();

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }

        self.current_function = Some(this_function.clone());

        self.variable_scopes.push_child(&mut self.id_generator); // the parameter scope
        for (param, _comma) in parameters {
            self.visit_simple_binding(param);
        }

        self.visit_function_body(body);

        self.variable_scopes.pop();

        self.referenced_function
            .insert(body.get_id(), this_function.clone());

        self.referenced_function.insert(*id, this_function);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_type_alias(&mut self, type_alias: &mut TypeAlias) -> Self::TopLevelOutput {
        let TypeAlias {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            type_,
            semicolon_token: _,
            id,
        } = type_alias;

        self.visit_type(type_);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_statement(statement);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token: _,
            id,
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let simple_binding_clone = simple_binding.clone();
        let SimpleBinding {
            name_token,
            name,
            type_,
            id,
        } = simple_binding;

        if let Some((_colon, type_)) = type_ {
            let dep = self.visit_type(type_);
            self.comptime_deps.add_dependency(*id, dep);
            self.comptime_deps.mark_comptime_required(dep);
        }
        if let Err(ScopeInsertAlreadyPresentError { present_var }) =
            self.variable_scopes.try_insert(Variable {
                symbol: SymbolDefinition::from_token(name.clone(), name_token),
                type_: None,
                is_constant: false,
                id: self.id_generator.next_variable_id(),
                definition_node_id: *id,
            })
        {
            self.errors.push(ErrorKind::Duplicate {
                kind: DuplicateKind::Variable,
                original: present_var.borrow().symbol.clone(),
                new_range: find_node_bounds(&simple_binding_clone),
            });
        }

        self.referenced_variable.insert(
            *id,
            self.variable_scopes
                .get_write(name)
                .expect("This variable should have just been added")
                .clone(),
        );

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let dep = self.visit_expression(expression);

        self.comptime_deps.add_dependency(*id, dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_on_statement(&mut self, stmt: &mut OnStatement) -> Self::StatementOutput {
        let OnStatement {
            on_token: _,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id,
        } = stmt;

        self.variable_scopes.push_child(&mut self.id_generator);
        self.variable_scopes.top_mut().copy_from_parent = true;

        self.visit_executor(executor);

        // TODO: remove/inline into loop after we allow triangular iterators
        let iterators = iterators.iter_mut().map(|osi| {
            let max_value_id = self.visit_expression(&mut osi.max_value);
            (osi, max_value_id)
        });

        for (
            OnStatementIterator {
                open_bracket_token: _,
                binding,
                equal_token: _,
                max_value: _,
                close_bracket_token: _,
            },
            max_value_id,
        ) in iterators.collect_vec()
        {
            let iterator_id = self.visit_simple_binding(binding);
            self.comptime_deps
                .add_dependencies(*id, [max_value_id, iterator_id]);
        }

        for (binding, _comma) in bindings {
            let dep = self.visit_lvalue(binding);
            self.comptime_deps.add_dependency(*id, dep);
        }

        // cannot write to the outside anymore
        self.variable_scopes.top_mut().is_write_guard = true;

        self.visit_statement(body);

        self.variable_scopes.pop();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_block_statement(&mut self, stmt: &mut BlockStatement) -> Self::StatementOutput {
        let BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id,
        } = stmt;

        self.variable_scopes.push_child(&mut self.id_generator);

        for stmt in body {
            let dep = self.visit_statement(stmt);
            self.comptime_deps.add_dependency(*id, dep);
        }

        self.variable_scopes.pop();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement) -> Self::StatementOutput {
        let ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id,
        } = stmt;

        if let Some(value) = value {
            let dep = self.visit_expression(value);
            self.comptime_deps.add_dependency(*id, dep);
        }

        self.current_function
            .as_ref()
            .expect("Return statements should only appear inside functions");

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        let VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id,
        } = vardef_stmt;

        // evaluated before the binding so it can potentially access variables that get shadowed
        // by this binding
        let value_dep = self.visit_expression(value);
        let binding_dep = self.visit_simple_binding(binding);

        self.comptime_deps
            .add_dependencies(*id, [value_dep, binding_dep]);
        self.comptime_deps.add_dependency(binding_dep, value_dep);
        self.comptime_deps.add_dependency(binding_dep, *id);

        let binding_ref_var = self.referenced_variable.get(&binding.id).unwrap().clone();
        self.referenced_variable.insert(*id, binding_ref_var);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
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
        let cond_dep = self.visit_expression(condition);
        let body_dep = self.visit_statement(if_body);

        self.comptime_deps
            .add_dependencies(*id, [cond_dep, body_dep]);

        for (_elif_token, elif_condition, elif_body) in elifs {
            let elif_dep = self.visit_expression(elif_condition);
            let body_dep = self.visit_statement(elif_body);
            self.comptime_deps
                .add_dependencies(*id, [elif_dep, body_dep]);
        }
        if let Some((_else_token, else_body)) = else_ {
            let dep = self.visit_statement(else_body);
            self.comptime_deps.add_dependency(*id, dep);
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
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
        let cond_dep = self.visit_expression(condition);
        let body_dep = self.visit_statement(body);

        self.comptime_deps
            .add_dependencies(*id, [cond_dep, body_dep]);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
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
        self.variable_scopes.push_child(&mut self.id_generator);
        let init_dep = self.visit_statement(initializer);

        if let Some(con) = condition {
            let cond_dep = self.visit_expression(con);
            self.comptime_deps.add_dependency(*id, cond_dep);
        }
        if let Some(inc) = incrementer {
            let inc_dep = self.visit_expression(inc);
            self.comptime_deps.add_dependency(*id, inc_dep);
        }
        let body_dep = self.visit_statement(body);

        self.comptime_deps
            .add_dependencies(*id, [init_dep, body_dep]);

        self.variable_scopes.pop();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_break_statement(
        &mut self,
        BreakStatement {
            break_token: _,
            semicolon_token: _,
            id,
        }: &mut BreakStatement,
    ) -> Self::StatementOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id,
        }: &mut SkipStatement,
    ) -> Self::StatementOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_self_executor_host(
        &mut self,
        SelfExecutorHost { token: _, id }: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
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
        self.visit_executor_host(host);
        let index_dep = self.visit_expression(index);
        self.comptime_deps.add_dependency(*id, index_dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
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
        self.visit_executor_host(host);
        let index_dep = self.visit_expression(gpu_index);
        self.comptime_deps.add_dependency(*id, index_dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_literal_expression(
        &mut self,
        expression: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        let LiteralExpression {
            value: _,
            token: _,
            id,
        } = expression;

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        let ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id,
        } = expression;

        for (item, _comma) in &mut *elements {
            let this_item_dep = self.visit_expression(item);
            self.comptime_deps.add_dependency(*id, this_item_dep);
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_struct_expression(
        &mut self,
        expression: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        let StructExpression {
            type_,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id,
        } = expression;

        let type_dep = self.visit_type(type_);
        self.comptime_deps.add_dependency(*id, type_dep);

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
            let value_dep = self.visit_expression(value);
            self.comptime_deps.add_dependency(*id, value_dep);
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let FunctionCallExpression {
            name,
            name_token: _,
            open_paren_token: _,
            arguments: _,
            close_paren_token: _,
            id,
        } = expression;
        let Some(ref_function) = self.function_list.get(name).cloned() else {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::Function,
                item: UnresolvedSymbol::from_token(name.clone(), &expression.name_token),
            });

            // still visit args, even though the function doesn't exist
            for (arg, _comma) in &mut expression.arguments {
                self.visit_expression(arg);
            }
            return *id;
        };
        self.referenced_function.insert(*id, ref_function.clone());

        self.comptime_deps
            .add_dependency(*id, ref_function.borrow().definition_node_id);

        let arg_deps = expression
            .arguments
            .iter_mut()
            .map(|(arg, _comma)| self.visit_expression(arg))
            .collect_vec();

        self.comptime_deps.add_dependencies(*id, arg_deps);

        // we don't handle mismatched argument/parameter count here because type checking has more
        // information to report a better error

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_compiler_expression(
        &mut self,
        expression: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        let CompilerExpression {
            at_token: _,
            name,
            name_token: _,
            open_paren_token: _,
            arguments: _,
            close_paren_token: _,
            id,
        } = expression;

        let arg_deps = expression
            .arguments
            .iter_mut()
            .map(|(arg, _comma)| self.visit_expression(arg))
            .collect_vec();

        self.comptime_deps.add_dependencies(*id, arg_deps);

        match name.as_str() {
            "zero" => {}
            "sqrt" => {}
            "sin" => {}
            "cos" => {}
            "length" => {}
            "comptime" => {
                self.comptime_deps.mark_comptime_required(*id);
            }
            _ => {
                self.errors.push(ErrorKind::IntrinsicUnknown {
                    intrinsic: UnresolvedSymbol::from_token(name.clone(), &expression.name_token),
                });
            }
        }

        // as with functions, parameter count mismatch is handled while typechecking

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
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
        let array_dep = self.visit_expression(array);
        let index_dep = self.visit_expression(index);

        self.comptime_deps
            .add_dependencies(*id, [array_dep, index_dep]);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
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
        let value_dep = self.visit_expression(value);

        self.comptime_deps.add_dependency(*id, value_dep);
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
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
        let sub_dep = self.visit_expression(subexpression);
        self.comptime_deps.add_dependency(*id, sub_dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let VariableAccessExpression {
            name,
            name_token: _,
            id,
        } = expression;
        let Some(ref_variable) = self.variable_scopes.get_read(name) else {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::Variable,
                item: UnresolvedSymbol::from_token(name.clone(), &expression.name_token),
            });
            return *id;
        };

        self.referenced_variable.insert(*id, ref_variable.clone());

        // MARKER: PHI_NODE_LIMITATION
        // TODO: NOTE: This doesn't work for variables that would need a phi node in ssa form.
        // For example:
        //
        // ```fleet
        // let a = 2;
        //
        // @RequireConsteval(a);
        //
        // if x {
        //     a = @RequireRuntimeEval(4);
        // }
        // ```
        //
        // This wouldn't work because "a = ..." adds a dependency of ... to all of a, even the a
        // that is required as consteval before this statement.

        self.comptime_deps
            .add_dependency(*id, ref_variable.borrow().definition_node_id);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }
    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let UnaryExpression {
            operator_token: _,
            operation: _,
            operand,
            id,
        } = expression;

        let dep = self.visit_expression(operand);

        self.comptime_deps.add_dependency(*id, dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        let CastExpression {
            operand,
            as_token: _,
            type_,
            id,
        } = expression;

        let from_dep = self.visit_expression(operand);
        let to_dep = self.visit_type(type_);

        self.comptime_deps.add_dependencies(*id, [from_dep, to_dep]);
        self.comptime_deps.mark_comptime_required(to_dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        let BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id,
        } = expression;
        let left_dep = self.visit_expression(left);
        let right_dep = self.visit_expression(right);

        self.comptime_deps
            .add_dependencies(*id, [left_dep, right_dep]);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id,
        } = expression;
        let left_dep = self.visit_lvalue(lvalue);
        let right_dep = self.visit_expression(right);

        // TODO: NOTE: see note above at PHI_NODE_LIMITATION
        self.comptime_deps.add_dependency(left_dep, right_dep);
        self.comptime_deps.add_dependency(*id, right_dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        let VariableLValue {
            name,
            name_token,
            id,
        } = lvalue;
        let Some(ref_variable) = self.variable_scopes.get_write(name) else {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::Variable,
                item: UnresolvedSymbol::from_token(name.clone(), name_token),
            });
            return *id;
        };
        if ref_variable.borrow().is_constant {
            self.errors.push(ErrorKind::ConstantVariableAsLValue {
                variable: ref_variable
                    .borrow()
                    .symbol
                    .clone()
                    .with_use(name_token.range.clone()),
            });
        }
        self.referenced_variable.insert(*id, ref_variable.clone());

        self.comptime_deps
            .add_dependency(*id, ref_variable.borrow().definition_node_id);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput {
        let _lvalue_clone = lvalue.clone();
        let ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        } = lvalue;

        let array_dep = self.visit_lvalue(array);
        let index_dep = self.visit_expression(index);

        self.comptime_deps
            .add_dependencies(*id, [array_dep, index_dep]);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_struct_access_lvalue(
        &mut self,
        lvalue: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        let _lvalue_clone = lvalue.clone();
        let StructAccessLValue {
            value,
            dot_token: _,
            member_name: _,
            member_name_token: _,
            id,
        } = lvalue;

        let value_dep = self.visit_lvalue(value);

        self.comptime_deps.add_dependency(*id, value_dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput {
        let _lvalue_clone = lvalue.clone();
        let GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id,
        } = lvalue;

        let dep = self.visit_lvalue(sublvalue);

        self.comptime_deps.add_dependency(*id, dep);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_simple_type(
        &mut self,
        SimpleType {
            token: _,
            type_: _,
            id,
        }: &mut SimpleType,
    ) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id,
        }: &mut UnitType,
    ) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_idk_type(&mut self, IdkType { token: _, id }: &mut IdkType) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        *id
    }

    fn visit_array_type(
        &mut self,
        ArrayType {
            subtype: _,
            open_bracket_token: _,
            size,
            close_bracket_token: _,
            id,
        }: &mut ArrayType,
    ) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        if let Some(size) = size {
            let size_dep = self.visit_expression(size);
            self.comptime_deps.add_dependency(*id, size_dep);
            self.comptime_deps.mark_comptime_required(*id);
        }

        *id
    }

    fn visit_struct_type(
        &mut self,
        StructType {
            struct_token: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id,
        }: &mut StructType,
    ) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        let mut member_unique_set = HashMap::new();

        for (
            StructMemberDefinition {
                name,
                name_token,
                colon_token: _,
                type_,
            },
            _comma,
        ) in members
        {
            let member_dep = self.visit_type(type_);
            self.comptime_deps.add_dependency(*id, member_dep);

            if let Some(other_range) =
                member_unique_set.insert(name.clone(), name_token.range.clone())
            {
                self.errors.push(ErrorKind::Duplicate {
                    kind: DuplicateKind::StructMember,
                    original: SymbolDefinition::new(name.clone(), other_range.clone()),
                    new_range: name_token.range.clone(),
                });
            }
        }

        *id
    }

    fn visit_alias_type(
        &mut self,
        AliasType {
            name: _,
            name_token: _,
            id,
        }: &mut AliasType,
    ) -> Self::TypeOutput {
        // Type aliases are handled by type propagation.

        *id
    }
}
