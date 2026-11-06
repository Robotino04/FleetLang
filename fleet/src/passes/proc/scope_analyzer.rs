use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    hash::Hash,
    rc::Rc,
};

use itertools::Itertools;
use thiserror::Error;

use crate::{
    NewtypeDerefNoDefault,
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType,
        BinaryExpression, BlockStatement, BreakStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, HasSourceRange,
        IdkType, IfStatement, LiteralExpression, NodeID, OnStatement, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement, StatementFunctionBody,
        StructAccessExpression, StructAccessLValue, StructExpression, StructMemberDefinition,
        StructMemberValue, StructType, ThreadExecutor, TopLevelStatement, TypeAlias,
        UnaryExpression, UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
        node_data::{FinalPerNodeData, NodeDataSpec, NodeDataSpecGroups, PerNodeData},
    },
    define_dispatch_methods,
    error_reporting::{
        DuplicateKind, ErrorKind, Errors, NotDefinedKind, SymbolDefinition, UnresolvedSymbol,
    },
    passes::{prelude::*, proc::parser::IdGenerator},
    runtime_type::RuntimeType,
    union_find_set::UnionFindSetPtr,
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

impl VariableScope {
    fn new(id: ScopeID) -> Self {
        Self {
            id,
            parent_references: HashMap::default(),
            variable_map: HashMap::default(),
            is_write_guard: false,
            parent: None,
            is_read_guard: false,
            copy_from_parent: false,
        }
    }

    pub fn vars_and_refs(&self) -> HashMap<String, Rc<RefCell<Variable>>> {
        let mut map = self.variable_map.clone();
        map.extend(self.parent_references.clone().drain());
        map
    }
}

pub struct VariableScopeStack {
    pub current: Rc<RefCell<VariableScope>>,
}

#[derive(Debug, Error)]
#[error(
    "Variable named {:?} was already present.", present_var.borrow().symbol
)]
pub struct ScopeInsertAlreadyPresentError {
    present_var: Rc<RefCell<Variable>>,
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

impl NodeDataSpec for VariableData {
    // TODO: will become incorrect once we have more complex patterns
    type VariableDefinitionStatement = Rc<RefCell<Variable>>;

    type SimpleBinding = Rc<RefCell<Variable>>;
    type VariableAccessExpression = Rc<RefCell<Variable>>;
    type VariableLValue = Rc<RefCell<Variable>>;
}
NewtypeDerefNoDefault!(pub VariableData, FinalPerNodeData<VariableData>);

impl NodeDataSpecGroups for ScopeData {
    type Program = Rc<RefCell<VariableScope>>;
    type TopLevelStatement = Rc<RefCell<VariableScope>>;
    type FunctionBody = Rc<RefCell<VariableScope>>;
    type SimpleBinding = Rc<RefCell<VariableScope>>;
    type Statement = Rc<RefCell<VariableScope>>;
    type ExecutorHost = Rc<RefCell<VariableScope>>;
    type Executor = Rc<RefCell<VariableScope>>;
    type Expression = Rc<RefCell<VariableScope>>;
    type LValue = Rc<RefCell<VariableScope>>;
    type Type = Rc<RefCell<VariableScope>>;
}
NewtypeDerefNoDefault!(pub ScopeData, FinalPerNodeData<ScopeData>);

impl NodeDataSpec for FunctionData {
    type FunctionDefinition = Rc<RefCell<Function>>;
    type FunctionCallExpression = Rc<RefCell<Function>>;
    type ExternFunctionBody = Rc<RefCell<Function>>;
}
NewtypeDerefNoDefault!(pub FunctionData, FinalPerNodeData<FunctionData>);

pub struct ScopeAnalyzer<'state> {
    errors: &'state mut Errors,
    program: Option<&'state mut Program>,
    id_generator: &'state mut IdGenerator,

    referenced_variable: PerNodeData<VariableData>,
    referenced_variable_entry: Empty<'state, VariableData>,
    referenced_function: PerNodeData<FunctionData>,
    referenced_function_entry: Empty<'state, FunctionData>,
    contained_scope: PerNodeData<ScopeData>,
    contained_scope_entry: Empty<'state, ScopeData>,

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
    ) -> Result<Self::Output<'state>, FetchError> {
        let Fetched {
            read: (),
            write: (errors, program, id_generator),
            empty: (referenced_variable, referenced_function, contained_scope),
        } = state.fetch()?;

        let variable_scopes = VariableScopeStack::new(id_generator);

        Ok(Self::Output {
            errors,
            program: Some(program),
            id_generator,

            referenced_variable: Default::default(),
            referenced_variable_entry: referenced_variable,
            referenced_function: Default::default(),
            referenced_function_entry: referenced_function,
            contained_scope: Default::default(),
            contained_scope_entry: contained_scope,

            variable_scopes,
            function_list: HashMap::new(),
            current_function: None,
        })
    }
}
impl Pass for ScopeAnalyzer<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = self.program.take().unwrap();
        self.analyze_program(program);
        self.referenced_variable_entry.insert(
            self.referenced_variable
                .finish(program)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?
                .into(),
        );
        self.contained_scope_entry.insert(
            self.contained_scope
                .finish(program)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?
                .into(),
        );

        self.referenced_function_entry.insert(
            self.referenced_function
                .finish(program)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?
                .into(),
        );

        Ok(())
    }
}

impl<'a> ScopeAnalyzer<'a> {
    fn register_top_level_statement(&mut self, tls: &mut TopLevelStatement) {
        match tls {
            TopLevelStatement::FunctionDefinition(function_definition) => {
                self.register_function(function_definition);
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

        self.variable_scopes.push_child(self.id_generator); // temporary parameter scope

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

impl ScopeAnalyzer<'_> {
    define_dispatch_methods! {
        analyze {
            TopLevelStatement => (),
            FunctionBody => (),
            Statement => (),
            ExecutorHost => (),
            Executor => (),
            Expression => (),
            LValue => (),
            Type => (),
        }
    }

    fn analyze_program(&mut self, program: &mut Program) {
        for tls in &mut program.top_level_statements {
            self.register_top_level_statement(tls);
        }
        for tls in &mut program.top_level_statements {
            self.analyze_top_level_statement(tls);
        }

        self.contained_scope
            .insert(program, self.variable_scopes.current.clone());
    }

    fn analyze_function_definition(&mut self, fdef: &mut FunctionDefinition) {
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
            id: _,
        } = fdef;

        let this_function = self
            .function_list
            .get(name)
            .expect("All functions should have been registered before traversing the tree")
            .clone();

        if let Some(return_type) = return_type {
            self.analyze_type(return_type);
        }

        self.current_function = Some(this_function.clone());

        self.variable_scopes.push_child(self.id_generator); // the parameter scope
        for (param, _comma) in parameters {
            self.analyze_simple_binding(param);
        }

        self.analyze_function_body(body);

        self.variable_scopes.pop();

        self.referenced_function.insert(fdef, this_function);

        self.contained_scope
            .insert(fdef, self.variable_scopes.current.clone());
    }

    fn analyze_type_alias(&mut self, type_alias: &mut TypeAlias) {
        let TypeAlias {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            type_,
            semicolon_token: _,
            id: _,
        } = type_alias;

        self.analyze_type(type_);

        self.contained_scope
            .insert(type_alias, self.variable_scopes.current.clone());
    }

    fn analyze_statement_function_body(&mut self, fbody: &mut StatementFunctionBody) {
        let StatementFunctionBody { statement, id: _ } = fbody;
        self.analyze_statement(statement);

        self.contained_scope
            .insert(fbody, self.variable_scopes.current.clone());
    }

    fn analyze_extern_function_body(&mut self, fbody: &mut ExternFunctionBody) {
        let ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token: _,
            id: _,
        } = fbody;

        self.referenced_function
            .insert(fbody, self.current_function.clone().unwrap());

        self.contained_scope
            .insert(fbody, self.variable_scopes.current.clone());
    }

    fn analyze_simple_binding(&mut self, simple_binding: &mut SimpleBinding) {
        let simple_binding_clone = simple_binding.clone();
        let SimpleBinding {
            name_token,
            name,
            type_,
            id,
        } = simple_binding;

        if let Some((_colon, type_)) = type_ {
            self.analyze_type(type_);
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
                new_range: simple_binding_clone.get_source_range(),
            });
        }

        let var = self
            .variable_scopes
            .get_write(name)
            .expect("This variable should have just been added")
            .clone();

        self.referenced_variable.insert(simple_binding, var);

        self.contained_scope
            .insert(simple_binding, self.variable_scopes.current.clone());
    }

    fn analyze_expression_statement(&mut self, stmt: &mut ExpressionStatement) {
        let ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        } = stmt;

        self.analyze_expression(expression);

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_on_statement(&mut self, stmt: &mut OnStatement) {
        let OnStatement {
            on_token: _,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id: _,
        } = stmt;

        self.variable_scopes.push_child(self.id_generator);
        self.variable_scopes.top_mut().copy_from_parent = true;

        self.analyze_executor(executor);

        // TODO: merge these loops after we allow triangular iterators
        for iter in &mut *iterators {
            self.analyze_expression(&mut iter.max_value);
        }
        for iter in &mut *iterators {
            self.analyze_simple_binding(&mut iter.binding);
        }

        for (binding, _comma) in bindings {
            self.analyze_lvalue(binding);
        }

        // cannot write to the outside anymore
        self.variable_scopes.top_mut().is_write_guard = true;

        self.analyze_statement(body);

        self.variable_scopes.pop();

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_block_statement(&mut self, stmt: &mut BlockStatement) {
        let BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        } = stmt;

        self.variable_scopes.push_child(self.id_generator);

        for stmt in body {
            self.analyze_statement(stmt);
        }

        self.variable_scopes.pop();

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_return_statement(&mut self, stmt: &mut ReturnStatement) {
        let ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id: _,
        } = stmt;

        if let Some(value) = value {
            self.analyze_expression(value);
        }

        self.current_function
            .as_ref()
            .expect("Return statements should only appear inside functions");

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) {
        let VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        } = vardef_stmt;

        // evaluated before the binding so it can potentially access variables that get shadowed
        // by this binding
        self.analyze_expression(value);
        self.analyze_simple_binding(binding);

        let binding_ref_var = self.referenced_variable.get(&**binding).unwrap().clone();
        self.referenced_variable
            .insert(vardef_stmt, binding_ref_var);

        self.contained_scope
            .insert(vardef_stmt, self.variable_scopes.current.clone());
    }

    fn analyze_if_statement(&mut self, stmt: &mut IfStatement) {
        let IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        } = stmt;
        self.analyze_expression(condition);
        self.analyze_statement(if_body);

        for (_elif_token, elif_condition, elif_body) in elifs {
            self.analyze_expression(elif_condition);
            self.analyze_statement(elif_body);
        }
        if let Some((_else_token, else_body)) = else_ {
            self.analyze_statement(else_body);
        }

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_while_loop_statement(&mut self, stmt: &mut WhileLoopStatement) {
        let WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        } = stmt;

        self.analyze_expression(condition);
        self.analyze_statement(body);

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_for_loop_statement(&mut self, stmt: &mut ForLoopStatement) {
        let ForLoopStatement {
            for_token: _,
            open_paren_token: _,
            initializer,
            condition,
            second_semicolon_token: _,
            incrementer,
            close_paren_token: _,
            body,
            id: _,
        } = stmt;

        self.variable_scopes.push_child(self.id_generator);
        self.analyze_statement(initializer);

        if let Some(con) = condition {
            self.analyze_expression(con);
        }
        if let Some(inc) = incrementer {
            self.analyze_expression(inc);
        }
        self.analyze_statement(body);

        self.variable_scopes.pop();

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_break_statement(&mut self, stmt: &mut BreakStatement) {
        let BreakStatement {
            break_token: _,
            semicolon_token: _,
            id: _,
        } = stmt;

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_skip_statement(&mut self, stmt: &mut SkipStatement) {
        let SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id: _,
        } = stmt;

        self.contained_scope
            .insert(stmt, self.variable_scopes.current.clone());
    }

    fn analyze_self_executor_host(&mut self, host: &mut SelfExecutorHost) {
        let SelfExecutorHost { token: _, id: _ } = host;

        self.contained_scope
            .insert(host, self.variable_scopes.current.clone());
    }

    fn analyze_thread_executor(&mut self, executor: &mut ThreadExecutor) {
        let ThreadExecutor {
            host,
            dot_token: _,
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        } = executor;

        self.analyze_executor_host(host);
        self.analyze_expression(index);

        self.contained_scope
            .insert(executor, self.variable_scopes.current.clone());
    }

    fn analyze_gpu_executor(&mut self, executor: &mut GPUExecutor) {
        let GPUExecutor {
            host,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index,
            close_bracket_token: _,
            id: _,
        } = executor;

        self.analyze_executor_host(host);
        self.analyze_expression(gpu_index);

        self.contained_scope
            .insert(executor, self.variable_scopes.current.clone());
    }

    fn analyze_literal_expression(&mut self, expression: &mut LiteralExpression) {
        let LiteralExpression {
            value: _,
            token: _,
            id: _,
        } = expression;

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_array_expression(&mut self, expression: &mut ArrayExpression) {
        let ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id: _,
        } = expression;

        for (item, _comma) in &mut *elements {
            self.analyze_expression(item);
        }

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_struct_expression(&mut self, expression: &mut StructExpression) {
        let StructExpression {
            type_,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        } = expression;

        self.analyze_type(type_);

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
            self.analyze_expression(value);
        }

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_function_call_expression(&mut self, expression: &mut FunctionCallExpression) {
        let FunctionCallExpression {
            name,
            name_token,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        } = expression;
        let ref_function = self.function_list.get(name).cloned().unwrap_or_else(|| {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::Function,
                item: UnresolvedSymbol::from_token(name.clone(), name_token),
            });

            // create a fake function
            Rc::new(RefCell::new(Function {
                symbol: SymbolDefinition::from_token(name.clone(), name_token),
                return_type: None,
                parameter_types: Some(
                    (0..arguments.len())
                        .map(|i| {
                            Rc::new(RefCell::new(Variable {
                                symbol: SymbolDefinition::from_token(
                                    format!("{name}_{i}"),
                                    name_token,
                                ),
                                type_: None,
                                is_constant: false,
                                id: self.id_generator.next_variable_id(),
                                definition_node_id: *id,
                            }))
                        })
                        .collect_vec(),
                ),
                id: self.id_generator.next_function_id(),
                definition_node_id: *id,
            }))
        });

        for (arg, _comma) in arguments {
            self.analyze_expression(arg);
        }

        // we don't handle mismatched argument/parameter count here because type checking has more
        // information to report a better error

        self.referenced_function
            .insert(expression, ref_function.clone());

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_compiler_expression(&mut self, expression: &mut CompilerExpression) {
        let CompilerExpression {
            at_token: _,
            name,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        } = expression;

        for (arg, _comma) in arguments {
            self.analyze_expression(arg);
        }

        match name.as_str() {
            "zero" => {}
            "sqrt" => {}
            "sin" => {}
            "cos" => {}
            "length" => {}
            _ => {
                self.errors.push(ErrorKind::IntrinsicUnknown {
                    intrinsic: UnresolvedSymbol::from_token(name.clone(), &expression.name_token),
                });
            }
        }

        // as with functions, parameter count mismatch is handled while typechecking

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_array_index_expression(&mut self, expression: &mut ArrayIndexExpression) {
        let ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        } = expression;

        self.analyze_expression(array);
        self.analyze_expression(index);

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_struct_access_expression(&mut self, expression: &mut StructAccessExpression) {
        let StructAccessExpression {
            value,
            dot_token: _,
            member_name: _,
            member_name_token: _,
            id: _,
        } = expression;

        self.analyze_expression(value);

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_grouping_expression(&mut self, expression: &mut GroupingExpression) {
        let GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id: _,
        } = expression;

        self.analyze_expression(subexpression);

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_variable_access_expression(&mut self, expression: &mut VariableAccessExpression) {
        let VariableAccessExpression {
            name,
            name_token,
            id,
        } = expression;
        let ref_variable = self.variable_scopes.get_read(name).unwrap_or_else(|| {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::Variable,
                item: UnresolvedSymbol::from_token(name.clone(), name_token),
            });

            // create a fake variable
            Rc::new(RefCell::new(Variable {
                symbol: SymbolDefinition::from_token(name.clone(), name_token),
                type_: None,
                is_constant: false,
                id: self.id_generator.next_variable_id(),
                definition_node_id: *id,
            }))
        });

        self.referenced_variable
            .insert(expression, ref_variable.clone());

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }
    fn analyze_unary_expression(&mut self, expression: &mut UnaryExpression) {
        let UnaryExpression {
            operator_token: _,
            operation: _,
            operand,
            id: _,
        } = expression;

        self.analyze_expression(operand);

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_cast_expression(&mut self, expression: &mut CastExpression) {
        let CastExpression {
            operand,
            as_token: _,
            type_,
            id: _,
        } = expression;

        self.analyze_expression(operand);
        self.analyze_type(type_);

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_binary_expression(&mut self, expression: &mut BinaryExpression) {
        let BinaryExpression {
            left,
            operator_token: _,
            operation: _,
            right,
            id: _,
        } = expression;
        self.analyze_expression(left);
        self.analyze_expression(right);

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) {
        let VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id: _,
        } = expression;
        self.analyze_lvalue(lvalue);
        self.analyze_expression(right);

        self.contained_scope
            .insert(expression, self.variable_scopes.current.clone());
    }

    fn analyze_variable_lvalue(&mut self, lvalue: &mut VariableLValue) {
        let VariableLValue {
            name,
            name_token,
            id,
        } = lvalue;
        let ref_variable = self.variable_scopes.get_write(name).unwrap_or_else(|| {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::Variable,
                item: UnresolvedSymbol::from_token(name.clone(), name_token),
            });

            // create a fake variable
            Rc::new(RefCell::new(Variable {
                symbol: SymbolDefinition::from_token(name.clone(), name_token),
                type_: None,
                is_constant: false,
                id: self.id_generator.next_variable_id(),
                definition_node_id: *id,
            }))
        });

        if ref_variable.borrow().is_constant {
            self.errors.push(ErrorKind::ConstantVariableAsLValue {
                variable: ref_variable
                    .borrow()
                    .symbol
                    .clone()
                    .with_use(name_token.range.clone()),
            });
        }
        self.referenced_variable
            .insert(lvalue, ref_variable.clone());

        self.contained_scope
            .insert(lvalue, self.variable_scopes.current.clone());
    }

    fn analyze_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) {
        let _lvalue_clone = lvalue.clone();
        let ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        } = lvalue;

        self.analyze_lvalue(array);
        self.analyze_expression(index);

        self.contained_scope
            .insert(lvalue, self.variable_scopes.current.clone());
    }

    fn analyze_struct_access_lvalue(&mut self, lvalue: &mut StructAccessLValue) {
        let _lvalue_clone = lvalue.clone();
        let StructAccessLValue {
            value,
            dot_token: _,
            member_name: _,
            member_name_token: _,
            id: _,
        } = lvalue;

        self.analyze_lvalue(value);

        self.contained_scope
            .insert(lvalue, self.variable_scopes.current.clone());
    }

    fn analyze_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) {
        let _lvalue_clone = lvalue.clone();
        let GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id: _,
        } = lvalue;

        self.analyze_lvalue(sublvalue);

        self.contained_scope
            .insert(lvalue, self.variable_scopes.current.clone());
    }

    fn analyze_simple_type(&mut self, ty: &mut SimpleType) {
        let SimpleType {
            token: _,
            type_: _,
            id: _,
        } = ty;

        self.contained_scope
            .insert(ty, self.variable_scopes.current.clone());
    }

    fn analyze_unit_type(&mut self, ty: &mut UnitType) {
        let UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id: _,
        } = ty;

        self.contained_scope
            .insert(ty, self.variable_scopes.current.clone());
    }

    fn analyze_idk_type(&mut self, ty: &mut IdkType) {
        let IdkType { token: _, id: _ } = ty;

        self.contained_scope
            .insert(ty, self.variable_scopes.current.clone());
    }

    fn analyze_array_type(&mut self, ty: &mut ArrayType) {
        let ArrayType {
            subtype,
            open_bracket_token: _,
            size,
            close_bracket_token: _,
            id: _,
        } = ty;

        self.analyze_type(subtype);

        if let Some(size) = size {
            self.analyze_expression(size);
        }

        self.contained_scope
            .insert(ty, self.variable_scopes.current.clone());
    }

    fn analyze_struct_type(&mut self, ty: &mut StructType) {
        let StructType {
            struct_token: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        } = ty;

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
            self.analyze_type(type_);

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

        self.contained_scope
            .insert(ty, self.variable_scopes.current.clone());
    }

    fn analyze_alias_type(&mut self, ty: &mut AliasType) {
        let AliasType {
            name: _,
            name_token: _,
            id: _,
        } = ty;

        // Type aliases are handled by type propagation.

        self.contained_scope
            .insert(ty, self.variable_scopes.current.clone());
    }
}
