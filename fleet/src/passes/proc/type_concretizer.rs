use std::{cell::RefCell, collections::HashMap, ops::ControlFlow, rc::Rc};

use crate::{
    NewtypeDerefNoDefault,
    ast::{
        AstNodeRefMut, Expression, ExternFunctionBody, FunctionCallExpression, FunctionDefinition,
        HasSourceRange, LValue, NodeID, Program, SimpleBinding, Type, VariableAccessExpression,
        VariableDefinitionStatement, VariableLValue,
        node_data::{
            CanStoreData, FinalPerNodeData, HasData, NodeDataSpec, NodeDataSpecGroups, PerNodeData,
        },
        partial_visitor::{AstWalker, PartialAstVisitor},
    },
    error_reporting::{ErrorKind, Errors, PrefetchedType, SymbolDefinition},
    passes::{
        prelude::*,
        proc::{
            scope_analyzer::{
                Function, FunctionData, FunctionID, ScopeData, ScopeID, Variable, VariableData,
                VariableID, VariableScope,
            },
            tokenizer::NamedSourceRange,
            type_checker::{TypeData, TypeSets},
        },
    },
    runtime_type::{ConcreteRuntimeType, RuntimeType, RuntimeTypeKind},
    union_find_set::UnionFindSetPtr,
};

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

impl ConcreteVariableScope {
    pub fn vars_and_refs(&self) -> HashMap<String, Rc<RefCell<ConcreteVariable>>> {
        let mut map = self.variable_map.clone();
        map.extend(self.parent_references.clone().drain());
        map
    }
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

pub struct ConcreteVariableScopeReader {
    current: Rc<RefCell<ConcreteVariableScope>>,
}

impl NodeDataSpecGroups for ConcreteTypeData {
    type Program = ();
    type TopLevelStatement = ();
    type FunctionBody = ();
    type SimpleBinding = ConcreteRuntimeType;
    type Statement = ();
    type ExecutorHost = ();
    type Executor = ();
    type Expression = ConcreteRuntimeType;
    type LValue = ConcreteRuntimeType;
    type Type = ConcreteRuntimeType;
}
NewtypeDerefNoDefault!(pub ConcreteTypeData, FinalPerNodeData<ConcreteTypeData>);

impl NodeDataSpec for ConcreteVariableData {
    // TODO: will become incorrect once we have more complex patterns
    type VariableDefinitionStatement = Rc<RefCell<ConcreteVariable>>;

    type SimpleBinding = Rc<RefCell<ConcreteVariable>>;
    type VariableAccessExpression = Rc<RefCell<ConcreteVariable>>;
    type VariableLValue = Rc<RefCell<ConcreteVariable>>;
}
NewtypeDerefNoDefault!(pub ConcreteVariableData, FinalPerNodeData<ConcreteVariableData>);

impl NodeDataSpecGroups for ConcreteScopeData {
    type Program = Rc<RefCell<ConcreteVariableScope>>;
    type TopLevelStatement = Rc<RefCell<ConcreteVariableScope>>;
    type FunctionBody = Rc<RefCell<ConcreteVariableScope>>;
    type SimpleBinding = Rc<RefCell<ConcreteVariableScope>>;
    type Statement = Rc<RefCell<ConcreteVariableScope>>;
    type ExecutorHost = Rc<RefCell<ConcreteVariableScope>>;
    type Executor = Rc<RefCell<ConcreteVariableScope>>;
    type Expression = Rc<RefCell<ConcreteVariableScope>>;
    type LValue = Rc<RefCell<ConcreteVariableScope>>;
    type Type = Rc<RefCell<ConcreteVariableScope>>;
}
NewtypeDerefNoDefault!(pub ConcreteScopeData, FinalPerNodeData<ConcreteScopeData>);

impl NodeDataSpec for ConcreteFunctionData {
    type FunctionDefinition = Rc<RefCell<ConcreteFunction>>;
    type FunctionCallExpression = Rc<RefCell<ConcreteFunction>>;
    type ExternFunctionBody = Rc<RefCell<ConcreteFunction>>;
}
NewtypeDerefNoDefault!(pub ConcreteFunctionData, FinalPerNodeData<ConcreteFunctionData>);

pub struct TypeConcretizer<'state> {
    errors: &'state mut Errors,
    //source: &'state InputSource,
    program: Option<&'state mut Program>,

    referenced_variable: &'state VariableData,
    referenced_function: &'state FunctionData,
    contained_scope: &'state ScopeData,
    type_sets: &'state TypeSets,
    type_data: &'state TypeData,

    visited_types: HashMap<UnionFindSetPtr<RuntimeType>, Option<ConcreteRuntimeType>>,

    concrete_referenced_variable: PerNodeData<ConcreteVariableData>,
    concrete_referenced_variable_entry: Empty<'state, ConcreteVariableData>,
    concrete_referenced_function: PerNodeData<ConcreteFunctionData>,
    concrete_referenced_function_entry: Empty<'state, ConcreteFunctionData>,
    concrete_contained_scope: PerNodeData<ConcreteScopeData>,
    concrete_contained_scope_entry: Empty<'state, ConcreteScopeData>,
    concrete_type_data: PerNodeData<ConcreteTypeData>,
    concrete_type_data_entry: Empty<'state, ConcreteTypeData>,

    concrete_variable_by_id: HashMap<VariableID, Rc<RefCell<ConcreteVariable>>>,
    concrete_scope_by_id: HashMap<ScopeID, Rc<RefCell<ConcreteVariableScope>>>,

    failure: bool,
}

impl PassFactory for TypeConcretizer<'_> {
    type Output<'state> = TypeConcretizer<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, FetchError> {
        let Fetched {
            read: (referenced_variable, referenced_function, contained_scope, type_sets, type_data),
            write: (errors, program),
            empty:
                (
                    concrete_referenced_variable,
                    concrete_referenced_function,
                    concrete_contained_scope,
                    concrete_type_data_entry,
                ),
        } = state.fetch()?;

        Ok(Self::Output {
            errors,
            program: Some(program),

            referenced_variable,
            referenced_function,
            contained_scope,
            type_sets,
            type_data,

            visited_types: Default::default(),

            concrete_referenced_variable: Default::default(),
            concrete_referenced_variable_entry: concrete_referenced_variable,
            concrete_referenced_function: Default::default(),
            concrete_referenced_function_entry: concrete_referenced_function,
            concrete_contained_scope: Default::default(),
            concrete_contained_scope_entry: concrete_contained_scope,
            concrete_type_data: Default::default(),
            concrete_type_data_entry,

            concrete_variable_by_id: Default::default(),
            concrete_scope_by_id: Default::default(),

            failure: false,
        })
    }
}

impl TypeConcretizer<'_> {
    fn concretize_function(&mut self, func: &Rc<RefCell<Function>>) -> Option<ConcreteFunction> {
        let func = func.borrow();

        let Some(return_type) = func.return_type else {
            self.errors.push(ErrorKind::FunctionMissingReturnType {
                function: func.symbol.clone(),
            });
            return None;
        };
        let Some(parameters) = &func.parameter_types else {
            self.errors.push(ErrorKind::FunctionMissingParameterTypes {
                function: func.symbol.clone(),
            });
            return None;
        };

        Some(ConcreteFunction {
            symbol: func.symbol.clone(),
            return_type: self.concretize_type(return_type, func.symbol.definition.clone())?,
            parameter_types: parameters
                .iter()
                .map(|param| self.concretize_variable(param))
                .collect::<Option<Vec<_>>>()?,
            id: func.id,
            definition_node_id: func.definition_node_id,
        })
    }
    fn concretize_variable(
        &mut self,
        var: &Rc<RefCell<Variable>>,
    ) -> Option<Rc<RefCell<ConcreteVariable>>> {
        if let Some(var) = self.concrete_variable_by_id.get(&var.borrow().id) {
            return Some(var.clone());
        }

        let var = var.borrow();

        let Some(type_) = var.type_ else {
            self.errors
                .push(ErrorKind::IncompleteTypeInferenceVariable {
                    variable: var.symbol.clone(),
                    best_guess: None,
                });
            return None;
        };

        let var = Rc::new(RefCell::new(ConcreteVariable {
            symbol: var.symbol.clone(),
            type_: self.concretize_type(type_, var.symbol.definition.clone())?,
            is_constant: var.is_constant,
            id: var.id,
            definition_node_id: var.definition_node_id,
        }));

        assert!(
            self.concrete_variable_by_id
                .insert(var.borrow().id, var.clone())
                .is_none()
        );

        Some(var)
    }
    fn concretize_type(
        &mut self,
        type_ptr: UnionFindSetPtr<RuntimeType>,
        parent_range: NamedSourceRange,
    ) -> Option<ConcreteRuntimeType> {
        if let Some(res) = self.visited_types.get(&type_ptr) {
            return res.clone();
        }

        let out = || {
            let type_ = self.type_sets.get(type_ptr);
            Some(match type_.kind.clone() {
                RuntimeTypeKind::ArrayOf {
                    subtype: _,
                    size: None,
                }
                | RuntimeTypeKind::Unknown
                | RuntimeTypeKind::Number { .. } => {
                    self.errors.push(ErrorKind::IncompleteTypeInference {
                        range: type_.definition_range.clone().unwrap_or(parent_range),
                        best_guess: Some(PrefetchedType::fetch(type_ptr, self.type_sets)),
                    });
                    return None;
                }
                RuntimeTypeKind::Error => {
                    // these are always the result of an already reported error, so let's not
                    // overreport subsequent ones.
                    return None;
                }

                RuntimeTypeKind::I8 => ConcreteRuntimeType::I8,
                RuntimeTypeKind::I16 => ConcreteRuntimeType::I16,
                RuntimeTypeKind::I32 => ConcreteRuntimeType::I32,
                RuntimeTypeKind::I64 => ConcreteRuntimeType::I64,
                RuntimeTypeKind::U8 => ConcreteRuntimeType::U8,
                RuntimeTypeKind::U16 => ConcreteRuntimeType::U16,
                RuntimeTypeKind::U32 => ConcreteRuntimeType::U32,
                RuntimeTypeKind::U64 => ConcreteRuntimeType::U64,
                RuntimeTypeKind::F32 => ConcreteRuntimeType::F32,
                RuntimeTypeKind::F64 => ConcreteRuntimeType::F64,
                RuntimeTypeKind::Boolean => ConcreteRuntimeType::Boolean,
                RuntimeTypeKind::Unit => ConcreteRuntimeType::Unit,
                RuntimeTypeKind::ArrayOf {
                    subtype,
                    size: Some(size),
                } => ConcreteRuntimeType::ArrayOf {
                    size,
                    subtype: Box::new(self.concretize_type(
                        subtype,
                        type_.definition_range.clone().unwrap_or(parent_range),
                    )?),
                },
                RuntimeTypeKind::Struct {
                    members,
                    source_hash,
                } => ConcreteRuntimeType::Struct {
                    source_hash: source_hash.inner(),
                    members: members
                        .clone()
                        .into_iter()
                        .map(|(name, range, type_)| {
                            Some((name.clone(), self.concretize_type(type_, range)?))
                        })
                        .collect::<Option<_>>()?,
                },
            })
        };

        let out = out();
        self.visited_types.insert(type_ptr, out.clone());
        out
    }
    fn concretize_scope(
        &mut self,
        scope: &VariableScope,
    ) -> Option<Rc<RefCell<ConcreteVariableScope>>> {
        if let Some(scope) = self.concrete_scope_by_id.get(&scope.id) {
            return Some(scope.clone());
        }

        Some(Rc::new(RefCell::new(ConcreteVariableScope {
            id: scope.id,
            variable_map: scope
                .variable_map
                .iter()
                .map(|(name, var)| Some((name.clone(), self.concretize_variable(var)?)))
                .collect::<Option<HashMap<_, _>>>()?,
            parent_references: scope
                .parent_references
                .iter()
                .map(|(name, var)| Some((name.clone(), self.concretize_variable(var)?)))
                .collect::<Option<HashMap<_, _>>>()?,
            copy_from_parent: scope.copy_from_parent,
            is_read_guard: scope.is_read_guard,
            is_write_guard: scope.is_write_guard,
            parent: if let Some(parent) = scope.parent.clone() {
                Some(self.concretize_scope(&parent.borrow())?)
            } else {
                None
            },
        })))

        // TODO: actually cache scope
    }

    fn concretize_node<H>(&mut self, node: &mut H)
    where
        H: HasData<TypeData, Data = UnionFindSetPtr<RuntimeType>>
            + CanStoreData<ConcreteTypeData, Data = ConcreteRuntimeType>
            + HasSourceRange,
    {
        match self.concretize_type(*self.type_data.get(node), node.get_source_range()) {
            Some(type_) => {
                self.concrete_type_data.insert(node, type_);
            }
            None => {
                self.failure = true;
            }
        }
    }
}

impl Pass for TypeConcretizer<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = self.program.take().unwrap();
        let _ = self.visit_program(program);

        if self.failure {
            return Err(PassError::InvalidInput {
                producing_pass: Self::name(),
                source: "Not all types could be concretized".into(),
            });
        }

        self.concrete_referenced_variable_entry.insert(
            self.concrete_referenced_variable
                .finish(program)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?
                .into(),
        );
        self.concrete_type_data_entry.insert(
            self.concrete_type_data
                .finish(program)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?
                .into(),
        );
        self.concrete_contained_scope_entry.insert(
            self.concrete_contained_scope
                .finish(program)
                .map_err(|err| PassError::CompilerError {
                    producing_pass: Self::name(),
                    source: err.into(),
                })?
                .into(),
        );
        self.concrete_referenced_function_entry.insert(
            self.concrete_referenced_function
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

impl PartialAstVisitor for TypeConcretizer<'_> {
    type Break<'a> = ();

    fn visit_pre<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        let scope = self.contained_scope.get(&node);

        if let Some(scope) = self.concretize_scope(&scope.borrow()) {
            self.concrete_contained_scope.insert(&node, scope);
        } else {
            self.failure = true;
        }

        ControlFlow::Continue(())
    }

    fn visit_simple_binding<'a>(
        &mut self,
        binding: &'a mut SimpleBinding,
    ) -> ControlFlow<Self::Break<'a>> {
        self.concretize_node(binding);

        if let Some(var) = self.concretize_variable(self.referenced_variable.get(binding)) {
            self.concrete_referenced_variable.insert(binding, var);
        } else {
            self.failure = true;
        }

        self.walk_simple_binding(binding)
    }

    fn visit_function_definition<'a>(
        &mut self,
        fdef: &'a mut FunctionDefinition,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(func) = self.concretize_function(self.referenced_function.get(fdef)) {
            self.concrete_referenced_function
                .insert(fdef, Rc::new(RefCell::new(func)));
        } else {
            self.failure = true;
        }

        self.walk_function_definition(fdef)
    }

    fn visit_extern_function_body<'a>(
        &mut self,
        body: &'a mut ExternFunctionBody,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(func) = self.concretize_function(self.referenced_function.get(body)) {
            self.concrete_referenced_function
                .insert(body, Rc::new(RefCell::new(func)));
        } else {
            self.failure = true;
        }

        self.walk_extern_function_body(body)
    }

    fn visit_variable_definition_statement<'a>(
        &mut self,
        stmt: &'a mut VariableDefinitionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(var) = self.concretize_variable(self.referenced_variable.get(stmt)) {
            self.concrete_referenced_variable.insert(stmt, var);
        } else {
            self.failure = true;
        }

        self.walk_variable_definition_statement(stmt)
    }

    fn visit_expression<'a>(&mut self, expr: &'a mut Expression) -> ControlFlow<Self::Break<'a>> {
        self.concretize_node(expr);
        self.walk_expression(expr)
    }

    fn visit_function_call_expression<'a>(
        &mut self,
        expr: &'a mut FunctionCallExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(func) = self.concretize_function(self.referenced_function.get(expr)) {
            self.concrete_referenced_function
                .insert(expr, Rc::new(RefCell::new(func)));
        } else {
            self.failure = true;
        }

        self.walk_function_call_expression(expr)
    }

    fn visit_variable_access_expression<'a>(
        &mut self,
        expr: &'a mut VariableAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(var) = self.concretize_variable(self.referenced_variable.get(expr)) {
            self.concrete_referenced_variable.insert(expr, var);
        } else {
            self.failure = true;
        }

        self.walk_variable_access_expression(expr)
    }

    fn visit_lvalue<'a>(&mut self, lvalue: &'a mut LValue) -> ControlFlow<Self::Break<'a>> {
        self.concretize_node(lvalue);
        self.walk_lvalue(lvalue)
    }

    fn visit_variable_lvalue<'a>(
        &mut self,
        lvalue: &'a mut VariableLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(var) = self.concretize_variable(self.referenced_variable.get(lvalue)) {
            self.concrete_referenced_variable.insert(lvalue, var);
        } else {
            self.failure = true;
        }

        self.walk_variable_lvalue(lvalue)
    }

    fn visit_type<'a>(&mut self, ty: &'a mut Type) -> ControlFlow<Self::Break<'a>> {
        self.concretize_node(ty);
        self.walk_type(ty)
    }
}
