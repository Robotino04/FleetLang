use std::{cell::RefCell, collections::HashMap, ops::ControlFlow, rc::Rc};

use crate::{
    ast::{
        AstNodeRefMut, Expression, HasSourceRange, LValue, Program, SimpleBinding, Type,
        VariableAccessExpression, VariableDefinitionStatement, VariableLValue,
    },
    error_reporting::{ErrorKind, Errors, PrefetchedType},
    node_data::{CanStoreData, HasData, PerNodeData},
    passes::{
        global_state::{Empty, FetchError, Fetched, GlobalState},
        partial_visitor::{AstWalker, PartialAstVisitor},
        pass_manager::{
            ConcreteFunctionData, ConcreteScopeData, ConcreteTypeData, ConcreteVariableData,
            FunctionData, Pass, PassError, PassFactory, PassResult, ScopeData, TypeData, TypeSets,
            VariableData,
        },
        runtime_type::{ConcreteRuntimeType, RuntimeType, RuntimeTypeKind},
        scope_analysis::{
            ConcreteFunction, ConcreteVariable, ConcreteVariableScope, Function, ScopeID, Variable,
            VariableID, VariableScope,
        },
        union_find_set::UnionFindSetPtr,
    },
    tokenizer::NamedSourceRange,
};
pub struct TypeConcretisationPass<'state> {
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
    concrete_referenced_function: &'state mut ConcreteFunctionData,
    concrete_contained_scope: PerNodeData<ConcreteScopeData>,
    concrete_contained_scope_entry: Empty<'state, ConcreteScopeData>,
    concrete_type_data: PerNodeData<ConcreteTypeData>,
    concrete_type_data_entry: Empty<'state, ConcreteTypeData>,

    concrete_variable_by_id: HashMap<VariableID, Rc<RefCell<ConcreteVariable>>>,
    concrete_scope_by_id: HashMap<ScopeID, Rc<RefCell<ConcreteVariableScope>>>,

    failure: bool,
}

impl PassFactory for TypeConcretisationPass<'_> {
    type Output<'state> = TypeConcretisationPass<'state>;
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
            concrete_referenced_function: concrete_referenced_function.insert_default(),
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

impl TypeConcretisationPass<'_> {
    fn concretisize_function(&mut self, func: &Rc<RefCell<Function>>) -> Option<ConcreteFunction> {
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
            return_type: self.concretisize_type(return_type, func.symbol.definition.clone())?,
            parameter_types: parameters
                .iter()
                .map(|param| self.concretisize_variable(param))
                .collect::<Option<Vec<_>>>()?,
            id: func.id,
            definition_node_id: func.definition_node_id,
        })
    }
    fn concretisize_variable(
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
            type_: self.concretisize_type(type_, var.symbol.definition.clone())?,
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
    fn concretisize_type(
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
                    subtype: Box::new(self.concretisize_type(
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
                            Some((name.clone(), self.concretisize_type(type_, range)?))
                        })
                        .collect::<Option<_>>()?,
                },
            })
        };

        let out = out();
        self.visited_types.insert(type_ptr, out.clone());
        out
    }
    fn concretisize_scope(
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
                .map(|(name, var)| Some((name.clone(), self.concretisize_variable(var)?)))
                .collect::<Option<HashMap<_, _>>>()?,
            parent_references: scope
                .parent_references
                .iter()
                .map(|(name, var)| Some((name.clone(), self.concretisize_variable(var)?)))
                .collect::<Option<HashMap<_, _>>>()?,
            copy_from_parent: scope.copy_from_parent,
            is_read_guard: scope.is_read_guard,
            is_write_guard: scope.is_write_guard,
            parent: if let Some(parent) = scope.parent.clone() {
                Some(self.concretisize_scope(&parent.borrow())?)
            } else {
                None
            },
        })))

        // TODO: actually cache scope
    }

    fn concretisize_node<H>(&mut self, node: &mut H)
    where
        H: HasData<TypeData, Data = UnionFindSetPtr<RuntimeType>>
            + CanStoreData<ConcreteTypeData, Data = ConcreteRuntimeType>
            + HasSourceRange,
    {
        match self.concretisize_type(*self.type_data.get(node), node.get_source_range()) {
            Some(type_) => {
                self.concrete_type_data.insert(node, type_);
            }
            None => {
                self.failure = true;
            }
        }
    }
}

impl Pass for TypeConcretisationPass<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let program = self.program.take().unwrap();
        let _ = self.visit_program(program);
        for (id, func) in self.referenced_function.iter() {
            let Some(func) = self.concretisize_function(func) else {
                self.failure = true;
                continue;
            };

            self.concrete_referenced_function
                .insert(*id, Rc::new(RefCell::new(func)));
        }
        if self.failure {
            return Err(PassError::InvalidInput {
                producing_pass: Self::name(),
                source: "Not all types could be concretisized".into(),
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

        Ok(())
    }
}

impl PartialAstVisitor for TypeConcretisationPass<'_> {
    type Break<'a> = ();

    fn visit_pre<'a>(&mut self, node: AstNodeRefMut<'a>) -> ControlFlow<Self::Break<'a>> {
        let scope = self.contained_scope.get(&node);

        if let Some(scope) = self.concretisize_scope(&scope.borrow()) {
            self.concrete_contained_scope.insert(&node, scope);
        } else {
            self.failure = true;
        }

        ControlFlow::Continue(())
    }

    fn visit_type<'a>(&mut self, ty: &'a mut Type) -> ControlFlow<Self::Break<'a>> {
        self.concretisize_node(ty);
        self.walk_type(ty)
    }

    fn visit_expression<'a>(&mut self, expr: &'a mut Expression) -> ControlFlow<Self::Break<'a>> {
        self.concretisize_node(expr);
        self.walk_expression(expr)
    }

    fn visit_variable_access_expression<'a>(
        &mut self,
        expr: &'a mut VariableAccessExpression,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(var) = self.concretisize_variable(self.referenced_variable.get(expr)) {
            self.concrete_referenced_variable.insert(expr, var);
        } else {
            self.failure = true;
        }

        self.walk_variable_access_expression(expr)
    }

    fn visit_simple_binding<'a>(
        &mut self,
        binding: &'a mut SimpleBinding,
    ) -> ControlFlow<Self::Break<'a>> {
        self.concretisize_node(binding);

        if let Some(var) = self.concretisize_variable(self.referenced_variable.get(binding)) {
            self.concrete_referenced_variable.insert(binding, var);
        } else {
            self.failure = true;
        }

        self.walk_simple_binding(binding)
    }

    fn visit_lvalue<'a>(&mut self, lvalue: &'a mut LValue) -> ControlFlow<Self::Break<'a>> {
        self.concretisize_node(lvalue);
        self.walk_lvalue(lvalue)
    }

    fn visit_variable_lvalue<'a>(
        &mut self,
        lvalue: &'a mut VariableLValue,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(var) = self.concretisize_variable(self.referenced_variable.get(lvalue)) {
            self.concrete_referenced_variable.insert(lvalue, var);
        } else {
            self.failure = true;
        }

        self.walk_variable_lvalue(lvalue)
    }

    fn visit_variable_definition_statement<'a>(
        &mut self,
        stmt: &'a mut VariableDefinitionStatement,
    ) -> ControlFlow<Self::Break<'a>> {
        if let Some(var) = self.concretisize_variable(self.referenced_variable.get(stmt)) {
            self.concrete_referenced_variable.insert(stmt, var);
        } else {
            self.failure = true;
        }

        self.walk_variable_definition_statement(stmt)
    }
}
