use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    rc::Rc,
};

use crate::{
    ast::Program,
    error_reporting::{ErrorKind, PrefetchedType},
    passes::{
        find_node_bounds::find_node_bounds,
        find_node_by_id::find_node_by_id,
        pass_manager::{
            ConcreteFunctionData, ConcreteScopeData, ConcreteTypeData, ConcreteVariableData,
            Errors, FunctionData, GlobalState, Pass, PassError, PassFactory, PassResult, ScopeData,
            TypeData, TypeSets, VariableData,
        },
        runtime_type::{ConcreteRuntimeType, RuntimeType, RuntimeTypeKind},
        scope_analysis::{
            ConcreteFunction, ConcreteVariable, ConcreteVariableScope, Function, ScopeID, Variable,
            VariableID, VariableScope,
        },
        union_find_set::UnionFindSetPtr,
    },
    tokenizer::{NamedSourceRange, SourceRange},
};
pub struct TypeConcretisationPass<'state> {
    errors: RefMut<'state, Errors>,
    //source: Ref<'state, InputSource>,
    program: Ref<'state, Program>,

    referenced_variable: Ref<'state, VariableData>,
    referenced_function: Ref<'state, FunctionData>,
    contained_scope: Ref<'state, ScopeData>,
    type_sets: Ref<'state, TypeSets>,
    type_data: Ref<'state, TypeData>,

    visited_types: HashMap<UnionFindSetPtr<RuntimeType>, Option<ConcreteRuntimeType>>,

    concrete_referenced_variable: RefMut<'state, ConcreteVariableData>,
    concrete_referenced_function: RefMut<'state, ConcreteFunctionData>,
    concrete_contained_scope: RefMut<'state, ConcreteScopeData>,
    concrete_type_data: RefMut<'state, ConcreteTypeData>,

    concrete_variable_by_id: HashMap<VariableID, Rc<RefCell<ConcreteVariable>>>,
    concrete_scope_by_id: HashMap<ScopeID, Rc<RefCell<ConcreteVariableScope>>>,
}

impl PassFactory for TypeConcretisationPass<'_> {
    type Output<'state> = TypeConcretisationPass<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        let errors = state.check_named()?;
        let program = state.check_named()?;
        //let source = state.check_named()?;

        let referenced_variable = state.check_named()?;
        let referenced_function = state.check_named()?;
        let contained_scope = state.check_named()?;
        let type_sets = state.check_named()?;
        let type_data = state.check_named()?;

        let concrete_referenced_variable = state.insert_default();
        let concrete_referenced_function = state.insert_default();
        let concrete_contained_scope = state.insert_default();
        let concrete_type_data = state.insert_default();

        Ok(Self::Output {
            errors: errors.get_mut(state),
            //source: source.get(state),
            program: program.get(state),

            referenced_variable: referenced_variable.get(state),
            referenced_function: referenced_function.get(state),
            contained_scope: contained_scope.get(state),
            type_sets: type_sets.get(state),
            type_data: type_data.get(state),

            visited_types: Default::default(),

            concrete_referenced_variable: concrete_referenced_variable.get_mut(state),
            concrete_referenced_function: concrete_referenced_function.get_mut(state),
            concrete_contained_scope: concrete_contained_scope.get_mut(state),
            concrete_type_data: concrete_type_data.get_mut(state),

            concrete_variable_by_id: Default::default(),
            concrete_scope_by_id: Default::default(),
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
                RuntimeTypeKind::Struct {
                    members: _,
                    source_hash: None,
                }
                | RuntimeTypeKind::ArrayOf {
                    subtype: _,
                    size: None,
                }
                | RuntimeTypeKind::Unknown
                | RuntimeTypeKind::Number { .. } => {
                    self.errors.push(ErrorKind::IncompleteTypeInference {
                        range: type_.definition_range.clone().unwrap_or(parent_range),
                        best_guess: Some(PrefetchedType::fetch(type_ptr, &self.type_sets)),
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
                    source_hash: Some(source_hash),
                } => ConcreteRuntimeType::Struct {
                    source_hash,
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
        scope: Rc<RefCell<VariableScope>>,
    ) -> Option<Rc<RefCell<ConcreteVariableScope>>> {
        if let Some(scope) = self.concrete_scope_by_id.get(&scope.borrow().id) {
            return Some(scope.clone());
        }

        let scope = scope.borrow();

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
                Some(self.concretisize_scope(parent)?)
            } else {
                None
            },
        })))
    }
}

impl Pass for TypeConcretisationPass<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut failure = false;
        for (id, var) in self.referenced_variable.clone().iter() {
            let Some(var) = self.concretisize_variable(var) else {
                failure = true;
                continue;
            };

            self.concrete_referenced_variable.insert(*id, var);
        }
        for (id, type_) in self.type_data.clone().iter() {
            let Some(type_) = self.concretisize_type(
                *type_,
                find_node_by_id(&*self.program, *id)
                    .map(find_node_bounds)
                    .unwrap_or_else(|| {
                        SourceRange::empty_start().named(self.program.file_name.clone())
                    }),
            ) else {
                failure = true;
                continue;
            };

            self.concrete_type_data.insert(*id, type_);
        }
        for (id, func) in self.referenced_function.clone().iter() {
            let Some(func) = self.concretisize_function(func) else {
                failure = true;
                continue;
            };

            self.concrete_referenced_function
                .insert(*id, Rc::new(RefCell::new(func)));
        }
        for (id, scope) in self.contained_scope.clone().iter() {
            let Some(scope) = self.concretisize_scope(scope.clone()) else {
                failure = true;
                continue;
            };

            self.concrete_contained_scope.insert(*id, scope);
        }
        if failure {
            return Err(PassError::InvalidInput {
                producing_pass: Self::name(),
                source: "Not all types could be concretisized".into(),
            });
        }

        Ok(())
    }
}
