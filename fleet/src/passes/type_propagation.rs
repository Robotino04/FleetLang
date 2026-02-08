use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    rc::Rc,
};

use itertools::{EitherOrBoth, Itertools};

use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, BreakStatement, CastExpression,
        CompilerExpression, Expression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionCallExpression, FunctionDefinition, GPUExecutor, GroupingExpression,
        GroupingLValue, HasID, IdkType, IfStatement, LiteralExpression, LiteralKind, OnStatement,
        OnStatementIterator, Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType,
        SkipStatement, StatementFunctionBody, StructAccessExpression, StructAccessLValue,
        StructExpression, StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor,
        TopLevelStatement, TypeAlias, UnaryExpression, UnaryOperation, UnitType,
        VariableAccessExpression, VariableAssignmentExpression, VariableDefinitionStatement,
        VariableLValue, WhileLoopStatement,
    },
    infra::{
        CastDirection, DuplicateKind, ErrorKind, ExtraParameter, ImpossibleCastReason,
        InternalError, Lint, MissingParameter, NotDefinedKind, ParameterCountDifference,
        PrefetchedType, SymbolDefinition, TypeMismatchKind, UnresolvedSymbol,
    },
    parser::IdGenerator,
    passes::{
        find_node_bounds::find_node_bounds,
        pass_manager::{
            Errors, FunctionData, GlobalState, Pass, PassFactory, PassResult, ScopeData, TypeData,
            TypeSets, VariableData,
        },
        runtime_type::{RuntimeType, RuntimeTypeKind},
        scope_analysis::Function,
        union_find_set::UnionFindSetPtr,
    },
    tokenizer::NamedSourceRange,
};

pub struct TypePropagator<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,
    id_generator: RefMut<'state, IdGenerator>,

    type_sets: RefMut<'state, TypeSets>,
    node_types: RefMut<'state, TypeData>,
    referenced_variable: Ref<'state, VariableData>,
    referenced_function: Ref<'state, FunctionData>,
    containing_scope: Ref<'state, ScopeData>,

    type_aliases: HashMap<String, (NamedSourceRange, UnionFindSetPtr<RuntimeType>)>,

    current_function: Option<Rc<RefCell<Function>>>,
    require_constant: bool,
}

impl PassFactory for TypePropagator<'_> {
    type Output<'state> = TypePropagator<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String> {
        let errors = state.check_named()?;
        let program = state.check_named()?;
        let id_generator = state.check_named()?;

        let type_sets = state.insert_default();
        let node_types = state.insert_default();
        let referenced_variable = state.check_named()?;
        let referenced_function = state.check_named()?;
        let scope_data = state.check_named()?;

        Ok(Self::Output {
            errors: errors.get_mut(state),
            program: Some(program.get_mut(state)),
            id_generator: id_generator.get_mut(state),

            type_sets: type_sets.get_mut(state),
            node_types: node_types.get_mut(state),
            referenced_variable: referenced_variable.get(state),
            referenced_function: referenced_function.get(state),
            containing_scope: scope_data.get(state),

            type_aliases: Default::default(),

            current_function: None,
            require_constant: false,
        })
    }
}
impl Pass for TypePropagator<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        let mut program = self.program.take().unwrap();
        self.visit_program(&mut program);

        Ok(())
    }
}

impl<'a> TypePropagator<'a> {
    fn register_top_level_statements(&mut self, tls: &mut TopLevelStatement) {
        match tls {
            TopLevelStatement::Function(function_definition) => {
                self.register_function(function_definition)
            }
            TopLevelStatement::TypeAlias(type_alias) => self.register_type_alias(type_alias),
        }
    }

    fn register_function(&mut self, function: &mut FunctionDefinition) {
        let FunctionDefinition {
            let_token: _,
            name,
            name_token,
            equal_token: _,
            open_paren_token: _,
            parameters,
            close_paren_token: _,
            right_arrow_token: _,
            return_type,
            body: _,
            id,
        } = function;

        let return_type = return_type
            .as_mut()
            .map(|t| self.visit_type(t))
            .unwrap_or_else(|| {
                self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Unknown,
                    definition_range: None,
                })
            });
        let parameter_types = parameters
            .iter_mut()
            .map(|(param, _comma)| {
                self.visit_simple_binding(param);
                self.referenced_variable.get(&param.id).unwrap().clone()
            })
            .collect();

        let Some(ref_func) = self.referenced_function.get(id) else {
            self.errors.push(ErrorKind::InternalError(
                InternalError::ScopeAnalysisMissedFunction {
                    function: SymbolDefinition::from_token(name.clone(), name_token),
                },
            ));
            return;
        };
        *ref_func.borrow_mut() = Function {
            symbol: SymbolDefinition::from_token(name.clone(), name_token),
            return_type: Some(return_type),
            parameter_types: Some(parameter_types),
            id: self.id_generator.next_function_id(),
            definition_node_id: *id,
        };
    }
    fn register_type_alias(&mut self, type_alias: &mut TypeAlias) {
        let TypeAlias {
            let_token: _,
            name,
            name_token,
            equal_token: _,
            type_: _,
            semicolon_token: _,
            id: _,
        } = type_alias;

        let type_ = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Unknown,
            definition_range: Some(name_token.range.clone()),
        });

        if let Some((original_range, _original_type)) = self
            .type_aliases
            .insert(name.clone(), (name_token.range.clone(), type_))
        {
            self.errors.push(ErrorKind::Duplicate {
                kind: DuplicateKind::TypeAlias,
                original: SymbolDefinition::new(name.clone(), original_range),
                new_range: name_token.range.clone(),
            });
        }
    }

    fn require_fully_specialized_scope(&mut self, scope_node: &impl HasID) {
        let scope = self
            .containing_scope
            .get(&scope_node.get_id())
            .unwrap()
            .borrow();

        for variable in scope.variable_map.values() {
            let actual_type_ptr = variable.borrow().type_.unwrap();
            let actual_type = self.type_sets.get_mut(actual_type_ptr);
            actual_type.kind.specialize_number_size();

            // release the borrow
            let actual_type = actual_type.clone();

            if let RuntimeTypeKind::Unknown
            | RuntimeTypeKind::Number { .. }
            | RuntimeTypeKind::ArrayOf {
                subtype: _,
                size: None,
            } = actual_type.kind
            {
                self.errors
                    .push(ErrorKind::IncompleteTypeInferenceVariable {
                        variable: variable.borrow().symbol.clone(),
                        best_guess: Some(PrefetchedType::fetch(actual_type_ptr, &self.type_sets)),
                    });
            }
        }
    }
}

impl AstVisitor for TypePropagator<'_> {
    type ProgramOutput = ();
    type TopLevelOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = UnionFindSetPtr<RuntimeType>;
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = UnionFindSetPtr<RuntimeType>;
    type LValueOutput = UnionFindSetPtr<RuntimeType>;
    type TypeOutput = UnionFindSetPtr<RuntimeType>;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        for tls in &mut program.top_level_statements {
            self.register_top_level_statements(tls);
        }
        for tls in &mut program.top_level_statements {
            self.visit_top_level_statement(tls);
        }

        // specify all int sizes
        for type_ in self.type_sets.data.values_mut() {
            type_.kind.specialize_number_size();
        }
    }

    fn visit_function_definition(&mut self, fdef: &mut FunctionDefinition) -> Self::TopLevelOutput {
        let FunctionDefinition {
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
        } = fdef;

        let this_function = self
            .referenced_function
            .get(id)
            .expect("All functions should have been registered before traversing the tree")
            .clone();

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }

        self.current_function = Some(this_function.clone());

        for (param, _comma) in parameters {
            self.visit_simple_binding(param);
        }

        self.visit_function_body(body);

        self.require_fully_specialized_scope(&**body);
    }

    fn visit_type_alias(&mut self, type_alias: &mut TypeAlias) -> Self::TopLevelOutput {
        let TypeAlias {
            let_token: _,
            name,
            name_token: _,
            equal_token: _,
            type_,
            semicolon_token: _,
            id: _,
        } = type_alias;

        let type_ = self.visit_type(type_);

        self.type_sets.get_mut(type_).kind.specialize_number_size();

        let (_range, aliased_type) = self.type_aliases.get(name).unwrap();

        // ignore failures because this should always succeed for valid programs
        let _ = RuntimeTypeKind::merge_types(type_, *aliased_type, &mut self.type_sets);
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_statement(statement);
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token: _,
            id: _,
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
    }

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let SimpleBinding {
            name_token,
            name,
            type_,
            id,
        } = simple_binding;

        let defined_type_ptr = if let Some((_colon, type_)) = type_ {
            self.visit_type(type_)
        } else {
            self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Unknown,
                definition_range: None,
            })
        };
        if self.type_sets.get(defined_type_ptr).kind == RuntimeTypeKind::Unit {
            self.errors.push(ErrorKind::UnitVariable {
                variable: SymbolDefinition::from_token(name.clone(), name_token),
                type_range: type_
                    .as_ref()
                    .map(|(_colon, type_)| find_node_bounds(type_)),
                type_: PrefetchedType::fetch(defined_type_ptr, &self.type_sets),
            });
        }

        self.referenced_variable.get(id).unwrap().borrow_mut().type_ = Some(defined_type_ptr);

        self.node_types.insert(*id, defined_type_ptr);

        defined_type_ptr
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let _type = self.visit_expression(expression);
    }

    fn visit_on_statement(&mut self, stmt: &mut OnStatement) -> Self::StatementOutput {
        let OnStatement {
            on_token,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id: _,
        } = stmt;

        self.visit_executor(executor);

        // TODO: remove/inline into loop after we allow triangular iterators
        let iterators = iterators.iter_mut().map(|osi| {
            let max_value_type = self.visit_expression(&mut osi.max_value);
            (osi, max_value_type)
        });

        for (
            OnStatementIterator {
                open_bracket_token: _,
                binding,
                equal_token: _,
                max_value,
                close_bracket_token: _,
            },
            max_value_type,
        ) in iterators.collect_vec()
        {
            let iterator_type = self.visit_simple_binding(binding);
            if !RuntimeTypeKind::merge_types(iterator_type, max_value_type, &mut self.type_sets) {
                self.errors.push(ErrorKind::IteratorValueMaxMismatch {
                    on_range: on_token.range.clone(),
                    iterator: SymbolDefinition::from_token(
                        binding.name.clone(),
                        &binding.name_token,
                    ),
                    max_value_range: find_node_bounds(&**max_value),
                    type_range: binding
                        .type_
                        .as_ref()
                        .map(|(_colon, type_)| find_node_bounds(type_)),
                    iterator_type: PrefetchedType::fetch(iterator_type, &self.type_sets),
                    max_type: PrefetchedType::fetch(max_value_type, &self.type_sets),
                });
            }

            // TODO: HACK: remove when we have proper constness and mutability
            self.referenced_variable
                .get(&binding.id)
                .expect("variable data should exist by now")
                .borrow_mut()
                .is_constant = true;

            let is_iterator_const = self
                .referenced_variable
                .get(&binding.id)
                .expect("variable data should exist by now")
                .borrow()
                .is_constant;

            if !is_iterator_const {
                self.errors.push(ErrorKind::MutableIterator {
                    on_range: on_token.range.clone(),
                    iterator: SymbolDefinition::from_token(
                        binding.name.clone(),
                        &binding.name_token,
                    ),
                });
            }
            let possible_iterator_type = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Number {
                    signed: Some(false),
                    integer: Some(true),
                },
                definition_range: None,
            });
            if !RuntimeTypeKind::merge_types(
                possible_iterator_type,
                iterator_type,
                &mut self.type_sets,
            ) {
                self.errors.push(ErrorKind::ImpossibleIteratorType {
                    on_range: on_token.range.clone(),
                    iterator: SymbolDefinition::from_token(
                        binding.name.clone(),
                        &binding.name_token,
                    ),
                    type_range: binding
                        .type_
                        .as_ref()
                        .map(|(_colon, type_)| find_node_bounds(type_)),
                    iterator_type: PrefetchedType::fetch(iterator_type, &self.type_sets),
                    possible_iterator_type: PrefetchedType::fetch(
                        possible_iterator_type,
                        &self.type_sets,
                    ),
                });
            }
        }

        self.require_constant = true;
        for (binding, _comma) in bindings {
            self.visit_lvalue(binding);
        }
        self.require_constant = false;

        self.visit_statement(body);

        self.require_fully_specialized_scope(&**body);
    }

    fn visit_block_statement(&mut self, stmt: &mut BlockStatement) -> Self::StatementOutput {
        let BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        } = stmt;

        for stmt in &mut *body {
            self.visit_statement(stmt);
        }

        if let Some(body_part) = body.first() {
            self.require_fully_specialized_scope(body_part);
        }
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement) -> Self::StatementOutput {
        let ReturnStatement {
            return_token,
            value,
            semicolon_token: _,
            id,
        } = stmt;

        let this_type_ptr = value
            .as_mut()
            .map(|value| self.visit_expression(value))
            .unwrap_or_else(|| {
                self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Unit,
                    definition_range: None,
                })
            });

        let current_function = self
            .current_function
            .as_ref()
            .expect("Return statements should only appear inside functions")
            .borrow();

        let expected_type_ptr = current_function.return_type.unwrap();

        if !RuntimeTypeKind::merge_types(this_type_ptr, expected_type_ptr, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::FunctionReturn {
                    function: current_function.symbol.clone(),
                },
                value_range: value
                    .as_ref()
                    .map(|value| find_node_bounds(&**value))
                    .unwrap_or_else(|| return_token.range.clone()),
                expected_types: vec![PrefetchedType::fetch(expected_type_ptr, &self.type_sets)],
                actual_type: PrefetchedType::fetch(this_type_ptr, &self.type_sets),
            });
        }

        self.node_types.insert(*id, this_type_ptr);
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
        let value_type_ptr = self.visit_expression(value);
        let defined_type_ptr = self.visit_simple_binding(binding);

        let var = self.referenced_variable.get(id).unwrap();

        if !RuntimeTypeKind::merge_types(value_type_ptr, defined_type_ptr, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::VariableInitializer {
                    variable: var.borrow().symbol.clone(),
                },
                value_range: find_node_bounds(&**value),
                expected_types: vec![PrefetchedType::fetch(defined_type_ptr, &self.type_sets)],
                actual_type: PrefetchedType::fetch(value_type_ptr, &self.type_sets),
            });
        }
    }

    fn visit_if_statement(
        &mut self,
        IfStatement {
            if_token: _,
            condition,
            if_body,
            elifs,
            else_,
            id: _,
        }: &mut IfStatement,
    ) -> Self::StatementOutput {
        let cond_type = self.visit_expression(condition);

        let boolean = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Boolean,
            definition_range: None,
        });

        if !RuntimeTypeKind::merge_types(cond_type, boolean, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::IfCondition,
                value_range: find_node_bounds(&**condition),
                expected_types: vec![PrefetchedType::fetch(boolean, &self.type_sets)],
                actual_type: PrefetchedType::fetch(cond_type, &self.type_sets),
            });
        }

        self.visit_statement(if_body);
        for (_elif_token, elif_condition, elif_body) in elifs {
            let elif_type = self.visit_expression(elif_condition);
            if !RuntimeTypeKind::merge_types(elif_type, boolean, &mut self.type_sets) {
                self.errors.push(ErrorKind::TypeMismatch {
                    kind: TypeMismatchKind::ElifCondition,
                    value_range: find_node_bounds(&*elif_condition),
                    expected_types: vec![PrefetchedType::fetch(boolean, &self.type_sets)],
                    actual_type: PrefetchedType::fetch(elif_type, &self.type_sets),
                });
            }

            self.visit_statement(elif_body);
        }
        if let Some((_else_token, else_body)) = else_ {
            self.visit_statement(else_body);
        }
    }

    fn visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        }: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        let cond_type = self.visit_expression(condition);

        let boolean = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Boolean,
            definition_range: None,
        });

        if !RuntimeTypeKind::merge_types(cond_type, boolean, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::WhileCondition,
                value_range: find_node_bounds(&**condition),
                expected_types: vec![PrefetchedType::fetch(boolean, &self.type_sets)],
                actual_type: PrefetchedType::fetch(cond_type, &self.type_sets),
            });
        }

        self.visit_statement(body);
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
            id: _,
        }: &mut ForLoopStatement,
    ) -> Self::StatementOutput {
        self.visit_statement(initializer);

        if let Some(con) = condition {
            let cond_type = self.visit_expression(con);

            let boolean = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Boolean,
                definition_range: None,
            });

            if !RuntimeTypeKind::merge_types(cond_type, boolean, &mut self.type_sets) {
                self.errors.push(ErrorKind::TypeMismatch {
                    kind: TypeMismatchKind::ForCondition,
                    value_range: find_node_bounds(&**con),
                    expected_types: vec![PrefetchedType::fetch(boolean, &self.type_sets)],
                    actual_type: PrefetchedType::fetch(cond_type, &self.type_sets),
                });
            }
        }
        if let Some(inc) = incrementer {
            self.visit_expression(inc);
        }
        self.visit_statement(body);

        self.require_fully_specialized_scope(&**body);
    }

    fn visit_break_statement(
        &mut self,
        BreakStatement {
            break_token: _,
            semicolon_token: _,
            id: _,
        }: &mut BreakStatement,
    ) -> Self::StatementOutput {
    }

    fn visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id: _,
        }: &mut SkipStatement,
    ) -> Self::StatementOutput {
    }

    fn visit_self_executor_host(
        &mut self,
        SelfExecutorHost { token: _, id: _ }: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
    }

    fn visit_thread_executor(
        &mut self,
        ThreadExecutor {
            host,
            dot_token: _,
            thread_token,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ThreadExecutor,
    ) -> Self::ExecutorOutput {
        self.visit_executor_host(host);
        let index_type = self.visit_expression(index);

        let expected_type = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Number {
                signed: Some(false),
                integer: Some(true),
            },
            definition_range: None,
        });

        if !RuntimeTypeKind::merge_types(index_type, expected_type, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::ThreadIndex {
                    thread: thread_token.range.clone(),
                },
                value_range: find_node_bounds(&**index),
                expected_types: vec![PrefetchedType::fetch(expected_type, &self.type_sets)],
                actual_type: PrefetchedType::fetch(index_type, &self.type_sets),
            });
        };
    }

    fn visit_gpu_executor(
        &mut self,
        GPUExecutor {
            host,
            dot_token: _,
            gpus_token,
            open_bracket_token: _,
            gpu_index,
            close_bracket_token: _,
            id: _,
        }: &mut GPUExecutor,
    ) -> Self::ExecutorOutput {
        self.visit_executor_host(host);
        let index_type = self.visit_expression(gpu_index);

        let expected_type = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Number {
                signed: Some(false),
                integer: Some(true),
            },
            definition_range: None,
        });

        if !RuntimeTypeKind::merge_types(index_type, expected_type, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::GpuIndex {
                    gpu: gpus_token.range.clone(),
                },
                value_range: find_node_bounds(&**gpu_index),
                expected_types: vec![PrefetchedType::fetch(expected_type, &self.type_sets)],
                actual_type: PrefetchedType::fetch(index_type, &self.type_sets),
            });
        };
    }

    fn visit_literal_expression(
        &mut self,
        expression: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        let LiteralExpression {
            value,
            token: _,
            id,
        } = expression;
        let type_ = self.type_sets.insert_set(RuntimeType {
            kind: match value {
                LiteralKind::Number(_) => RuntimeTypeKind::Number {
                    signed: None,
                    integer: None,
                },
                LiteralKind::Char(_) => RuntimeTypeKind::U8,
                LiteralKind::Float(_) => RuntimeTypeKind::Number {
                    signed: Some(true),
                    integer: Some(false),
                },
                LiteralKind::Bool(_) => RuntimeTypeKind::Boolean,
            },
            definition_range: None,
        });
        self.node_types.insert(*id, type_);

        type_
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

        let element_type_ptr = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Unknown,
            definition_range: None,
        });

        let first_item_range = elements.first().map(|el| find_node_bounds(&el.0));
        for (item, _comma) in &mut *elements {
            let this_item_type_ptr = self.visit_expression(item);

            if !RuntimeTypeKind::merge_types(
                element_type_ptr,
                this_item_type_ptr,
                &mut self.type_sets,
            ) {
                self.errors.push(ErrorKind::TypeMismatch {
                    kind: TypeMismatchKind::ArrayElement {
                        first_element_range: first_item_range.clone().unwrap(),
                    },
                    value_range: find_node_bounds(item),

                    expected_types: vec![PrefetchedType::fetch(element_type_ptr, &self.type_sets)],
                    actual_type: PrefetchedType::fetch(this_item_type_ptr, &self.type_sets),
                });
            }
        }

        let type_ = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::ArrayOf {
                subtype: element_type_ptr,
                size: Some(elements.len()),
            },
            definition_range: None,
        });
        self.node_types.insert(*id, type_);
        type_
    }

    fn visit_struct_expression(
        &mut self,
        expression: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        let expression_range = find_node_bounds(&*expression);
        let StructExpression {
            type_,
            open_brace_token,
            members,
            close_brace_token,
            id,
        } = expression;

        let defined_type_ptr = self.visit_type(type_);

        let mut member_types = vec![];

        let defined_members = match &self.type_sets.get(defined_type_ptr).kind {
            RuntimeTypeKind::Struct {
                members,
                source_hash: _,
            } => Some(members.clone()),
            RuntimeTypeKind::Unknown => None,
            _ => {
                self.errors.push(ErrorKind::StructInitializeNonStruct {
                    expression_range,
                    type_range: find_node_bounds(&*type_),
                    wrong_type: PrefetchedType::fetch(defined_type_ptr, &self.type_sets),
                });

                None
            }
        };

        for (
            i,
            (
                StructMemberValue {
                    name,
                    name_token,
                    colon_token: _,
                    value,
                },
                _comma,
            ),
        ) in members.iter_mut().enumerate()
        {
            let this_type_ptr = self.visit_expression(value);
            member_types.push((name.clone(), name_token.range.clone(), this_type_ptr));

            // TODO: allow reordering
            if let Some(defined_members) = defined_members.as_ref()
                && let Some((this_defined_name, this_defined_range, this_defined_type_ptr)) =
                    defined_members.get(i).cloned()
            {
                if *name != this_defined_name {
                    self.errors.push(ErrorKind::StructMemberMismatch {
                        member_index: i,
                        struct_type: PrefetchedType::fetch(defined_type_ptr, &self.type_sets),
                        expected: SymbolDefinition::new(
                            this_defined_name.clone(),
                            this_defined_range.clone(),
                        ),
                        actual: UnresolvedSymbol::from_token(name.clone(), name_token),
                    });
                }
                if !RuntimeTypeKind::merge_types(
                    this_type_ptr,
                    this_defined_type_ptr,
                    &mut self.type_sets,
                ) {
                    self.errors.push(ErrorKind::TypeMismatch {
                        kind: TypeMismatchKind::StructMember {
                            member_index: i,
                            member: SymbolDefinition::new(
                                this_defined_name.clone(),
                                this_defined_range.clone(),
                            )
                            .with_use(name_token.range.clone()),
                        },
                        value_range: find_node_bounds(&**value),
                        expected_types: vec![PrefetchedType::fetch(
                            this_defined_type_ptr,
                            &self.type_sets,
                        )],
                        actual_type: PrefetchedType::fetch(this_type_ptr, &self.type_sets),
                    });
                }
            }
        }

        let constructed_type_ptr = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Struct {
                members: member_types,
                source_hash: None,
            },
            definition_range: None,
        });

        if !RuntimeTypeKind::merge_types(
            defined_type_ptr,
            constructed_type_ptr,
            &mut self.type_sets,
        ) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::StructFields {
                    type_expression: find_node_bounds(&*type_),
                },
                value_range: open_brace_token
                    .range
                    .clone()
                    .extend_with(close_brace_token.range.clone()),
                expected_types: vec![PrefetchedType::fetch(defined_type_ptr, &self.type_sets)],
                actual_type: PrefetchedType::fetch(constructed_type_ptr, &self.type_sets),
            });
        }

        self.node_types.insert(*id, constructed_type_ptr);
        constructed_type_ptr
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let FunctionCallExpression {
            name,
            name_token,
            open_paren_token: _,
            arguments: _,
            close_paren_token: _,
            id,
        } = expression;
        let Some(ref_function) = self.referenced_function.get(id).cloned() else {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::Function,
                item: UnresolvedSymbol::from_token(name.clone(), name_token),
            });

            // still typecheck args, even though the function doesn't exist
            for (arg, _comma) in &mut expression.arguments {
                self.visit_expression(arg);
            }

            return self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            });
        };

        let mut extra_parameters = Vec::new();
        let mut missing_parameters = Vec::new();

        for (i, types) in expression
            .arguments
            .iter_mut()
            .map(|(arg, _comma)| (self.visit_expression(arg), arg))
            .collect_vec()
            .iter()
            .zip_longest(
                ref_function
                    .borrow()
                    .parameter_types
                    .as_ref()
                    .unwrap()
                    .iter(),
            )
            .enumerate()
        {
            match types {
                EitherOrBoth::Both((arg_type_ptr, arg), param) => {
                    // function arguments shouldn't get specialized by calling the function
                    let param_type_ptr = param.borrow().type_.unwrap_or_else(|| {
                        self.type_sets.insert_set(RuntimeType {
                            kind: RuntimeTypeKind::Unknown,
                            definition_range: None,
                        })
                    });
                    let param_type_ptr = self.type_sets.detach(param_type_ptr);

                    if !RuntimeTypeKind::merge_types(
                        *arg_type_ptr,
                        param_type_ptr,
                        &mut self.type_sets,
                    ) {
                        self.errors.push(ErrorKind::TypeMismatch {
                            kind: TypeMismatchKind::FunctionCallParameter {
                                parameter_index: i,
                                parameter: param.borrow().symbol.clone(),
                                function: ref_function
                                    .borrow()
                                    .symbol
                                    .clone()
                                    .with_use(name_token.range.clone()),
                            },
                            value_range: find_node_bounds(&**arg),
                            expected_types: vec![PrefetchedType::fetch(
                                param_type_ptr,
                                &self.type_sets,
                            )],
                            actual_type: PrefetchedType::fetch(*arg_type_ptr, &self.type_sets),
                        });
                    }
                }
                EitherOrBoth::Left((arg_type_ptr, arg)) => {
                    extra_parameters.push(ExtraParameter {
                        range: find_node_bounds(&**arg),
                        type_: PrefetchedType::fetch(*arg_type_ptr, &self.type_sets),
                    });
                }
                EitherOrBoth::Right(param) => {
                    let param_type_ptr = param.borrow().type_.unwrap_or_else(|| {
                        self.type_sets.insert_set(RuntimeType {
                            kind: RuntimeTypeKind::Unknown,
                            definition_range: None,
                        })
                    });

                    missing_parameters.push(MissingParameter {
                        name: param.borrow().symbol.name.clone(),
                        name_range: Some(param.borrow().symbol.definition.clone()),
                        type_: PrefetchedType::fetch(param_type_ptr, &self.type_sets),
                    });
                }
            }
        }

        match (extra_parameters.is_empty(), missing_parameters.is_empty()) {
            (true, true) => {}
            (true, false) => {
                let ref_function = ref_function.borrow();

                self.errors
                    .push(ErrorKind::FunctionCallWrongParameterCount {
                        function: ref_function
                            .symbol
                            .clone()
                            .with_use(name_token.range.clone()),
                        difference: ParameterCountDifference::TooFewGiven(missing_parameters),
                        defined_parameter_count: ref_function
                            .parameter_types
                            .as_ref()
                            .unwrap()
                            .len(),
                    });
            }
            (false, true) => {
                let ref_function = ref_function.borrow();

                self.errors
                    .push(ErrorKind::FunctionCallWrongParameterCount {
                        function: ref_function
                            .symbol
                            .clone()
                            .with_use(name_token.range.clone()),
                        difference: ParameterCountDifference::TooManyGiven(extra_parameters),
                        defined_parameter_count: ref_function
                            .parameter_types
                            .as_ref()
                            .unwrap()
                            .len(),
                    });
            }
            (false, false) => {
                unreachable!()
            }
        }

        let detached_return_type = self
            .type_sets
            .detach(ref_function.borrow().return_type.unwrap());

        self.node_types.insert(*id, detached_return_type);

        detached_return_type
    }

    fn visit_compiler_expression(
        &mut self,
        expression: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        let CompilerExpression {
            at_token: _,
            name,
            name_token,
            open_paren_token: _,
            arguments: _,
            close_paren_token: _,
            id,
        } = expression;
        let (parameter_types, return_type): (
            Vec<(UnionFindSetPtr<RuntimeType>, String)>,
            UnionFindSetPtr<RuntimeType>,
        ) = match name.as_str() {
            "zero" => (
                vec![],
                self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Unknown,
                    definition_range: None,
                }),
            ),
            "sqrt" => {
                let type_ = self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Number {
                        signed: Some(true),
                        integer: Some(false),
                    },
                    definition_range: None,
                });
                (vec![(type_, "value".to_string())], type_)
            }
            "sin" => {
                let type_ = self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Number {
                        signed: Some(true),
                        integer: Some(false),
                    },
                    definition_range: None,
                });
                (vec![(type_, "value".to_string())], type_)
            }
            "cos" => {
                let type_ = self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Number {
                        signed: Some(true),
                        integer: Some(false),
                    },
                    definition_range: None,
                });
                (vec![(type_, "value".to_string())], type_)
            }
            "length" => {
                let element_type = self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Unknown,
                    definition_range: None,
                });
                let in_type = self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::ArrayOf {
                        subtype: element_type,
                        size: None,
                    },
                    definition_range: None,
                });
                let out_type = self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Number {
                        signed: None,
                        integer: Some(true),
                    },
                    definition_range: None,
                });
                (vec![(in_type, "value".to_string())], out_type)
            }
            "comptime" => {
                let type_ = self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Unknown,
                    definition_range: None,
                });
                (vec![(type_, "value".to_string())], type_)
            }
            _ => {
                self.errors.push(ErrorKind::IntrinsicUnknown {
                    intrinsic: UnresolvedSymbol::from_token(name.clone(), name_token),
                });

                // still typecheck args, even though the function doesn't exist
                for (arg, _comma) in &mut expression.arguments {
                    self.visit_expression(arg);
                }

                return self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Error,
                    definition_range: None,
                });
            }
        };

        let mut extra_parameters = Vec::new();
        let mut missing_parameters = Vec::new();

        for (i, types) in expression
            .arguments
            .iter_mut()
            .map(|(arg, _comma)| (self.visit_expression(arg), arg))
            .collect_vec()
            .into_iter()
            .zip_longest(parameter_types.iter())
            .enumerate()
        {
            match types {
                EitherOrBoth::Both((arg_type_ptr, arg), (param_type_ptr, param_name)) => {
                    if !RuntimeTypeKind::merge_types(
                        arg_type_ptr,
                        *param_type_ptr,
                        &mut self.type_sets,
                    ) {
                        self.errors.push(ErrorKind::TypeMismatch {
                            kind: TypeMismatchKind::IntrinsicCallParameter {
                                parameter_index: i,
                                parameter_name: param_name.clone(),
                                intrinsic: UnresolvedSymbol::from_token(name.clone(), name_token),
                            },
                            value_range: find_node_bounds(&*arg),
                            expected_types: vec![PrefetchedType::fetch(
                                *param_type_ptr,
                                &self.type_sets,
                            )],
                            actual_type: PrefetchedType::fetch(arg_type_ptr, &self.type_sets),
                        });
                    }
                }
                EitherOrBoth::Left((arg_type_ptr, arg)) => {
                    extra_parameters.push(ExtraParameter {
                        range: find_node_bounds(&*arg),
                        type_: PrefetchedType::fetch(arg_type_ptr, &self.type_sets),
                    });
                }
                EitherOrBoth::Right((param_type_ptr, param_name)) => {
                    missing_parameters.push(MissingParameter {
                        name: param_name.clone(),
                        name_range: None,
                        type_: PrefetchedType::fetch(*param_type_ptr, &self.type_sets),
                    });
                }
            }
        }

        match (extra_parameters.is_empty(), missing_parameters.is_empty()) {
            (true, true) => {}
            (true, false) => {
                self.errors
                    .push(ErrorKind::IntrinsicCallWrongParameterCount {
                        intrinsic: UnresolvedSymbol::from_token(name.clone(), name_token),
                        difference: ParameterCountDifference::TooFewGiven(missing_parameters),
                        defined_parameter_count: parameter_types.len(),
                    });
            }
            (false, true) => {
                self.errors
                    .push(ErrorKind::IntrinsicCallWrongParameterCount {
                        intrinsic: UnresolvedSymbol::from_token(name.clone(), name_token),
                        difference: ParameterCountDifference::TooManyGiven(extra_parameters),
                        defined_parameter_count: parameter_types.len(),
                    });
            }
            (false, false) => {
                unreachable!()
            }
        }

        self.node_types.insert(*id, return_type);

        return_type
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
        let array_type = self.visit_expression(array);
        let index_type = self.visit_expression(index);

        let RuntimeTypeKind::ArrayOf { subtype, size: _ } = self.type_sets.get(array_type).kind
        else {
            self.errors.push(ErrorKind::ArrayIndexNonArray {
                value: find_node_bounds(&**array),
                wrong_type: PrefetchedType::fetch(array_type, &self.type_sets),
            });
            let err_type = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            });
            self.node_types.insert(*id, err_type);
            return err_type;
        };

        let expected_index_type = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Number {
                signed: Some(false),
                integer: Some(true),
            },
            definition_range: None,
        });

        if !RuntimeTypeKind::merge_types(index_type, expected_index_type, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::ArrayIndex {
                    array: find_node_bounds(&**array),
                },
                value_range: find_node_bounds(&**index),
                expected_types: vec![PrefetchedType::fetch(expected_index_type, &self.type_sets)],
                actual_type: PrefetchedType::fetch(index_type, &self.type_sets),
            });
        }

        self.node_types.insert(*id, subtype);
        subtype
    }

    fn visit_struct_access_expression(
        &mut self,
        StructAccessExpression {
            value,
            dot_token: _,
            member_name,
            member_name_token,
            id,
        }: &mut StructAccessExpression,
    ) -> Self::ExpressionOutput {
        let value_type = self.visit_expression(value);

        let RuntimeTypeKind::Struct {
            members,
            source_hash: _,
        } = &self.type_sets.get(value_type).kind
        else {
            if self.type_sets.get(value_type).kind != RuntimeTypeKind::Error {
                self.errors.push(ErrorKind::StructAccessNonStruct {
                    value: find_node_bounds(&**value),
                    wrong_type: PrefetchedType::fetch(value_type, &self.type_sets),
                    member: UnresolvedSymbol::from_token(member_name.clone(), member_name_token),
                });
            }
            let err_type = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            });
            self.node_types.insert(*id, err_type);
            return err_type;
        };

        let type_ = if let Some((_name, _range, member_type)) =
            members.iter().find(|m| m.0 == *member_name)
        {
            *member_type
        } else {
            self.errors.push(ErrorKind::NonexistentStructMember {
                struct_type: PrefetchedType::fetch(value_type, &self.type_sets),
                value: find_node_bounds(&**value),
                member: UnresolvedSymbol::from_token(member_name.clone(), member_name_token),
            });

            self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            })
        };

        self.node_types.insert(*id, type_);
        type_
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
        let type_ = self.visit_expression(subexpression);
        self.node_types.insert(*id, type_);
        type_
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let VariableAccessExpression {
            name: _,
            name_token,
            id,
        } = expression;
        let Some(ref_variable) = self.referenced_variable.get(id) else {
            return self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Unknown,
                definition_range: None,
            });
        };
        if self.require_constant && !ref_variable.borrow().is_constant {
            self.errors.push(ErrorKind::ConstantVariableRequired {
                variable: ref_variable
                    .borrow()
                    .symbol
                    .clone()
                    .with_use(name_token.range.clone()),
            });
        }

        let type_ = ref_variable.borrow().type_.unwrap();
        self.node_types.insert(*id, type_);
        type_
    }
    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let UnaryExpression {
            operator_token: _,
            operation,
            operand,
            id,
        } = expression;

        let type_ = self.visit_expression(operand);

        let allowed_types = match operation {
            UnaryOperation::BitwiseNot => vec![self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Number {
                    signed: None,
                    integer: Some(true),
                },
                definition_range: None,
            })],
            UnaryOperation::LogicalNot => vec![
                self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Number {
                        signed: None,
                        integer: None,
                    },
                    definition_range: None,
                }),
                self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Boolean,
                    definition_range: None,
                }),
            ],
            UnaryOperation::Negate => vec![self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Number {
                    signed: Some(true),
                    integer: None,
                },
                definition_range: None,
            })],
        };

        let is_ok = allowed_types.iter().any(|expected_type| {
            RuntimeTypeKind::merge_types(type_, *expected_type, &mut self.type_sets)
        });

        if !is_ok {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::UnaryOperation(*operation),
                value_range: find_node_bounds(&**operand),
                expected_types: allowed_types
                    .iter()
                    .map(|ptr| PrefetchedType::fetch(*ptr, &self.type_sets))
                    .collect(),
                actual_type: PrefetchedType::fetch(type_, &self.type_sets),
            });
        }
        let this_type = match operation {
            UnaryOperation::BitwiseNot => type_,
            UnaryOperation::LogicalNot => self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Boolean,
                definition_range: None,
            }),
            UnaryOperation::Negate => type_,
        };

        self.node_types.insert(*id, this_type);
        this_type
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let CastExpression {
            operand,
            as_token: _,
            type_,
            id,
        } = expression;

        let from_ptr = self.visit_expression(operand);
        let to_ptr = self.visit_type(type_);

        use RuntimeTypeKind as T;
        enum CastResult {
            Possible,
            Redundant,
            Impossible,
        }
        fn perform_cast(
            expression_clone: CastExpression,
            from_ptr: UnionFindSetPtr<RuntimeType>,
            to_ptr: UnionFindSetPtr<RuntimeType>,
            self_errors: &mut Vec<ErrorKind>,
            types: &mut TypeSets,
        ) -> CastResult {
            let from_clone = types.get(from_ptr).kind.clone();
            let to_clone = types.get(to_ptr).kind.clone();
            match (&from_clone, &to_clone) {
                (_, T::Error) | (T::Error, _) => CastResult::Possible,

                (T::I8, T::I8)
                | (T::I16, T::I16)
                | (T::I32, T::I32)
                | (T::I64, T::I64)
                | (T::U8, T::U8)
                | (T::U16, T::U16)
                | (T::U32, T::U32)
                | (T::U64, T::U64)
                | (T::F32, T::F32)
                | (T::F64, T::F64)
                | (T::Boolean, T::Boolean)
                | (T::Unit, T::Unit) => {
                    self_errors.push(ErrorKind::Lint(Lint::SelfCast {
                        expression: find_node_bounds(&expression_clone),
                        type_: PrefetchedType::fetch(from_ptr, types),
                    }));
                    CastResult::Redundant
                }
                (
                    T::Number { .. }
                    | T::I8
                    | T::I16
                    | T::I32
                    | T::I64
                    | T::U8
                    | T::U16
                    | T::U32
                    | T::U64
                    | T::F32
                    | T::F64,
                    T::Number { .. }
                    | T::I8
                    | T::I16
                    | T::I32
                    | T::I64
                    | T::U8
                    | T::U16
                    | T::U32
                    | T::U64
                    | T::F32
                    | T::F64,
                ) => {
                    let _ = RuntimeTypeKind::merge_types(from_ptr, to_ptr, types);
                    CastResult::Possible
                }

                (
                    T::Number { .. }
                    | T::I8
                    | T::I16
                    | T::I32
                    | T::I64
                    | T::U8
                    | T::U16
                    | T::U32
                    | T::U64
                    | T::F32
                    | T::F64,
                    T::Boolean,
                ) => CastResult::Possible,
                (
                    T::Boolean,
                    T::Number { .. }
                    | T::I8
                    | T::I16
                    | T::I32
                    | T::I64
                    | T::U8
                    | T::U16
                    | T::U32
                    | T::U64
                    | T::F32
                    | T::F64,
                ) => CastResult::Possible,

                (_, T::Unit) => {
                    self_errors.push(ErrorKind::ImpossibleCast {
                        reason: ImpossibleCastReason::InvolvesUnit {
                            direction: CastDirection::To,
                        },
                        expression: find_node_bounds(&expression_clone),
                        from: PrefetchedType::fetch(from_ptr, types),
                        to: PrefetchedType::fetch(to_ptr, types),
                    });
                    CastResult::Impossible
                }
                (T::Unit, _) => {
                    self_errors.push(ErrorKind::ImpossibleCast {
                        reason: ImpossibleCastReason::InvolvesUnit {
                            direction: CastDirection::From,
                        },
                        expression: find_node_bounds(&expression_clone),
                        from: PrefetchedType::fetch(from_ptr, types),
                        to: PrefetchedType::fetch(to_ptr, types),
                    });
                    CastResult::Impossible
                }
                (
                    T::ArrayOf {
                        subtype: _,
                        size: a_size,
                    },
                    T::ArrayOf {
                        subtype: _,
                        size: b_size,
                    },
                ) if a_size == b_size || a_size.is_none() || b_size.is_none() => {
                    let _ = RuntimeTypeKind::merge_types(from_ptr, to_ptr, types);
                    let res = perform_cast(
                        expression_clone.clone(),
                        from_ptr,
                        to_ptr,
                        self_errors,
                        types,
                    );
                    match res {
                        CastResult::Possible => CastResult::Possible,
                        CastResult::Redundant => {
                            self_errors.push(ErrorKind::Lint(Lint::SelfCast {
                                expression: find_node_bounds(&expression_clone),
                                type_: PrefetchedType::fetch(from_ptr, types),
                            }));
                            CastResult::Redundant
                        }
                        CastResult::Impossible => {
                            self_errors.push(ErrorKind::ImpossibleCast {
                                reason: ImpossibleCastReason::ArrayElementsIncompatible,
                                expression: find_node_bounds(&expression_clone),
                                from: PrefetchedType::fetch(from_ptr, types),
                                to: PrefetchedType::fetch(to_ptr, types),
                            });
                            CastResult::Impossible
                        }
                    }
                }
                (
                    T::ArrayOf {
                        size: Some(from_size),
                        ..
                    },
                    T::ArrayOf {
                        size: Some(to_size),
                        ..
                    },
                ) => {
                    // equal sizes are already handled above
                    self_errors.push(ErrorKind::ImpossibleCast {
                        reason: ImpossibleCastReason::ArrayLengthIncompatible {
                            from_size: *from_size,
                            to_size: *to_size,
                        },
                        expression: find_node_bounds(&expression_clone),
                        from: PrefetchedType::fetch(from_ptr, types),
                        to: PrefetchedType::fetch(to_ptr, types),
                    });
                    CastResult::Impossible
                }
                (
                    _,
                    T::ArrayOf {
                        subtype: _,
                        size: _,
                    },
                ) => {
                    self_errors.push(ErrorKind::ImpossibleCast {
                        reason: ImpossibleCastReason::ArrayAndNonArray {
                            direction: CastDirection::To,
                        },
                        expression: find_node_bounds(&expression_clone),
                        from: PrefetchedType::fetch(from_ptr, types),
                        to: PrefetchedType::fetch(to_ptr, types),
                    });
                    CastResult::Impossible
                }
                (
                    T::ArrayOf {
                        subtype: _,
                        size: _,
                    },
                    _,
                ) => {
                    self_errors.push(ErrorKind::ImpossibleCast {
                        reason: ImpossibleCastReason::ArrayAndNonArray {
                            direction: CastDirection::From,
                        },
                        expression: find_node_bounds(&expression_clone),
                        from: PrefetchedType::fetch(from_ptr, types),
                        to: PrefetchedType::fetch(to_ptr, types),
                    });
                    CastResult::Impossible
                }
                (
                    a @ T::Struct {
                        members: a_members,
                        source_hash: a_hash,
                    },
                    b @ T::Struct {
                        members: b_members,
                        source_hash: b_hash,
                    },
                ) => {
                    let members_equal = a_members == b_members;
                    let hash_equal = a_hash == b_hash;

                    match (hash_equal, members_equal) {
                        (true, true) => {
                            self_errors.push(ErrorKind::Lint(Lint::SelfCast {
                                expression: find_node_bounds(&expression_clone),
                                type_: PrefetchedType::fetch(from_ptr, types),
                            }));
                            CastResult::Redundant
                        }
                        (true, false) => {
                            unreachable!(
                                "Two struct types have different members but same source hash:\na: {a:#?}\nb: {b:#?}"
                            )
                        }
                        (false, true) => {
                            self_errors.push(ErrorKind::ImpossibleCast {
                                reason: ImpossibleCastReason::DifferentStructOrigin,
                                expression: find_node_bounds(&expression_clone),
                                from: PrefetchedType::fetch(from_ptr, types),
                                to: PrefetchedType::fetch(to_ptr, types),
                            });

                            CastResult::Impossible
                        }
                        (false, false) => {
                            self_errors.push(ErrorKind::ImpossibleCast {
                                reason: ImpossibleCastReason::StructMembersDiffer,
                                expression: find_node_bounds(&expression_clone),
                                from: PrefetchedType::fetch(from_ptr, types),
                                to: PrefetchedType::fetch(to_ptr, types),
                            });
                            CastResult::Impossible
                        }
                    }
                }
                (
                    T::Struct {
                        members: _,
                        source_hash: _,
                    },
                    _,
                ) => {
                    self_errors.push(ErrorKind::ImpossibleCast {
                        reason: ImpossibleCastReason::StructAndNonStruct {
                            direction: CastDirection::From,
                        },
                        expression: find_node_bounds(&expression_clone),
                        from: PrefetchedType::fetch(from_ptr, types),
                        to: PrefetchedType::fetch(to_ptr, types),
                    });

                    CastResult::Impossible
                }
                (
                    _,
                    T::Struct {
                        members: _,
                        source_hash: _,
                    },
                ) => {
                    self_errors.push(ErrorKind::ImpossibleCast {
                        reason: ImpossibleCastReason::StructAndNonStruct {
                            direction: CastDirection::To,
                        },
                        expression: find_node_bounds(&expression_clone),
                        from: PrefetchedType::fetch(from_ptr, types),
                        to: PrefetchedType::fetch(to_ptr, types),
                    });

                    CastResult::Impossible
                }
                (T::Unknown, T::Unknown) => {
                    assert!(RuntimeTypeKind::merge_types(from_ptr, to_ptr, types));
                    self_errors.push(ErrorKind::Lint(Lint::SelfCast {
                        expression: find_node_bounds(&expression_clone),
                        type_: PrefetchedType::fetch(from_ptr, types),
                    }));
                    CastResult::Possible
                }
                (_, T::Unknown) | (T::Unknown, _) => {
                    assert!(RuntimeTypeKind::merge_types(from_ptr, to_ptr, types));
                    CastResult::Possible
                }
            }
        }
        perform_cast(
            expression_clone,
            from_ptr,
            to_ptr,
            &mut self.errors,
            &mut self.type_sets,
        );

        self.node_types.insert(*id, to_ptr);
        to_ptr
    }

    fn visit_binary_expression(
        &mut self,
        expression: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let BinaryExpression {
            left,
            operator_token: _,
            operation,
            right,
            id,
        } = expression;
        let left_type = self.visit_expression(left);
        let right_type = self.visit_expression(right);

        // only works as long as this is symmetric
        let mut allowed_gen = || match operation {
            BinaryOperation::Add
            | BinaryOperation::Subtract
            | BinaryOperation::Multiply
            | BinaryOperation::Divide
            | BinaryOperation::Modulo
            | BinaryOperation::GreaterThan
            | BinaryOperation::GreaterThanOrEqual
            | BinaryOperation::LessThan
            | BinaryOperation::LessThanOrEqual => vec![self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Number {
                    signed: None,
                    integer: None,
                },
                definition_range: None,
            })],
            BinaryOperation::Equal | BinaryOperation::NotEqual => {
                vec![
                    self.type_sets.insert_set(RuntimeType {
                        kind: RuntimeTypeKind::Number {
                            signed: None,
                            integer: None,
                        },
                        definition_range: None,
                    }),
                    self.type_sets.insert_set(RuntimeType {
                        kind: RuntimeTypeKind::Boolean,
                        definition_range: None,
                    }),
                ]
            }

            BinaryOperation::LogicalAnd | BinaryOperation::LogicalOr => {
                vec![self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Boolean,
                    definition_range: None,
                })]
            }
        };

        let allowed_types_left = allowed_gen();
        let allowed_types_right = allowed_gen();

        let is_left_ok = allowed_types_left.iter().any(|expected_type| {
            RuntimeTypeKind::merge_types(left_type, *expected_type, &mut self.type_sets)
        });
        let is_right_ok = allowed_types_right.iter().any(|expected_type| {
            RuntimeTypeKind::merge_types(right_type, *expected_type, &mut self.type_sets)
        });

        if !RuntimeTypeKind::merge_types(left_type, right_type, &mut self.type_sets)
            || !is_left_ok
            || !is_right_ok
        {
            if !is_left_ok {
                self.errors.push(ErrorKind::TypeMismatch {
                    kind: TypeMismatchKind::BinaryOperationLeft(*operation),
                    value_range: find_node_bounds(&**left),
                    expected_types: allowed_types_left
                        .iter()
                        .map(|ptr| PrefetchedType::fetch(*ptr, &self.type_sets))
                        .collect(),
                    actual_type: PrefetchedType::fetch(left_type, &self.type_sets),
                });
            }
            if !is_right_ok {
                self.errors.push(ErrorKind::TypeMismatch {
                    kind: TypeMismatchKind::BinaryOperationRight(*operation),
                    value_range: find_node_bounds(&**right),
                    expected_types: allowed_types_right
                        .iter()
                        .map(|ptr| PrefetchedType::fetch(*ptr, &self.type_sets))
                        .collect(),
                    actual_type: PrefetchedType::fetch(right_type, &self.type_sets),
                });
            }
            if is_left_ok && is_right_ok && left_type != right_type {
                self.errors.push(ErrorKind::TypeMismatch {
                    kind: TypeMismatchKind::BinaryOperation {
                        operation: *operation,
                        right_expected_types: allowed_types_right
                            .iter()
                            .map(|ptr| PrefetchedType::fetch(*ptr, &self.type_sets))
                            .collect(),
                        right_actual_type: PrefetchedType::fetch(right_type, &self.type_sets),
                    },
                    value_range: find_node_bounds(&expression_clone),
                    expected_types: allowed_types_left
                        .iter()
                        .map(|ptr| PrefetchedType::fetch(*ptr, &self.type_sets))
                        .collect(),
                    actual_type: PrefetchedType::fetch(left_type, &self.type_sets),
                });
            }

            let this_type = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            });
            self.node_types.insert(*id, this_type);
            this_type
        } else {
            let this_type = match operation {
                BinaryOperation::Add
                | BinaryOperation::Subtract
                | BinaryOperation::Multiply
                | BinaryOperation::Divide
                | BinaryOperation::Modulo => left_type,
                BinaryOperation::GreaterThan
                | BinaryOperation::GreaterThanOrEqual
                | BinaryOperation::LessThan
                | BinaryOperation::LessThanOrEqual
                | BinaryOperation::Equal
                | BinaryOperation::NotEqual
                | BinaryOperation::LogicalAnd
                | BinaryOperation::LogicalOr => self.type_sets.insert_set(RuntimeType {
                    kind: RuntimeTypeKind::Boolean,
                    definition_range: None,
                }),
            };
            self.node_types.insert(*id, this_type);
            this_type
        }
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
        let left_type = self.visit_lvalue(lvalue);
        let right_type = self.visit_expression(right);

        if !RuntimeTypeKind::merge_types(left_type, right_type, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::LValueAssignment {
                    lvalue: find_node_bounds(&*lvalue),
                },
                value_range: find_node_bounds(&**right),
                expected_types: vec![PrefetchedType::fetch(left_type, &self.type_sets)],
                actual_type: PrefetchedType::fetch(right_type, &self.type_sets),
            });
        }

        self.node_types.insert(*id, left_type);
        left_type
    }

    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        let VariableLValue {
            name: _,
            name_token,
            id,
        } = lvalue;
        let Some(ref_variable) = self.referenced_variable.get(id) else {
            return self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Unknown,
                definition_range: None,
            });
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

        let type_ = ref_variable.borrow().type_.unwrap();
        self.node_types.insert(*id, type_);
        type_
    }

    fn visit_array_index_lvalue(&mut self, lvalue: &mut ArrayIndexLValue) -> Self::LValueOutput {
        let ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        } = lvalue;

        let array_type = self.visit_lvalue(array);
        let index_type = self.visit_expression(index);

        let RuntimeTypeKind::ArrayOf { subtype, size: _ } = self.type_sets.get(array_type).kind
        else {
            self.errors.push(ErrorKind::ArrayIndexNonArray {
                value: find_node_bounds(&**array),
                wrong_type: PrefetchedType::fetch(array_type, &self.type_sets),
            });
            let err_type = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            });
            self.node_types.insert(*id, err_type);
            return err_type;
        };

        let expected_type = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Number {
                signed: Some(false),
                integer: Some(true),
            },
            definition_range: None,
        });

        if !RuntimeTypeKind::merge_types(index_type, expected_type, &mut self.type_sets) {
            self.errors.push(ErrorKind::TypeMismatch {
                kind: TypeMismatchKind::ArrayIndex {
                    array: find_node_bounds(&**array),
                },
                value_range: find_node_bounds(&**index),
                expected_types: vec![PrefetchedType::fetch(expected_type, &self.type_sets)],
                actual_type: PrefetchedType::fetch(index_type, &self.type_sets),
            });
        }

        self.node_types.insert(*id, subtype);
        subtype
    }

    fn visit_struct_access_lvalue(
        &mut self,
        StructAccessLValue {
            value,
            dot_token: _,
            member_name,
            member_name_token,
            id,
        }: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        let value_type = self.visit_lvalue(value);

        let RuntimeTypeKind::Struct {
            members,
            source_hash: _,
        } = &self.type_sets.get(value_type).kind
        else {
            if self.type_sets.get(value_type).kind != RuntimeTypeKind::Error {
                self.errors.push(ErrorKind::StructAccessNonStruct {
                    value: find_node_bounds(&**value),
                    wrong_type: PrefetchedType::fetch(value_type, &self.type_sets),
                    member: UnresolvedSymbol::from_token(member_name.clone(), member_name_token),
                });
            }
            let err_type = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            });
            self.node_types.insert(*id, err_type);
            return err_type;
        };

        let type_ = if let Some((_name, _range, member_type)) =
            members.iter().find(|m| m.0 == *member_name)
        {
            *member_type
        } else {
            self.errors.push(ErrorKind::NonexistentStructMember {
                struct_type: PrefetchedType::fetch(value_type, &self.type_sets),
                value: find_node_bounds(&**value),
                member: UnresolvedSymbol::from_token(member_name.clone(), member_name_token),
            });

            self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: None,
            })
        };

        self.node_types.insert(*id, type_);
        type_
    }

    fn visit_grouping_lvalue(&mut self, lvalue: &mut GroupingLValue) -> Self::LValueOutput {
        let _lvalue_clone = lvalue.clone();
        let GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id,
        } = lvalue;

        let subtype = self.visit_lvalue(sublvalue);

        self.node_types.insert(*id, subtype);
        subtype
    }

    fn visit_simple_type(
        &mut self,
        SimpleType {
            token: _,
            type_,
            id,
        }: &mut SimpleType,
    ) -> Self::TypeOutput {
        // make sure function registration gets the same type as the full eval
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let t = self.type_sets.insert_set(type_.clone());
        self.node_types.insert(*id, t);
        t
    }

    fn visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token,
            close_paren_token,
            id,
        }: &mut UnitType,
    ) -> Self::TypeOutput {
        // make sure function registration gets the same type as the full eval
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let t = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Unit,
            definition_range: Some(
                open_paren_token
                    .range
                    .clone()
                    .extend_with(close_paren_token.range.clone()),
            ),
        });
        self.node_types.insert(*id, t);
        t
    }

    fn visit_idk_type(&mut self, IdkType { token, id }: &mut IdkType) -> Self::TypeOutput {
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let t = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Unknown,
            definition_range: Some(token.range.clone()),
        });
        self.node_types.insert(*id, t);
        t
    }

    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput {
        let array_type_range = find_node_bounds(&*array_type);
        let ArrayType {
            subtype,
            open_bracket_token,
            size,
            close_bracket_token,
            id,
        } = array_type;
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let subtype_t = self.visit_type(subtype);

        if matches!(self.type_sets.get(subtype_t).kind, RuntimeTypeKind::Unit) {
            self.errors.push(ErrorKind::ArrayOfUnit {
                type_range: array_type_range.clone(),
                element_type: PrefetchedType::fetch(subtype_t, &self.type_sets),
            });
        }

        let size = match size.as_ref().map(|boxed_x| &**boxed_x) {
            Some(Expression::Literal(LiteralExpression {
                value: LiteralKind::Number(value),
                token: _,
                id: _,
            })) => Some(*value as usize),
            Some(expr) => {
                // TODO: once we have consteval, relax this restriction
                self.errors.push(ErrorKind::NonLiteralArrayLength {
                    type_range: array_type_range.clone(),
                    element_type: PrefetchedType::fetch(subtype_t, &self.type_sets),
                    length_range: find_node_bounds(expr),
                });
                None
            }
            None => None,
        };

        let t = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::ArrayOf {
                subtype: subtype_t,
                size,
            },
            definition_range: Some(
                open_bracket_token
                    .range
                    .clone()
                    .extend_with(close_bracket_token.range.clone()),
            ),
        });
        self.node_types.insert(*id, t);
        t
    }

    fn visit_struct_type(
        &mut self,
        StructType {
            struct_token,
            open_brace_token: _,
            members,
            close_brace_token,
            id,
        }: &mut StructType,
    ) -> Self::TypeOutput {
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let mut rt_members = vec![];
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
            let subtype = self.visit_type(type_);

            if self.type_sets.get(subtype).kind == RuntimeTypeKind::Unit {
                self.errors.push(ErrorKind::StructOfUnit {
                    member: SymbolDefinition::from_token(name.clone(), name_token),
                    type_: PrefetchedType::fetch(subtype, &self.type_sets),
                    type_range: find_node_bounds(&*type_),
                });
            }

            rt_members.push((name.clone(), name_token.range.clone(), subtype));
        }

        let mut hash = DefaultHasher::new();
        struct_token.range.hash(&mut hash);

        let struct_t = self.type_sets.insert_set(RuntimeType {
            kind: RuntimeTypeKind::Struct {
                members: rt_members,
                source_hash: Some(hash.finish()),
            },
            definition_range: Some(
                struct_token
                    .range
                    .clone()
                    .extend_with(close_brace_token.range.clone()),
            ),
        });
        self.node_types.insert(*id, struct_t);
        struct_t
    }

    fn visit_alias_type(&mut self, alias_type: &mut AliasType) -> Self::TypeOutput {
        let AliasType {
            name,
            name_token,
            id,
        } = alias_type;

        let Some((_definition_range, aliased_type)) = self.type_aliases.get(name) else {
            self.errors.push(ErrorKind::NotDefined {
                kind: NotDefinedKind::TypeAlias,
                item: UnresolvedSymbol::from_token(name.clone(), name_token),
            });

            let type_ = self.type_sets.insert_set(RuntimeType {
                kind: RuntimeTypeKind::Error,
                definition_range: Some(name_token.range.clone()),
            });
            self.node_types.insert(*id, type_);
            return type_;
        };

        self.node_types.insert(*id, *aliased_type);
        *aliased_type
    }
}
