use std::{
    cell::{RefCell, RefMut},
    collections::HashMap,
    error::Error,
    hash::{Hash, Hasher},
    marker::PhantomData,
    rc::Rc,
};

use itertools::{EitherOrBoth, Itertools};

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNode, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, BoolExpression, BoolType,
        BreakStatement, CastExpression, Expression, ExpressionStatement, ExternFunctionBody,
        ForLoopStatement, FunctionCallExpression, FunctionDefinition, GPUExecutor,
        GroupingExpression, GroupingLValue, HasID, IdkType, IfStatement, IntType, NumberExpression,
        OnStatement, PerNodeData, Program, ReturnStatement, SelfExecutorHost, SimpleBinding,
        SkipStatement, StatementFunctionBody, ThreadExecutor, UnaryExpression, UnaryOperation,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    parser::IdGenerator,
};

type AnyResult<T> = ::core::result::Result<T, Box<dyn Error>>;

#[derive(Debug, Default)]
pub struct UnionFindSetPtr<T>(u64, PhantomData<T>);

impl<T> PartialEq for UnionFindSetPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for UnionFindSetPtr<T> {}

impl<T> Hash for UnionFindSetPtr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> Copy for UnionFindSetPtr<T> {}

impl<T> Clone for UnionFindSetPtr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone, Debug)]
pub struct UnionFindSet<T> {
    parent: HashMap<UnionFindSetPtr<T>, UnionFindSetPtr<T>>,
    pub data: HashMap<UnionFindSetPtr<T>, T>,
    highest_ptr: UnionFindSetPtr<T>,
}

impl<T> Default for UnionFindSet<T> {
    fn default() -> Self {
        Self {
            parent: HashMap::default(),
            data: HashMap::default(),
            highest_ptr: UnionFindSetPtr(0, PhantomData),
        }
    }
}

pub enum UnionFindSetMergeResult<T> {
    Merged(T),
    NotMerged { a: T, b: T },
}

impl<T> UnionFindSet<T> {
    pub fn insert_set(&mut self, initial_data: T) -> UnionFindSetPtr<T> {
        let current_ptr = self.highest_ptr;
        self.highest_ptr.0 += 1;
        self.data.insert(current_ptr, initial_data);
        self.parent.insert(current_ptr, current_ptr);
        current_ptr
    }
    /// Creates a new, detached, set with the representative being a copy of the one for [`ptr`]
    pub fn detach(&mut self, ptr: UnionFindSetPtr<T>) -> UnionFindSetPtr<T>
    where
        T: Clone,
    {
        self.insert_set(self.get(ptr).clone())
    }
    pub fn get_repr(&self, mut ptr: UnionFindSetPtr<T>) -> UnionFindSetPtr<T> {
        while self.parent.get(&ptr) != Some(&ptr) {
            ptr = *self.parent.get(&ptr).unwrap();
        }
        ptr
    }
    pub fn get(&self, ptr: UnionFindSetPtr<T>) -> &T {
        self.data.get(&self.get_repr(ptr)).unwrap()
    }
    pub fn get_mut(&mut self, ptr: UnionFindSetPtr<T>) -> &mut T {
        self.data.get_mut(&self.get_repr(ptr)).unwrap()
    }
    pub fn try_merge<Combiner: FnOnce(T, T, &mut Self) -> UnionFindSetMergeResult<T>>(
        &mut self,
        a: UnionFindSetPtr<T>,
        b: UnionFindSetPtr<T>,
        combiner: Combiner,
    ) -> bool {
        let a_repr = self.get_repr(a);
        let b_repr = self.get_repr(b);
        if a_repr == b_repr {
            return true;
        }

        let a_data = self.data.remove(&a_repr).unwrap();
        let b_data = self.data.remove(&b_repr).unwrap();
        match combiner(a_data, b_data, self) {
            UnionFindSetMergeResult::Merged(combined_data) => {
                *self.parent.get_mut(&b_repr).unwrap() = a_repr;
                self.data.insert(a_repr, combined_data);
                true
            }
            UnionFindSetMergeResult::NotMerged {
                a: a_data,
                b: b_data,
            } => {
                self.data.insert(a_repr, a_data);
                self.data.insert(b_repr, b_data);
                false
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RuntimeType {
    I8,
    I16,
    I32,
    I64,
    UnsizedInt,
    Boolean,
    Unit,
    Unknown,
    Error,
    ArrayOf {
        subtype: UnionFindSetPtr<RuntimeType>,
        size: Option<usize>,
    },
}

impl RuntimeType {
    pub fn stringify(&self, types: &UnionFindSet<RuntimeType>) -> String {
        match *self {
            RuntimeType::I8 => "i8".to_string(),
            RuntimeType::I16 => "i16".to_string(),
            RuntimeType::I32 => "i32".to_string(),
            RuntimeType::I64 => "i64".to_string(),
            RuntimeType::UnsizedInt => "{unsized int}".to_string(),
            RuntimeType::Unit => "()".to_string(),
            RuntimeType::Boolean => "bool".to_string(),
            RuntimeType::Unknown => "{unknown}".to_string(),
            RuntimeType::Error => "{error}".to_string(),
            RuntimeType::ArrayOf {
                subtype,
                size: None,
            } => format!("{}[]", types.get(subtype).stringify(types)),
            RuntimeType::ArrayOf {
                subtype,
                size: Some(size),
            } => format!("{}[{}]", types.get(subtype).stringify(types), size),
        }
    }
    pub fn is_numeric(&self) -> bool {
        match self {
            RuntimeType::I8 => true,
            RuntimeType::I16 => true,
            RuntimeType::I32 => true,
            RuntimeType::I64 => true,
            RuntimeType::UnsizedInt => true,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
        }
    }
    pub fn is_boolean(&self) -> bool {
        match self {
            RuntimeType::I8 => false,
            RuntimeType::I16 => false,
            RuntimeType::I32 => false,
            RuntimeType::I64 => false,
            RuntimeType::UnsizedInt => false,
            RuntimeType::Boolean => true,
            RuntimeType::Unit => false,
            RuntimeType::Unknown => false,
            RuntimeType::Error => false,
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => false,
        }
    }

    /// true means the specialization succeeded
    pub fn specialize_int_size(&mut self) -> bool {
        if *self == RuntimeType::UnsizedInt {
            *self = RuntimeType::I32;
            true
        } else {
            false
        }
    }

    #[must_use = "Failed merges usually indicate a compile error"]
    pub fn merge_types(
        a: UnionFindSetPtr<RuntimeType>,
        b: UnionFindSetPtr<RuntimeType>,
        types: &mut UnionFindSet<RuntimeType>,
    ) -> bool {
        types.try_merge(a, b, |mut a, b, types| {
            use UnionFindSetMergeResult::*;
            if b == RuntimeType::Unknown {
                return Merged(a);
            }
            match a {
                RuntimeType::I8 => {
                    if b == RuntimeType::I8 {
                        Merged(b)
                    } else if b == RuntimeType::UnsizedInt {
                        return Merged(a);
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::I16 => {
                    if b == RuntimeType::I16 {
                        Merged(b)
                    } else if b == RuntimeType::UnsizedInt {
                        return Merged(a);
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::I32 => {
                    if b == RuntimeType::I32 {
                        Merged(b)
                    } else if b == RuntimeType::UnsizedInt {
                        return Merged(a);
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::I64 => {
                    if b == RuntimeType::I64 {
                        Merged(b)
                    } else if b == RuntimeType::UnsizedInt {
                        return Merged(a);
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::UnsizedInt => {
                    if b.is_numeric() {
                        Merged(b)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::Boolean => {
                    if b.is_boolean() {
                        Merged(b)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::Unit => {
                    if b == RuntimeType::Unit {
                        Merged(b)
                    } else {
                        NotMerged { a, b }
                    }
                }
                RuntimeType::Unknown => Merged(b),
                RuntimeType::Error => Merged(a),
                RuntimeType::ArrayOf {
                    subtype: a_subtype,
                    size: ref mut a_size,
                } => {
                    if let RuntimeType::ArrayOf {
                        subtype: b_subtype,
                        size: b_size,
                    } = b
                    {
                        if !RuntimeType::merge_types(a_subtype, b_subtype, types) {
                            return NotMerged { a, b };
                        }
                        match (*a_size, b_size) {
                            (None, None) => {}
                            (None, Some(_)) => {
                                *a_size = b_size;
                            }
                            (Some(_), None) => {}
                            (Some(a_size), Some(b_size)) => {
                                if a_size != b_size {
                                    return NotMerged { a, b };
                                }
                            }
                        }

                        Merged(a)
                    } else {
                        NotMerged { a, b }
                    }
                }
            }
        })
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct VariableID(pub u64);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionID(pub u64);

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub type_: UnionFindSetPtr<RuntimeType>,
    pub is_constant: bool,
    pub id: VariableID,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub return_type: UnionFindSetPtr<RuntimeType>,
    pub parameter_types: Vec<(UnionFindSetPtr<RuntimeType>, String)>,
    pub id: FunctionID,
}

#[derive(Clone, Default, Debug)]
pub struct VariableScope {
    pub variable_map: HashMap<String, Rc<RefCell<Variable>>>,
    pub copy_from_parent: bool,
    pub is_read_guard: bool,
    pub is_write_guard: bool,
    pub parent: Option<Rc<RefCell<VariableScope>>>,
}

#[derive(Default)]
pub struct VariableScopeStack {
    current: Rc<RefCell<VariableScope>>,
}

impl VariableScopeStack {
    pub fn get_read(&self, name: &str) -> Option<Rc<RefCell<Variable>>> {
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
                    .variable_map
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
                    .variable_map
                    .insert(name.to_string(), res.clone());
            }
        }

        res
    }

    pub fn try_insert(&mut self, var: Variable) -> AnyResult<()> {
        if self.current.borrow().variable_map.contains_key(&var.name) {
            Err("variable already exists".into())
        } else {
            self.current
                .borrow_mut()
                .variable_map
                .insert(var.name.clone(), Rc::new(RefCell::new(var)));
            Ok(())
        }
    }
    pub fn push_child(&mut self) {
        self.current = Rc::new(RefCell::new(VariableScope {
            parent: Some(self.current.clone()),
            ..Default::default()
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

    pub fn top_mut(&self) -> RefMut<VariableScope> {
        self.current.borrow_mut()
    }
}

pub struct TypePropagator<'a> {
    type_sets: UnionFindSet<RuntimeType>,
    node_types: PerNodeData<UnionFindSetPtr<RuntimeType>>,
    errors: &'a mut Vec<FleetError>,
    variable_scopes: VariableScopeStack,
    referenced_variable: PerNodeData<Rc<RefCell<Variable>>>,
    function_list: HashMap<String, Rc<RefCell<Function>>>,
    referenced_function: PerNodeData<Rc<RefCell<Function>>>,
    id_generator: &'a mut IdGenerator,
    current_function: Option<Rc<RefCell<Function>>>,
    contained_scope: PerNodeData<Rc<RefCell<VariableScope>>>,
}

impl<'a> TypePropagator<'a> {
    pub fn new(error_output: &'a mut Vec<FleetError>, id_generator: &'a mut IdGenerator) -> Self {
        Self {
            type_sets: UnionFindSet::default(),
            node_types: PerNodeData::default(),
            errors: error_output,
            variable_scopes: VariableScopeStack::default(),
            referenced_variable: PerNodeData::default(),
            function_list: HashMap::new(),
            referenced_function: PerNodeData::default(),
            id_generator,
            current_function: None,
            contained_scope: PerNodeData::default(),
        }
    }

    fn register_function(&mut self, function: &mut FunctionDefinition) {
        let function_clone = function.clone();
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
            body: _,
            id: _,
        } = function;

        self.variable_scopes.push_child(); // temporary parameter scope
        let return_type = return_type
            .as_mut()
            .map(|t| self.visit_type(t))
            .unwrap_or_else(|| self.type_sets.insert_set(RuntimeType::Unknown));
        let parameter_types = parameters
            .iter_mut()
            .map(|(param, _comma)| (self.visit_simple_binding(param), param.name.clone()))
            .collect();

        if self
            .function_list
            .insert(
                name.clone(),
                Rc::new(RefCell::new(Function {
                    name: name.clone(),
                    return_type,
                    parameter_types,
                    id: self.id_generator.next_function_id(),
                })),
            )
            .is_some()
        {
            self.errors.push(FleetError::from_node(
                &function_clone,
                format!("Multiple functions named {name:?} defined"),
                ErrorSeverity::Error,
            ));
        }

        self.variable_scopes.pop();
    }

    fn generate_mismatched_type_error_if<I: Into<AstNode> + Clone>(
        &mut self,
        condition: bool,
        node: &I,
        expression_type: impl AsRef<str>,
        expected_name: impl AsRef<str>,
        actual_type: RuntimeType,
    ) {
        if condition {
            self.errors.push(FleetError::from_node(
                node,
                format!(
                    "Expected {} to be {}. Got value of type {} instead",
                    expression_type.as_ref(),
                    expected_name.as_ref(),
                    actual_type.stringify(&self.type_sets)
                ),
                ErrorSeverity::Error,
            ));
        }
    }

    fn stringify_type_ptr(&self, ptr: UnionFindSetPtr<RuntimeType>) -> String {
        self.type_sets.get(ptr).stringify(&self.type_sets)
    }
}

#[derive(Clone, Debug)]
pub struct TypeAnalysisData {
    pub type_data: PerNodeData<UnionFindSetPtr<RuntimeType>>,
    pub type_sets: UnionFindSet<RuntimeType>,
    pub variable_data: PerNodeData<Rc<RefCell<Variable>>>,
    pub function_data: PerNodeData<Rc<RefCell<Function>>>,
    pub scope_data: PerNodeData<Rc<RefCell<VariableScope>>>,
}

impl AstVisitor for TypePropagator<'_> {
    type ProgramOutput = TypeAnalysisData;
    type FunctionDefinitionOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = UnionFindSetPtr<RuntimeType>;
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();
    type ExpressionOutput = UnionFindSetPtr<RuntimeType>;
    type LValueOutput = UnionFindSetPtr<RuntimeType>;
    type TypeOutput = UnionFindSetPtr<RuntimeType>;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        for f in &mut program.functions {
            self.register_function(f);
        }
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }

        // specify all int sizes
        for type_ in self.type_sets.data.values_mut() {
            type_.specialize_int_size();
        }

        TypeAnalysisData {
            type_data: self.node_types,
            type_sets: self.type_sets,
            variable_data: self.referenced_variable,
            function_data: self.referenced_function,
            scope_data: self.contained_scope,
        }
    }

    fn visit_function_definition(
        &mut self,
        FunctionDefinition {
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
        }: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        let this_function = self
            .function_list
            .get(name)
            .expect("All functions should have been registered before traversing the tree")
            .clone();

        if let Some(return_type) = return_type {
            self.visit_type(return_type);
        }

        self.current_function = Some(this_function.clone());

        self.variable_scopes.push_child(); // the parameter scope
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
        let simple_binding_clone = simple_binding.clone();
        let SimpleBinding {
            name_token: _,
            name,
            type_,
            id,
        } = simple_binding;

        let defined_type = if let Some((_colon, type_)) = type_ {
            self.visit_type(type_)
        } else {
            self.type_sets.insert_set(RuntimeType::Unknown)
        };
        if self
            .variable_scopes
            .try_insert(Variable {
                name: name.clone(),
                type_: defined_type,
                is_constant: false,
                id: self.id_generator.next_variable_id(),
            })
            .is_err()
        {
            self.errors.push(FleetError::from_node(
                &simple_binding_clone,
                format!(
                    "A variable named {} was already defined in this scope",
                    name.clone()
                ),
                ErrorSeverity::Error,
            ));
        }
        if *self.type_sets.get(defined_type) == RuntimeType::Unit {
            self.errors.push(FleetError::from_node(
                &type_
                    .as_ref()
                    .map(|(_colon, type_)| Into::<AstNode>::into(type_.clone()))
                    .unwrap_or(simple_binding_clone.into()),
                "Variables cannot have Unit type".to_string(),
                ErrorSeverity::Error,
            ));
        }

        self.referenced_variable.insert(
            *id,
            self.variable_scopes
                .get_write(name)
                .expect("This variable should have just been added")
                .clone(),
        );
        self.node_types.insert(*id, defined_type);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        defined_type
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let type_ = self.visit_expression(expression);
        self.type_sets.get_mut(type_).specialize_int_size();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_on_statement(
        &mut self,
        OnStatement {
            on_token: _,
            executor,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id,
        }: &mut OnStatement,
    ) -> Self::StatementOutput {
        self.variable_scopes.push_child();
        self.variable_scopes.top_mut().copy_from_parent = true;

        self.visit_executor(executor);

        for (binding, _comma) in bindings {
            self.visit_lvalue(binding);
        }

        // cannot write to the outside anymore
        self.variable_scopes.top_mut().is_write_guard = true;

        self.visit_statement(body);

        self.variable_scopes.pop();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
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
        self.variable_scopes.push_child();

        for stmt in body {
            self.visit_statement(stmt);
        }

        self.variable_scopes.pop();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_return_statement(&mut self, stmt: &mut ReturnStatement) -> Self::StatementOutput {
        let stmt_clone = stmt.clone();
        let ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id,
        } = stmt;

        let this_type = value
            .as_mut()
            .map(|value| self.visit_expression(value))
            .unwrap_or_else(|| self.type_sets.insert_set(RuntimeType::Unit));

        let expected_type = self
            .current_function
            .as_ref()
            .expect("Return statements should only appear inside functions")
            .borrow()
            .return_type;

        if !RuntimeType::merge_types(this_type, expected_type, &mut self.type_sets) {
            self.errors.push(FleetError::from_node(
                &stmt_clone,
                format!(
                    "Expected this functions to return {}. Got {}",
                    self.stringify_type_ptr(expected_type),
                    self.stringify_type_ptr(this_type),
                ),
                ErrorSeverity::Error,
            ));
        }

        self.type_sets.get_mut(this_type).specialize_int_size();
        self.node_types.insert(*id, this_type);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        let stmt_clone = vardef_stmt.clone();
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
        let value_type = self.visit_expression(value);
        let defined_type = self.visit_simple_binding(binding);

        if !RuntimeType::merge_types(value_type, defined_type, &mut self.type_sets) {
            self.errors.push(FleetError::from_node(
                &stmt_clone,
                format!(
                    "Variable {:?} is defined as type {}, but the initializer value has type {}",
                    binding.name,
                    self.stringify_type_ptr(defined_type),
                    self.stringify_type_ptr(value_type),
                ),
                ErrorSeverity::Error,
            ));
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
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
        let cond_type = self.visit_expression(condition);

        self.generate_mismatched_type_error_if(
            !self.type_sets.get(cond_type).is_boolean(),
            condition,
            "if condition",
            "of type bool",
            *self.type_sets.get(cond_type),
        );

        self.visit_statement(if_body);
        for (_elif_token, elif_condition, elif_body) in elifs {
            let elif_type = self.visit_expression(elif_condition);
            self.generate_mismatched_type_error_if(
                !self.type_sets.get(elif_type).is_boolean(),
                elif_condition,
                "elif condition",
                "of type bool",
                *self.type_sets.get(elif_type),
            );

            self.visit_statement(elif_body);
        }
        if let Some((_else_token, else_body)) = else_ {
            self.visit_statement(else_body);
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
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
        let cond_type = self.visit_expression(condition);

        self.generate_mismatched_type_error_if(
            !self.type_sets.get(cond_type).is_boolean(),
            condition,
            "while condition",
            "of type bool",
            *self.type_sets.get(cond_type),
        );
        self.visit_statement(body);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
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
        self.variable_scopes.push_child();
        self.visit_statement(initializer);

        if let Some(con) = condition {
            let cond_type = self.visit_expression(con);
            self.generate_mismatched_type_error_if(
                !self.type_sets.get(cond_type).is_boolean(),
                con,
                "for condition",
                "of type bool",
                *self.type_sets.get(cond_type),
            );
        }
        if let Some(inc) = incrementer {
            self.visit_expression(inc);
        }
        self.visit_statement(body);

        self.variable_scopes.pop();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
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
        let index_type = self.visit_expression(index);

        self.generate_mismatched_type_error_if(
            !self.type_sets.get(index_type).is_numeric(),
            index,
            "thread index",
            "numeric",
            *self.type_sets.get(index_type),
        );

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
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
        self.visit_executor_host(host);
        let index_type = self.visit_expression(gpu_index);

        self.generate_mismatched_type_error_if(
            !self.type_sets.get(index_type).is_numeric(),
            gpu_index,
            "gpu index",
            "numeric",
            *self.type_sets.get(index_type),
        );

        let iterator_type = self.visit_simple_binding(iterator);
        let max_value_type = self.visit_expression(max_value);
        if !RuntimeType::merge_types(iterator_type, max_value_type, &mut self.type_sets) {
            self.errors.push(FleetError::from_node(
                iterator,
                format!(
                    "Iterator is defined as type {}, but has max value of type {}",
                    self.stringify_type_ptr(iterator_type),
                    self.stringify_type_ptr(max_value_type),
                ),
                ErrorSeverity::Error,
            ));
        }

        // TODO: HACK: remive when we have proper constness and mutability
        self.referenced_variable
            .get(&iterator.id)
            .expect("variable data should exist by now")
            .borrow_mut()
            .is_constant = true;

        let is_iterator_const = self
            .referenced_variable
            .get(&iterator.id)
            .expect("variable data should exist by now")
            .borrow()
            .is_constant;
        if !is_iterator_const {
            self.errors.push(FleetError::from_node(
                iterator,
                "The iterator of an on-statement cannot be mutable",
                ErrorSeverity::Error,
            ));
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        let NumberExpression {
            value: _,
            token: _,
            id,
        } = expression;
        let type_ = self.type_sets.insert_set(RuntimeType::UnsizedInt);
        self.node_types.insert(*id, type_);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        type_
    }

    fn visit_bool_expression(&mut self, expression: &mut BoolExpression) -> Self::ExpressionOutput {
        let BoolExpression {
            value: _,
            token: _,
            id,
        } = expression;
        let type_ = self.type_sets.insert_set(RuntimeType::Boolean);
        self.node_types.insert(*id, type_);

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

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

        let element_type = self.type_sets.insert_set(RuntimeType::Unknown);
        for (item, _comma) in &mut *elements {
            let this_item_type = self.visit_expression(item);

            if !RuntimeType::merge_types(element_type, this_item_type, &mut self.type_sets) {
                self.errors.push(FleetError::from_node(
                    item,
                    format!(
                        "This item has type {}, but was expected \
                        to have type {} (inferred from the first element)",
                        self.stringify_type_ptr(this_item_type),
                        self.stringify_type_ptr(element_type),
                    ),
                    ErrorSeverity::Error,
                ));
            }
        }
        self.type_sets.get_mut(element_type).specialize_int_size();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        let type_ = self.type_sets.insert_set(RuntimeType::ArrayOf {
            subtype: element_type,
            size: Some(elements.len()),
        });
        self.node_types.insert(*id, type_);
        type_
    }

    fn visit_function_call_expression(
        &mut self,
        expression: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let FunctionCallExpression {
            name,
            name_token: _,
            open_paren_token: _,
            arguments: _,
            close_paren_token,
            id,
        } = expression;
        let Some(ref_function) = self.function_list.get(name).cloned() else {
            self.errors.push(FleetError::from_node(
                &expression_clone,
                format!("No function named {name:?} is defined"),
                ErrorSeverity::Error,
            ));

            // still typecheck args, even though the function doesn't exist
            for (arg, _comma) in &mut expression.arguments {
                self.visit_expression(arg);
            }

            return self.type_sets.insert_set(RuntimeType::Error);
        };
        self.referenced_function.insert(*id, ref_function.clone());

        let num_expected_arguments = ref_function.borrow().parameter_types.len();

        for (i, types) in expression
            .arguments
            .iter_mut()
            .map(|(arg, _comma)| (self.visit_expression(arg), arg))
            .collect_vec()
            .iter()
            .zip_longest(ref_function.borrow().parameter_types.iter())
            .enumerate()
        {
            match types {
                EitherOrBoth::Both((arg_type, arg), (param_type, param_name)) => {
                    // function arguments shouldn't get specialized by calling the function
                    let param_type = self.type_sets.detach(*param_type);

                    if !RuntimeType::merge_types(*arg_type, param_type, &mut self.type_sets) {
                        self.errors.push(FleetError::from_node(
                            *arg,
                            format!(
                                "{name:?} expects a value of type {} as argument {param_name:?} (Nr. {}). Got {}",
                                self.stringify_type_ptr(param_type),
                                i + 1,
                                self.stringify_type_ptr(*arg_type),
                            ),
                            ErrorSeverity::Error,
                        ));
                    }
                }
                EitherOrBoth::Left((_arg_type, arg)) => {
                    self.errors.push(FleetError::from_node(
                        *arg,
                        format!("{name:?} only has {num_expected_arguments} parameters"),
                        ErrorSeverity::Error,
                    ));
                }
                EitherOrBoth::Right((param_type, name)) => {
                    self.errors.push(FleetError::from_token(
                        close_paren_token,
                        format!(
                            "{name:?} is missing parameter {name:?} (Nr. {}) of type {}",
                            i + 1,
                            self.stringify_type_ptr(*param_type)
                        ),
                        ErrorSeverity::Error,
                    ));
                }
            }
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        let detached_return_type = self.type_sets.detach(ref_function.borrow().return_type);

        self.node_types.insert(*id, detached_return_type);

        detached_return_type
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

        let &RuntimeType::ArrayOf { subtype, size: _ } = self.type_sets.get(array_type) else {
            self.errors.push(FleetError::from_node(
                &**array,
                "Trying to index into non-array typed value",
                ErrorSeverity::Error,
            ));
            let err_type = self.type_sets.insert_set(RuntimeType::Error);
            self.node_types.insert(*id, err_type);
            return err_type;
        };

        self.type_sets.get_mut(index_type).specialize_int_size();

        if !self.type_sets.get(index_type).is_numeric() {
            self.errors.push(FleetError::from_node(
                &**index,
                format!(
                    "Cannot index into array using index of type {}",
                    self.stringify_type_ptr(index_type)
                ),
                ErrorSeverity::Error,
            ));
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        self.node_types.insert(*id, subtype);
        subtype
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
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        let type_ = self.visit_expression(subexpression);
        self.node_types.insert(*id, type_);
        type_
    }

    fn visit_variable_access_expression(
        &mut self,
        expression: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let VariableAccessExpression {
            name,
            name_token: _,
            id,
        } = expression;
        let Some(ref_variable) = self.variable_scopes.get_read(name) else {
            self.errors.push(FleetError::from_node(
                &expression_clone,
                format!("No variable named {name:?} is defined"),
                ErrorSeverity::Error,
            ));
            return self.type_sets.insert_set(RuntimeType::Unknown);
        };
        self.referenced_variable.insert(*id, ref_variable.clone());

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        self.node_types.insert(*id, ref_variable.borrow().type_);
        return ref_variable.borrow().type_;
    }

    fn visit_unary_expression(
        &mut self,
        expression: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let UnaryExpression {
            operator_token: _,
            operation,
            operand,
            id,
        } = expression;

        let type_ = self.visit_expression(operand);
        let this_type = match operation {
            UnaryOperation::BitwiseNot => type_,
            UnaryOperation::LogicalNot => {
                self.type_sets.get_mut(type_).specialize_int_size();
                self.type_sets.insert_set(RuntimeType::Boolean)
            }
            UnaryOperation::Negate => type_,
        };

        let is_ok = match operation {
            UnaryOperation::BitwiseNot => {
                let type_ = self.type_sets.get(type_);
                type_.is_numeric() || *type_ == RuntimeType::Unknown
            }
            UnaryOperation::LogicalNot => {
                let type_ = self.type_sets.get(type_);
                type_.is_numeric() || type_.is_boolean() || *type_ == RuntimeType::Unknown
            }
            UnaryOperation::Negate => {
                let type_ = self.type_sets.get(type_);
                type_.is_numeric() || *type_ == RuntimeType::Unknown
            }
        };

        if !is_ok {
            let (verb, expected) = match operation {
                UnaryOperation::BitwiseNot => ("bitwise negate", "a number"),
                UnaryOperation::LogicalNot => ("logically negate", "a number or boolean"),
                UnaryOperation::Negate => ("arithmetically negate", "a number"),
            };
            self.errors.push(FleetError::from_node(
                &expression_clone,
                format!(
                    "Cannot {verb} {}. Expected {expected}.",
                    self.stringify_type_ptr(type_)
                ),
                ErrorSeverity::Error,
            ));
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

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

        let from = *self.type_sets.get(from_ptr);
        let to = *self.type_sets.get(to_ptr);
        use RuntimeType::*;
        enum CastResult {
            Possible,
            Redundant,
            Impossible,
        }
        fn perform_cast(
            from: RuntimeType,
            to: RuntimeType,
            expression_clone: CastExpression,
            from_ptr: UnionFindSetPtr<RuntimeType>,
            to_ptr: UnionFindSetPtr<RuntimeType>,
            self_errors: &mut Vec<FleetError>,
            types: &mut UnionFindSet<RuntimeType>,
        ) -> CastResult {
            match (from, to) {
                (_, Error) | (Error, _) => CastResult::Possible,

                (I8, I8)
                | (I16, I16)
                | (I32, I32)
                | (I64, I64)
                | (Boolean, Boolean)
                | (Unit, Unit) => {
                    self_errors.push(FleetError::from_node(
                        &expression_clone,
                        format!("Casting {} to itself is redundant", from.stringify(types)),
                        ErrorSeverity::Warning,
                    ));
                    CastResult::Redundant
                }
                (I8 | I16 | I32 | I64 | UnsizedInt, I8 | I16 | I32 | I64 | UnsizedInt) => {
                    let _ = RuntimeType::merge_types(from_ptr, to_ptr, types);
                    CastResult::Possible
                }

                (I8 | I16 | I32 | I64 | UnsizedInt, Boolean) => {
                    types.get_mut(from_ptr).specialize_int_size();
                    CastResult::Possible
                }
                (Boolean, I8 | I16 | I32 | I64 | UnsizedInt) => CastResult::Possible,

                (_, Unit) | (Unit, _) => {
                    self_errors.push(FleetError::from_node(
                        &expression_clone,
                        "Cannot cast to or from Unit".to_string(),
                        ErrorSeverity::Error,
                    ));
                    CastResult::Impossible
                }
                (
                    ArrayOf {
                        subtype: a,
                        size: a_size,
                    },
                    ArrayOf {
                        subtype: b,
                        size: b_size,
                    },
                ) if a_size == b_size || a_size.is_none() || b_size.is_none() => {
                    let _ = RuntimeType::merge_types(from_ptr, to_ptr, types);
                    let res = perform_cast(
                        *types.get(a),
                        *types.get(b),
                        expression_clone.clone(),
                        from_ptr,
                        to_ptr,
                        self_errors,
                        types,
                    );
                    match res {
                        CastResult::Possible => CastResult::Possible,
                        CastResult::Redundant => {
                            self_errors.push(FleetError::from_node(
                                &expression_clone,
                                format!(
                                    "Casting array of type {} to array of type {} is \
                                    redundant because the element types are equal",
                                    from.stringify(types),
                                    to.stringify(types)
                                ),
                                ErrorSeverity::Warning,
                            ));
                            CastResult::Redundant
                        }
                        CastResult::Impossible => {
                            self_errors.push(FleetError::from_node(
                                &expression_clone,
                                format!(
                                    "Casting array of type {} to array of type {} is \
                                    impossible because the element types can't be cast",
                                    from.stringify(types),
                                    to.stringify(types)
                                ),
                                ErrorSeverity::Error,
                            ));
                            CastResult::Impossible
                        }
                    }
                }
                (
                    ArrayOf {
                        subtype: _,
                        size: a_size,
                    },
                    ArrayOf {
                        subtype: _,
                        size: b_size,
                    },
                ) => {
                    self_errors.push(FleetError::from_node(
                        &expression_clone,
                        format!(
                            "Casting array with length {} to length {} is impossible",
                            a_size
                                .map(|x| x.to_string())
                                .unwrap_or("{unknown}".to_string()),
                            b_size
                                .map(|x| x.to_string())
                                .unwrap_or("{unknown}".to_string())
                        ),
                        ErrorSeverity::Error,
                    ));
                    CastResult::Impossible
                }
                (
                    _,
                    ArrayOf {
                        subtype: _,
                        size: _,
                    },
                ) => {
                    self_errors.push(FleetError::from_node(
                        &expression_clone,
                        format!(
                            "Casting non-array of type {} to array of type {} is impossible",
                            from.stringify(types),
                            to.stringify(types)
                        ),
                        ErrorSeverity::Error,
                    ));
                    CastResult::Impossible
                }
                (
                    ArrayOf {
                        subtype: _,
                        size: _,
                    },
                    _,
                ) => {
                    self_errors.push(FleetError::from_node(
                        &expression_clone,
                        format!(
                            "Casting array of type {} to non-array of type {} is impossible",
                            from.stringify(types),
                            to.stringify(types)
                        ),
                        ErrorSeverity::Error,
                    ));
                    CastResult::Impossible
                }
                (Unknown, Unknown) => {
                    if !RuntimeType::merge_types(from_ptr, to_ptr, types) {
                        self_errors.push(FleetError::from_node(
                            &expression_clone,
                            format!(
                                "Casting value of type {} to value of type {} is impossible",
                                from.stringify(types),
                                to.stringify(types)
                            ),
                            ErrorSeverity::Error,
                        ));
                    }
                    self_errors.push(FleetError::from_node(
                        &expression_clone,
                        format!(
                            "Casting value of type {} to value of type {} is redundant",
                            from.stringify(types),
                            to.stringify(types)
                        ),
                        ErrorSeverity::Warning,
                    ));
                    CastResult::Possible
                }
                (_, Unknown) => CastResult::Possible,
                (Unknown, _) => {
                    if !RuntimeType::merge_types(from_ptr, to_ptr, types) {
                        self_errors.push(FleetError::from_node(
                            &expression_clone,
                            format!(
                                "Casting value of type {} to value of type {} is impossible",
                                from.stringify(types),
                                to.stringify(types)
                            ),
                            ErrorSeverity::Error,
                        ));
                    }
                    CastResult::Possible
                }
            }
        }
        perform_cast(
            from,
            to,
            expression_clone,
            from_ptr,
            to_ptr,
            self.errors,
            &mut self.type_sets,
        );

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

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

        let is_left_ok = match operation {
            BinaryOperation::Add
            | BinaryOperation::Subtract
            | BinaryOperation::Multiply
            | BinaryOperation::Divide
            | BinaryOperation::Modulo
            | BinaryOperation::GreaterThan
            | BinaryOperation::GreaterThanOrEqual
            | BinaryOperation::LessThan
            | BinaryOperation::LessThanOrEqual => RuntimeType::merge_types(
                left_type,
                self.type_sets.insert_set(RuntimeType::UnsizedInt),
                &mut self.type_sets,
            ),
            BinaryOperation::Equal | BinaryOperation::NotEqual => {
                RuntimeType::merge_types(
                    left_type,
                    self.type_sets.insert_set(RuntimeType::UnsizedInt),
                    &mut self.type_sets,
                ) || RuntimeType::merge_types(
                    left_type,
                    self.type_sets.insert_set(RuntimeType::Boolean),
                    &mut self.type_sets,
                )
            }

            BinaryOperation::LogicalAnd | BinaryOperation::LogicalOr => RuntimeType::merge_types(
                left_type,
                self.type_sets.insert_set(RuntimeType::Boolean),
                &mut self.type_sets,
            ),
        };

        self.type_sets.get_mut(left_type).specialize_int_size();

        let is_right_ok = match operation {
            BinaryOperation::Add
            | BinaryOperation::Subtract
            | BinaryOperation::Multiply
            | BinaryOperation::Divide
            | BinaryOperation::Modulo
            | BinaryOperation::GreaterThan
            | BinaryOperation::GreaterThanOrEqual
            | BinaryOperation::LessThan
            | BinaryOperation::LessThanOrEqual => RuntimeType::merge_types(
                right_type,
                self.type_sets.insert_set(RuntimeType::UnsizedInt),
                &mut self.type_sets,
            ),
            BinaryOperation::Equal | BinaryOperation::NotEqual => {
                RuntimeType::merge_types(
                    right_type,
                    self.type_sets.insert_set(RuntimeType::UnsizedInt),
                    &mut self.type_sets,
                ) || RuntimeType::merge_types(
                    right_type,
                    self.type_sets.insert_set(RuntimeType::Boolean),
                    &mut self.type_sets,
                )
            }

            BinaryOperation::LogicalAnd | BinaryOperation::LogicalOr => RuntimeType::merge_types(
                right_type,
                self.type_sets.insert_set(RuntimeType::Boolean),
                &mut self.type_sets,
            ),
        };
        self.type_sets.get_mut(right_type).specialize_int_size();

        if !RuntimeType::merge_types(left_type, right_type, &mut self.type_sets)
            || !is_left_ok
            || !is_right_ok
        {
            let (verb, preposition, l_expected, r_expected) = match operation {
                BinaryOperation::Add => ("add", "to", "number", "number"),
                BinaryOperation::Subtract => ("subtract", "from", "number", "number"),
                BinaryOperation::Multiply => ("multiply", "by", "number", "number"),
                BinaryOperation::Divide => ("divide", "by", "number", "number"),
                BinaryOperation::Modulo => ("modulo", "by", "number", "number"),
                BinaryOperation::GreaterThan => ("compare", ">", "number", "number"),
                BinaryOperation::GreaterThanOrEqual => ("compare", ">=", "number", "number"),
                BinaryOperation::LessThan => ("compare", "<", "number", "number"),
                BinaryOperation::LessThanOrEqual => ("compare", "<=", "number", "number"),
                BinaryOperation::Equal => {
                    ("compare", "==", "number or boolean", "number or boolean")
                }
                BinaryOperation::NotEqual => {
                    ("compare", "!=", "number or boolean", "number or boolean")
                }
                BinaryOperation::LogicalAnd => ("logically AND", "with", "boolean", "boolean"),
                BinaryOperation::LogicalOr => ("logically OR", "with", "boolean", "boolean"),
            };
            if !is_left_ok {
                self.errors.push(FleetError::from_node(
                    &**left,
                    format!(
                        "Cannot {verb} {}. Expected {l_expected}.",
                        self.stringify_type_ptr(left_type)
                    ),
                    ErrorSeverity::Error,
                ));
            }
            if !is_right_ok {
                self.errors.push(FleetError::from_node(
                    &**right,
                    format!(
                        "Cannot {verb} {preposition} {}. Expected {r_expected}.",
                        self.stringify_type_ptr(right_type)
                    ),
                    ErrorSeverity::Error,
                ));
            }
            if is_left_ok && is_right_ok && left_type != right_type {
                self.errors.push(FleetError::from_node(
                    &expression_clone,
                    format!(
                        "Cannot {} {} {} {}. Expected {} and {}.",
                        verb,
                        self.stringify_type_ptr(left_type),
                        preposition,
                        self.stringify_type_ptr(right_type),
                        l_expected,
                        r_expected
                    ),
                    ErrorSeverity::Error,
                ));
            }
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

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
            | BinaryOperation::LogicalOr => self.type_sets.insert_set(RuntimeType::Boolean),
        };
        self.node_types.insert(*id, this_type);
        this_type
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id,
        } = expression;
        let left_type = self.visit_lvalue(lvalue);
        let right_type = self.visit_expression(right);

        if !RuntimeType::merge_types(left_type, right_type, &mut self.type_sets) {
            self.errors.push(FleetError::from_node(
                &expression_clone,
                format!(
                    "Cannot assign value of type {} to lvalue of type {}",
                    self.stringify_type_ptr(right_type),
                    self.stringify_type_ptr(left_type),
                ),
                ErrorSeverity::Error,
            ));
        }

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        self.node_types.insert(*id, left_type);
        left_type
    }
    fn visit_variable_lvalue(&mut self, lvalue: &mut VariableLValue) -> Self::LValueOutput {
        let lvalue_clone = lvalue.clone();
        let VariableLValue {
            name,
            name_token: _,
            id,
        } = lvalue;
        let Some(ref_variable) = self.variable_scopes.get_write(name) else {
            self.errors.push(FleetError::from_node(
                &lvalue_clone,
                format!("No variable named {name:?} is defined"),
                ErrorSeverity::Error,
            ));
            return self.type_sets.insert_set(RuntimeType::Unknown);
        };
        if ref_variable.borrow().is_constant {
            self.errors.push(FleetError::from_node(
                &lvalue_clone,
                format!("Variable {name:?} is constant and can't be used as an lvalue"),
                ErrorSeverity::Error,
            ));
        }
        self.referenced_variable.insert(*id, ref_variable.clone());

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        self.node_types.insert(*id, ref_variable.borrow().type_);
        return ref_variable.borrow().type_;
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

        let array_type = self.visit_lvalue(array);
        let index_type = self.visit_expression(index);

        let RuntimeType::ArrayOf { subtype, size: _ } = *self.type_sets.get(array_type) else {
            self.errors.push(FleetError::from_node(
                &**array,
                "Trying to index into non-array typed value",
                ErrorSeverity::Error,
            ));
            let err_type = self.type_sets.insert_set(RuntimeType::Error);
            self.node_types.insert(*id, err_type);
            return err_type;
        };

        if !RuntimeType::merge_types(
            index_type,
            self.type_sets.insert_set(RuntimeType::UnsizedInt),
            &mut self.type_sets,
        ) {
            self.errors.push(FleetError::from_node(
                &**index,
                format!(
                    "Cannot index into array using index of type {}",
                    self.stringify_type_ptr(index_type)
                ),
                ErrorSeverity::Error,
            ));
        }

        self.type_sets.get_mut(index_type).specialize_int_size();

        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        self.node_types.insert(*id, subtype);
        subtype
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

    fn visit_int_type(
        &mut self,
        IntType {
            token: _,
            type_,
            id,
        }: &mut IntType,
    ) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        // make sure function registration gets the same type as the full eval
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let t = self.type_sets.insert_set(*type_);
        self.node_types.insert(*id, t);
        t
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

        // make sure function registration gets the same type as the full eval
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let t = self.type_sets.insert_set(RuntimeType::Unit);
        self.node_types.insert(*id, t);
        t
    }

    fn visit_bool_type(&mut self, BoolType { token: _, id }: &mut BoolType) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        // make sure function registration gets the same type as the full eval
        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let t = self.type_sets.insert_set(RuntimeType::Boolean);
        self.node_types.insert(*id, t);
        t
    }

    fn visit_idk_type(&mut self, IdkType { token: _, id }: &mut IdkType) -> Self::TypeOutput {
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let t = self.type_sets.insert_set(RuntimeType::Unknown);
        self.node_types.insert(*id, t);
        t
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
        self.contained_scope
            .insert(*id, self.variable_scopes.current.clone());

        if let Some(type_) = self.node_types.get(id) {
            return *type_;
        }
        let subtype_t = self.visit_type(subtype);

        if matches!(self.type_sets.get(subtype_t), RuntimeType::Unit) {
            self.errors.push(FleetError::from_node(
                &**subtype,
                "Cannot have array of Unit".to_string(),
                ErrorSeverity::Error,
            ));
        }
        let size = match size.as_ref().map(|boxed_x| &**boxed_x) {
            Some(Expression::Number(NumberExpression {
                value,
                token: _,
                id: _,
            })) => Some(*value as usize),
            Some(_) => {
                // TODO: once we have consteval, relax this restriction
                self.errors.push(FleetError::from_node(
                    &**subtype,
                    "Arrays can only have number literals as a size for now".to_string(),
                    ErrorSeverity::Error,
                ));
                None
            }
            None => None,
        };

        let t = self.type_sets.insert_set(RuntimeType::ArrayOf {
            subtype: subtype_t,
            size,
        });
        self.node_types.insert(*id, t);
        t
    }
}
