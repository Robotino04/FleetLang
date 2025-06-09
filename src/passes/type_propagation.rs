use std::{cell::RefCell, collections::HashMap, error::Error, rc::Rc};

use itertools::{EitherOrBoth, Itertools};

use crate::{
    ast::{
        AstNode, AstVisitor, BinaryExpression, BinaryOperation, BlockStatement, BoolExpression,
        BoolType, BreakStatement, CastExpression, ExpressionStatement, ExternFunctionBody,
        ForLoopStatement, FunctionCallExpression, FunctionDefinition, GroupingExpression, I32Type,
        IfStatement, NumberExpression, OnStatement, PerNodeData, Program, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SkipStatement, StatementFunctionBody, ThreadExecutor,
        UnaryExpression, UnaryOperation, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, WhileLoopStatement,
    },
    infra::{ErrorSeverity, FleetError},
    parser::IdGenerator,
};

type AnyResult<T> = ::core::result::Result<T, Box<dyn Error>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum RuntimeType {
    I32,
    Boolean,
    Unit,
    Unknown,
}

impl RuntimeType {
    pub fn is_numeric(self) -> bool {
        match self {
            RuntimeType::I32 => true,
            RuntimeType::Unit => false,
            RuntimeType::Boolean => false,
            RuntimeType::Unknown => false,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct VariableID(pub u64);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct FunctionID(pub u64);

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: String,
    pub type_: RuntimeType,
    pub id: VariableID,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: String,
    pub return_type: RuntimeType,
    pub parameter_types: Vec<RuntimeType>,
    pub id: FunctionID,
}

#[derive(Clone)]
struct VariableScopeStack {
    scopes: Vec<HashMap<String, Rc<RefCell<Variable>>>>,
}

impl VariableScopeStack {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn get<'b>(&'b self, name: &String) -> Option<&'b Rc<RefCell<Variable>>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        return None;
    }
    pub fn try_insert(&mut self, var: Variable) -> AnyResult<()> {
        let scope = self
            .scopes
            .last_mut()
            .expect("A variable scope stack must always contain at least one scope");
        if scope.contains_key(&var.name) {
            return Err("variable already exists".into());
        } else {
            scope.insert(var.name.clone(), Rc::new(RefCell::new(var)));
            return Ok(());
        }
    }
    pub fn push_child(&mut self) {
        self.scopes.push(HashMap::new());
    }
    pub fn pop<'b>(&mut self) {
        self.scopes.pop();
        assert!(self.scopes.len() >= 1);
    }
}

pub struct TypePropagator<'a> {
    types: PerNodeData<RuntimeType>,
    errors: &'a mut Vec<FleetError>,
    variable_scopes: VariableScopeStack,
    referenced_variable: PerNodeData<Rc<RefCell<Variable>>>,
    function_list: HashMap<String, Rc<RefCell<Function>>>,
    referenced_function: PerNodeData<Rc<RefCell<Function>>>,
    id_generator: &'a mut IdGenerator,
    current_function: Option<Rc<RefCell<Function>>>,
}

impl<'a> TypePropagator<'a> {
    pub fn new(error_output: &'a mut Vec<FleetError>, id_generator: &'a mut IdGenerator) -> Self {
        Self {
            types: PerNodeData::new(),
            errors: error_output,
            variable_scopes: VariableScopeStack::new(),
            referenced_variable: PerNodeData::new(),
            function_list: HashMap::new(),
            referenced_function: PerNodeData::new(),
            id_generator,
            current_function: None,
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

        let return_type = self.visit_type(return_type);
        let parameter_types = parameters
            .iter_mut()
            .map(|(param, _comma)| self.visit_simple_binding(param))
            .collect();

        if let Some(_) = self.function_list.insert(
            name.clone(),
            Rc::new(RefCell::new(Function {
                name: name.clone(),
                return_type,
                parameter_types,
                id: self.id_generator.next_function_id(),
            })),
        ) {
            self.errors.push(FleetError::from_node(
                function_clone,
                format!("Multiple functions named {name:?} defined"),
                ErrorSeverity::Error,
            ));
        }
    }

    fn generate_mismatched_type_error_if(
        &mut self,
        condition: bool,
        node: impl Into<AstNode>,
        expression_type: impl AsRef<str>,
        expected_name: impl AsRef<str>,
        actual_type: RuntimeType,
    ) {
        if condition {
            self.errors.push(FleetError::from_node(
                node,
                format!(
                    "Expected {} to be {}. Got value of type {:?} instead",
                    expression_type.as_ref(),
                    expected_name.as_ref(),
                    actual_type
                ),
                ErrorSeverity::Error,
            ));
        }
    }
}

impl<'errors> AstVisitor for TypePropagator<'errors> {
    type ProgramOutput = (
        PerNodeData<RuntimeType>,
        PerNodeData<Rc<RefCell<Variable>>>,
        PerNodeData<Rc<RefCell<Function>>>,
    );
    type FunctionDefinitionOutput = ();
    type FunctionBodyOutput = ();
    type SimpleBindingOutput = RuntimeType;
    type StatementOutput = ();
    type ExecutorHostOutput = ();
    type ExecutorOutput = ();

    type ExpressionOutput = RuntimeType;

    type TypeOutput = RuntimeType;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        for f in &mut program.functions {
            self.register_function(f);
        }
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }
        return (
            self.types,
            self.referenced_variable,
            self.referenced_function,
        );
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
        self.visit_type(return_type);

        let this_function = self
            .function_list
            .get(name)
            .expect("All functions should have been registered before traversing the tree")
            .clone();
        self.current_function = Some(this_function.clone());

        self.variable_scopes.push_child(); // the parameter scope
        for (param, _comma) in parameters {
            self.visit_simple_binding(param);
        }

        self.visit_function_body(body);
        self.variable_scopes.pop();

        self.referenced_function.insert(id.clone(), this_function);
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
            colon_token: _,
            type_,
            id,
        } = simple_binding;

        let defined_type = self.visit_type(type_);
        if let Err(_) = self.variable_scopes.try_insert(Variable {
            name: name.clone(),
            type_: defined_type,
            id: self.id_generator.next_variable_id(),
        }) {
            self.errors.push(FleetError::from_node(
                simple_binding_clone,
                format!(
                    "A variable named {:?} was already defined in this scope",
                    name.clone()
                ),
                ErrorSeverity::Error,
            ));
        }
        if defined_type == RuntimeType::Unit {
            self.errors.push(FleetError::from_node(
                type_.clone(),
                format!("Variables cannot have Unit type"),
                ErrorSeverity::Error,
            ));
        }

        self.referenced_variable.insert(
            *id,
            self.variable_scopes
                .get(name)
                .expect("This variable should have just been added")
                .clone(),
        );
        return defined_type;
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.visit_expression(expression);
    }

    fn visit_on_statement(
        &mut self,
        OnStatement {
            on_token: _,
            open_paren_token: _,
            executor,
            close_paren_token: _,
            body,
            id: _,
        }: &mut OnStatement,
    ) -> Self::StatementOutput {
        self.visit_executor(executor);
        self.visit_statement(body);
    }

    fn visit_block_statement(
        &mut self,
        BlockStatement {
            open_brace_token: _,
            body,
            close_brace_token: _,
            id: _,
        }: &mut BlockStatement,
    ) -> Self::StatementOutput {
        self.variable_scopes.push_child();

        for stmt in body {
            self.visit_statement(stmt);
        }

        self.variable_scopes.pop();
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
            .unwrap_or(RuntimeType::Unit);

        self.types.insert(*id, this_type);

        let expected_type = self
            .current_function
            .as_ref()
            .expect("Return statements should only appear inside functions")
            .borrow()
            .return_type;

        if this_type != expected_type {
            self.errors.push(FleetError::from_node(
                stmt_clone.clone(),
                format!("Expected this functions to return {expected_type:?}. Got {this_type:?}"),
                ErrorSeverity::Error,
            ));
        }
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
            id: _,
        } = vardef_stmt;

        // evaluated before the binding so it can potentially access variables that gets shadowed
        // by this binding
        let value_type = self.visit_expression(value);

        let defined_type = self.visit_simple_binding(binding);
        if value_type != defined_type {
            self.errors.push(FleetError::from_node(
                stmt_clone.clone(),
                format!("Variable {:?} is defined as type {defined_type:?}, but the initializer value has type {value_type:?}", binding.name),
                ErrorSeverity::Error
            ));
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

        self.generate_mismatched_type_error_if(
            !match cond_type {
                RuntimeType::I32 => false,
                RuntimeType::Unit => false,
                RuntimeType::Boolean => true,
                RuntimeType::Unknown => false,
            },
            condition.clone(),
            "if condition",
            "of type bool",
            cond_type,
        );

        self.visit_statement(if_body);
        for (_elif_token, elif_condition, elif_body) in elifs {
            let elif_type = self.visit_expression(elif_condition);
            self.generate_mismatched_type_error_if(
                !match elif_type {
                    RuntimeType::I32 => false,
                    RuntimeType::Unit => false,
                    RuntimeType::Boolean => true,
                    RuntimeType::Unknown => false,
                },
                elif_condition.clone(),
                "elif condition",
                "of type bool",
                elif_type,
            );

            self.visit_statement(elif_body);
        }
        if let Some((_else_token, else_body)) = else_ {
            self.visit_statement(else_body);
        }
    }

    fn visit_while_loop_statement(
        &mut self,
        while_stmt: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        let cond_type = self.visit_expression(&mut while_stmt.condition);

        self.generate_mismatched_type_error_if(
            !match cond_type {
                RuntimeType::I32 => false,
                RuntimeType::Unit => false,
                RuntimeType::Boolean => true,
                RuntimeType::Unknown => false,
            },
            while_stmt.condition.clone(),
            "while condition",
            "of type bool",
            cond_type,
        );
        self.visit_statement(&mut while_stmt.body);
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
        self.variable_scopes.push_child();
        self.visit_statement(initializer);

        if let Some(con) = condition {
            let cond_type = self.visit_expression(con);
            self.generate_mismatched_type_error_if(
                !match cond_type {
                    RuntimeType::I32 => false,
                    RuntimeType::Unit => false,
                    RuntimeType::Boolean => true,
                    RuntimeType::Unknown => false,
                },
                con.clone(),
                "for condition",
                "of type bool",
                cond_type,
            );
        }
        if let Some(inc) = incrementer {
            self.visit_expression(inc);
        }
        self.visit_statement(body);

        self.variable_scopes.pop();
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
            thread_token: _,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ThreadExecutor,
    ) -> Self::ExecutorOutput {
        self.visit_executor_host(host);
        let index_type = self.visit_expression(index);

        self.generate_mismatched_type_error_if(
            !match index_type {
                RuntimeType::I32 => true,
                RuntimeType::Unit => false,
                RuntimeType::Boolean => false,
                RuntimeType::Unknown => false,
            },
            index.clone(),
            "thread index",
            "numeric",
            index_type,
        );
    }

    fn visit_number_expression(
        &mut self,
        expression: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        let NumberExpression {
            value: _,
            token: _,
            id: _,
        } = expression;
        self.types.insert_node(expression, RuntimeType::I32);
        return RuntimeType::I32;
    }

    fn visit_bool_expression(
        &mut self,
        expression: &mut crate::ast::BoolExpression,
    ) -> Self::ExpressionOutput {
        let BoolExpression {
            value: _,
            token: _,
            id: _,
        } = expression;
        self.types.insert_node(expression, RuntimeType::Boolean);
        return RuntimeType::Boolean;
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
                expression_clone.clone(),
                format!("No function named {name:?} is defined"),
                ErrorSeverity::Error,
            ));

            // still typecheck args, even though the function doesn't exist
            for (arg, _comma) in &mut expression.arguments {
                self.visit_expression(arg);
            }
            return RuntimeType::Unknown;
        };
        self.referenced_function.insert(*id, ref_function.clone());
        let ref_function = ref_function.borrow();

        let num_expected_arguments = ref_function.parameter_types.len();

        let self_shared = RefCell::new(&mut *self);

        expression
            .arguments
            .iter_mut()
            .map(|(arg, _comma)| (self_shared.borrow_mut().visit_expression(arg), arg))
            .zip_longest(ref_function.parameter_types.iter())
            .enumerate()
            .for_each(|(i, types)| match types {
                EitherOrBoth::Both((arg_type, arg), param_type) => {
                    if arg_type != *param_type {
                        self_shared.borrow_mut().errors.push(FleetError::from_node(
                            arg.clone(),
                            format!(
                                "{name:?} expects a value of type {param_type:?}\
                                as argument {i}. Got {arg_type:?}"
                            ),
                            ErrorSeverity::Error,
                        ));
                    }
                }
                EitherOrBoth::Left((_arg_type, arg)) => {
                    self_shared.borrow_mut().errors.push(FleetError::from_node(
                        arg.clone(),
                        format!("{name:?} only has {num_expected_arguments} parameters"),
                        ErrorSeverity::Error,
                    ));
                }
                EitherOrBoth::Right(param_type) => {
                    self_shared.borrow_mut().errors.push(FleetError::from_token(
                        close_paren_token,
                        format!(
                            "{name:?} is missing parameter {} of type {param_type:?}",
                            i + 1
                        ),
                        ErrorSeverity::Error,
                    ));
                }
            });

        self.types.insert_node(expression, ref_function.return_type);
        return ref_function.return_type;
    }

    fn visit_grouping_expression(
        &mut self,
        expression: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        let type_ = self.visit_expression(&mut expression.subexpression);
        self.types.insert_node(expression, type_);
        return type_;
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
        let Some(ref_variable) = self.variable_scopes.get(name) else {
            self.errors.push(FleetError::from_node(
                expression_clone,
                format!("No variable named {name:?} is defined"),
                ErrorSeverity::Error,
            ));
            return RuntimeType::Unknown;
        };
        self.referenced_variable.insert(*id, ref_variable.clone());
        let ref_variable = ref_variable.borrow();

        self.types.insert(*id, ref_variable.type_);
        return ref_variable.type_;
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

        let is_ok = match operation {
            UnaryOperation::BitwiseNot => type_.is_numeric(),
            UnaryOperation::LogicalNot => type_.is_numeric() || type_ == RuntimeType::Boolean,
            UnaryOperation::Negate => type_.is_numeric(),
        };

        if !is_ok {
            let (verb, expected) = match operation {
                UnaryOperation::BitwiseNot => ("bitwise negate", "a number"),
                UnaryOperation::LogicalNot => ("logically negate", "a number or boolean"),
                UnaryOperation::Negate => ("arithmetically negate", "a number"),
            };
            self.errors.push(FleetError::from_node(
                expression_clone,
                format! {"Cannot {verb} {type_:?}. Expected {expected}."},
                ErrorSeverity::Error,
            ));
        }

        let this_type = match operation {
            UnaryOperation::BitwiseNot => type_,
            UnaryOperation::LogicalNot => RuntimeType::Boolean,
            UnaryOperation::Negate => type_,
        };
        self.types.insert(*id, this_type);
        return this_type;
    }

    fn visit_cast_expression(&mut self, expression: &mut CastExpression) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let CastExpression {
            operand,
            as_token: _,
            type_,
            id,
        } = expression;

        let from_type = self.visit_expression(operand);
        let to_type = self.visit_type(type_);

        match (from_type, to_type) {
            (RuntimeType::I32, RuntimeType::I32)
            | (RuntimeType::Boolean, RuntimeType::Boolean)
            | (RuntimeType::Unit, RuntimeType::Unit) => {
                self.errors.push(FleetError::from_node(
                    expression_clone,
                    format!("Casting {from_type:?} to itself is redundant"),
                    ErrorSeverity::Warning,
                ));
            }
            (RuntimeType::I32, RuntimeType::I32) => {} // here for future iX to iX casts

            (RuntimeType::I32, RuntimeType::Boolean) => {}
            (RuntimeType::Boolean, RuntimeType::I32) => {}

            (_, RuntimeType::Unit) | (RuntimeType::Unit, _) => {
                self.errors.push(FleetError::from_node(
                    expression_clone,
                    format!("Cannot cast to or from Unit"),
                    ErrorSeverity::Error,
                ));
            }
            (_, RuntimeType::Unknown) | (RuntimeType::Unknown, _) => {
                self.errors.push(FleetError::from_node(
                    expression_clone,
                    format!("Type of this expression is unknown"),
                    ErrorSeverity::Error,
                ));
            }
        }

        self.types.insert(*id, to_type);
        return to_type;
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
            | BinaryOperation::LessThanOrEqual => right_type.is_numeric(),
            BinaryOperation::Equal | BinaryOperation::NotEqual => {
                right_type.is_numeric() || right_type == RuntimeType::Boolean
            }
            BinaryOperation::LogicalAnd | BinaryOperation::LogicalOr => {
                left_type == RuntimeType::Boolean
            }
        };

        let is_right_ok = match operation {
            BinaryOperation::Add
            | BinaryOperation::Subtract
            | BinaryOperation::Multiply
            | BinaryOperation::Divide
            | BinaryOperation::Modulo
            | BinaryOperation::GreaterThan
            | BinaryOperation::GreaterThanOrEqual
            | BinaryOperation::LessThan
            | BinaryOperation::LessThanOrEqual => right_type.is_numeric(),
            BinaryOperation::Equal | BinaryOperation::NotEqual => {
                right_type.is_numeric() || right_type == RuntimeType::Boolean
            }
            BinaryOperation::LogicalAnd | BinaryOperation::LogicalOr => {
                right_type == RuntimeType::Boolean
            }
        };

        if left_type != right_type || !is_left_ok || !is_right_ok {
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
                    (**left).clone(),
                    format!("Cannot {verb} {left_type:?}. Expected {l_expected}."),
                    ErrorSeverity::Error,
                ));
            }
            if !is_right_ok {
                self.errors.push(FleetError::from_node(
                    (**right).clone(),
                    format!("Cannot {verb} {preposition} {right_type:?}. Expected {r_expected}."),
                    ErrorSeverity::Error,
                ));
            }
            if is_left_ok && is_right_ok && left_type != right_type {
                self.errors.push(FleetError::from_node(
                    expression_clone,
                    format!(
                        "Cannot {} {:?} {} {:?}. Expected {} and {}.",
                        verb, left_type, preposition, right_type, l_expected, r_expected
                    ),
                    ErrorSeverity::Error,
                ));
            }
        }

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
            | BinaryOperation::LogicalOr => RuntimeType::Boolean,
        };
        self.types.insert(*id, this_type);
        return this_type;
    }

    fn visit_variable_assignment_expression(
        &mut self,
        expression: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let VariableAssignmentExpression {
            name,
            name_token: _,
            equal_token: _,
            right,
            id,
        } = expression;
        let Some(ref_variable) = self.variable_scopes.get(name).cloned() else {
            self.errors.push(FleetError::from_node(
                expression_clone.clone(),
                format!("No variable named {name:?} is defined"),
                ErrorSeverity::Error,
            ));
            return RuntimeType::Unknown;
        };
        self.referenced_variable.insert(*id, ref_variable.clone());
        let ref_variable = ref_variable.borrow();

        let defined_type = ref_variable.type_;

        let right_type = self.visit_expression(right);
        if right_type != defined_type {
            self.errors.push(FleetError::from_node(
                expression_clone.clone(),
                    format!("Cannot assign value of type {right_type:?} to variable {name:?} defined as type {defined_type:?}"),
                ErrorSeverity::Error
            ));
        }

        self.types.insert_node(expression, defined_type);
        return defined_type;
    }

    fn visit_i32_type(&mut self, _type: &mut I32Type) -> Self::TypeOutput {
        return RuntimeType::I32;
    }

    fn visit_unit_type(&mut self, _unit_type: &mut UnitType) -> Self::TypeOutput {
        return RuntimeType::Unit;
    }

    fn visit_bool_type(&mut self, _bool_type: &mut BoolType) -> Self::TypeOutput {
        return RuntimeType::Boolean;
    }
}
