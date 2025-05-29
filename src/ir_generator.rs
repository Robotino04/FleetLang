use std::{collections::HashMap, error::Error};

use inkwell::{
    IntPredicate,
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue,
    },
};

use crate::{
    ast::{
        AstVisitor, BinaryExpression, BinaryOperation, BlockStatement, ExpressionStatement,
        FunctionCallExpression, FunctionDefinition, GroupingExpression, I32Type, IfStatement,
        NumberExpression, OnStatement, PerNodeData, Program, ReturnStatement, SelfExecutorHost,
        ThreadExecutor, UnaryExpression, UnaryOperation, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement,
    },
    escape::unescape,
    infra::{ErrorSeverity, FleetError},
    passes::function_termination_analysis::FunctionTermination,
    tokenizer::SourceLocation,
};

type Result<T> = ::core::result::Result<T, Box<dyn Error>>;

#[derive(Clone)]
struct VariableScopeStack<'a> {
    scopes: Vec<HashMap<String, (BasicTypeEnum<'a>, PointerValue<'a>)>>,
}

impl<'a> VariableScopeStack<'a> {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn get<'b>(&'b self, name: &String) -> Option<&'b (BasicTypeEnum<'a>, PointerValue<'a>)> {
        for scope in &self.scopes {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        return None;
    }
    pub fn try_insert(
        &mut self,
        name: &String,
        value: PointerValue<'a>,
        type_: BasicTypeEnum<'a>,
    ) -> Result<()> {
        let scope = self
            .scopes
            .first_mut()
            .expect("A variable scope stack must always contain at least one scope");
        if scope.contains_key(name) {
            return Err("variable already exists".into());
        } else {
            scope.insert(name.clone(), (type_, value));
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

pub struct IrGenerator<'a, 'errors> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    functions: HashMap<String, FunctionValue<'a>>,
    errors: &'errors mut Vec<FleetError>,
    function_termination: PerNodeData<FunctionTermination>,

    variable_scopes: VariableScopeStack<'a>,
}
impl<'a, 'errors> IrGenerator<'a, 'errors> {
    pub fn new(
        context: &'a Context,
        errors: &'errors mut Vec<FleetError>,
        function_termination: PerNodeData<FunctionTermination>,
    ) -> Self {
        let module = context.create_module("module");
        let builder = context.create_builder();

        IrGenerator {
            context,
            module,
            builder,
            functions: HashMap::new(),
            errors,
            function_termination,
            variable_scopes: VariableScopeStack::new(),
        }
    }

    fn make_into_function_type<'c>(
        &self,
        type_: AnyTypeEnum<'c>,
        param_types: &[BasicMetadataTypeEnum<'c>],
        is_var_args: bool,
    ) -> Result<FunctionType<'c>> {
        match type_ {
            AnyTypeEnum::ArrayType(array_type) => Ok(array_type.fn_type(param_types, is_var_args)),
            AnyTypeEnum::FloatType(float_type) => Ok(float_type.fn_type(param_types, is_var_args)),
            AnyTypeEnum::FunctionType(_) => {
                Err("Tried to make a function type into a function type again".into())
            }
            AnyTypeEnum::IntType(int_type) => Ok(int_type.fn_type(param_types, is_var_args)),
            AnyTypeEnum::PointerType(pointer_type) => {
                Ok(pointer_type.fn_type(param_types, is_var_args))
            }
            AnyTypeEnum::StructType(struct_type) => {
                Ok(struct_type.fn_type(param_types, is_var_args))
            }
            AnyTypeEnum::VectorType(vector_type) => {
                Ok(vector_type.fn_type(param_types, is_var_args))
            }
            AnyTypeEnum::VoidType(void_type) => Ok(void_type.fn_type(param_types, is_var_args)),
        }
    }
}

impl<'a, 'errors> AstVisitor for IrGenerator<'a, 'errors> {
    type ProgramOutput = Result<Module<'a>>;
    type FunctionDefinitionOutput = Result<FunctionValue<'a>>;
    type StatementOutput = Result<()>;
    type ExecutorHostOutput = Result<()>;
    type ExecutorOutput = Result<()>;
    type ExpressionOutput = Result<Option<BasicValueEnum<'a>>>;
    type TypeOutput = Result<AnyTypeEnum<'a>>;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        eprintln!("Generating program");

        for f in &mut program.functions {
            self.visit_function_definition(f)?;
        }

        if let Err(err) = self.module.verify() {
            self.errors.push(FleetError {
                start: SourceLocation::start(),
                end: program
                    .functions
                    .last()
                    .map_or(SourceLocation::start(), |f| f.close_paren_token.end),
                message: format!(
                    "LLVM module is invalid: {}\nModule dump:\n{}",
                    unescape(err.to_str().unwrap()),
                    self.module.print_to_string().to_str().unwrap()
                ),
                severity: ErrorSeverity::Error,
            });
        }

        return Ok(self.module);
    }

    fn visit_function_definition(
        &mut self,
        function: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        eprintln!("Generating function {:?}", function.name);

        let return_type_ir = self.visit_type(&mut function.return_type)?;
        let ir_function = self.module.add_function(
            &function.name,
            self.make_into_function_type(return_type_ir, &[], false)?,
            None,
        );

        let entry = self.context.append_basic_block(ir_function, "entry");
        self.builder.position_at_end(entry);

        self.variable_scopes.push_child(); // the parameter scope
        self.visit_statement(&mut function.body)?;
        self.variable_scopes.pop();

        let current_block = self
            .builder
            .get_insert_block()
            .expect("A function should always have a body");

        if *self
            .function_termination
            .get(&function.body)
            .expect("all function bodies should have been analyzed for termination")
            == FunctionTermination::Terminates
        {
            if current_block.get_terminator() == None {
                // All code paths have already returned, so just remove this block. It is either empty
                // or contains unreachable code.
                current_block
                    .remove_from_function()
                    .map_err(|_| "Removing the last, unreachable block from a function failed")?;
            }
        } else {
            self.errors.push(FleetError::from_node(
                function.body.clone(),
                "All code paths must return.",
                ErrorSeverity::Error,
            ));
        }

        /*
        match function.return_type {
            Type::I32 { token: _ } => self
                .builder
                .build_return(Some(&self.context.i32_type().const_int(0, false)))?,
            // Type::Unit { token } => { self.builder.build_return(None)?; }
        };
        */

        return Ok(ir_function);
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        self.visit_expression(expression)?;
        Ok(())
    }

    fn visit_on_statement(&mut self, _on_stmt: &mut OnStatement) -> Self::StatementOutput {
        todo!()
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
        for substmt in body {
            self.visit_statement(substmt)?;
        }
        self.variable_scopes.pop();
        Ok(())
    }

    fn visit_return_statement(
        &mut self,
        return_stmt: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        let ir_value = self.visit_expression(&mut return_stmt.value)?;

        self.builder.build_return(match &ir_value {
            Some(BasicValueEnum::ArrayValue(array_value)) => Some(array_value),
            Some(BasicValueEnum::IntValue(int_value)) => Some(int_value),
            Some(BasicValueEnum::FloatValue(float_value)) => Some(float_value),
            Some(BasicValueEnum::PointerValue(pointer_value)) => Some(pointer_value),
            Some(BasicValueEnum::StructValue(struct_value)) => Some(struct_value),
            Some(BasicValueEnum::VectorValue(vector_value)) => Some(vector_value),
            None => None,
        })?;
        Ok(())
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        let eval_value = self
            .visit_expression(&mut vardef_stmt.value)?
            .ok_or(format!(
                "Somehow passed using a void value as a variable initializer"
            ))?;
        let ptr = self
            .builder
            .build_alloca(eval_value.get_type().into_int_type(), &vardef_stmt.name)?;
        self.builder.build_store(ptr, eval_value)?;
        if let Err(_) =
            self.variable_scopes
                .try_insert(&vardef_stmt.name, ptr, eval_value.get_type())
        {
            self.errors.push(FleetError::from_token(
                &vardef_stmt.name_token,
                format!(
                    "Variable {:?} was already defined in this scope",
                    vardef_stmt.name
                ),
                ErrorSeverity::Error,
            ));
        }

        Ok(())
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::StatementOutput {
        let current_block = self
            .builder
            .get_insert_block()
            .expect("expected builder to be in a block");

        let body_block = self
            .context
            .insert_basic_block_after(current_block, "if_body");

        let mut next_block = self.context.insert_basic_block_after(body_block, "elif");

        let end_block = self.context.insert_basic_block_after(next_block, "if_end");

        self.builder.position_at_end(current_block);
        let condition_value = self
            .visit_expression(&mut if_stmt.condition)?
            .expect("Somehow passed a Unit value to an if condition");
        let condition_value = self.builder.build_int_compare(
            IntPredicate::NE,
            condition_value.into_int_value(),
            condition_value.get_type().const_zero().into_int_value(),
            "if_condition",
        )?;

        self.builder
            .build_conditional_branch(condition_value, body_block, next_block)?;

        self.builder.position_at_end(body_block);
        self.visit_statement(&mut if_stmt.if_body)?;

        if *self
            .function_termination
            .get(&*if_stmt.if_body)
            .expect(&format!(
                "{:?} didn't get analyzed for termination",
                if_stmt.if_body
            ))
            != FunctionTermination::Terminates
        {
            self.builder.build_unconditional_branch(end_block)?;
        }

        self.builder.position_at_end(next_block);

        for (_token, condition, body) in &mut if_stmt.elifs {
            next_block.set_name("elif_condition");
            self.builder.position_at_end(next_block);

            let condition_value = self
                .visit_expression(condition)?
                .expect("Somehow passed a Unit value to an elif condition");
            let condition_value = self.builder.build_int_compare(
                IntPredicate::NE,
                condition_value.into_int_value(),
                condition_value.get_type().const_zero().into_int_value(),
                "if_condition",
            )?;

            let body_block = self
                .context
                .insert_basic_block_after(next_block, "elif_body");

            next_block = self
                .context
                .insert_basic_block_after(body_block, "elif_or_else");

            self.builder
                .build_conditional_branch(condition_value, body_block, next_block)?;

            self.builder.position_at_end(body_block);
            self.visit_statement(body)?;

            if *self
                .function_termination
                .get(body)
                .expect(&format!("{body:?} didn't get analyzed for termination"))
                != FunctionTermination::Terminates
            {
                self.builder.build_unconditional_branch(end_block)?;
            }
            self.builder.position_at_end(next_block);
        }

        if let Some((_token, else_body)) = &mut if_stmt.else_ {
            next_block.set_name("else_body");

            self.visit_statement(else_body)?;
            if *self.function_termination.get(&**else_body).expect(&format!(
                "{:?} didn't get analyzed for termination",
                else_body
            )) != FunctionTermination::Terminates
            {
                self.builder.build_unconditional_branch(end_block)?;
            }
        } else {
            next_block.replace_all_uses_with(&end_block);
            next_block
                .remove_from_function()
                .expect("any block produced for conditionals should be part of a function");
        }

        self.builder.position_at_end(end_block);

        Ok(())
    }

    fn visit_self_executor_host(
        &mut self,
        _executor_host: &mut SelfExecutorHost,
    ) -> Self::ExecutorHostOutput {
        todo!()
    }

    fn visit_thread_executor(&mut self, _executor: &mut ThreadExecutor) -> Self::ExecutorOutput {
        todo!()
    }

    fn visit_number_expression(
        &mut self,
        NumberExpression {
            value,
            token: _,
            id: _,
        }: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        return Ok(Some(
            self.context
                .i32_type()
                .const_int(*value as u64, false)
                .as_basic_value_enum(),
        ));
    }

    fn visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name,
            name_token,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let mut args: Vec<BasicMetadataValueEnum> = vec![];
        for arg in arguments {
            args.push(
                self.visit_expression(arg)?
                    .ok_or(format!(
                        "Somehow passed a Unit value as a function parameter near {:#?}",
                        name_token
                    ))?
                    .into(),
            );
        }

        if let Some(f) = self.functions.get(name) {
            return Ok(self
                .builder
                .build_call(*f, &args[..], name)?
                .try_as_basic_value()
                .left()
                .map(|l| l.as_basic_value_enum()));
        } else {
            self.errors.push(FleetError::from_token(
                name_token,
                format!("Function {name:?} called but not defined"),
                ErrorSeverity::Error,
            ));
            return Ok(Some(
                self.context
                    .i32_type()
                    .const_all_ones()
                    .as_basic_value_enum(),
            ));
        }
    }

    fn visit_grouping_expression(
        &mut self,
        GroupingExpression {
            open_paren_token: _,
            subexpression,
            close_paren_token: _,
            id: _,
        }: &mut GroupingExpression,
    ) -> Self::ExpressionOutput {
        self.visit_expression(subexpression)
    }

    fn visit_variable_access_expression(
        &mut self,
        VariableAccessExpression {
            name,
            name_token,
            id: _,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        if let Some((type_, ptr)) = self.variable_scopes.get(name) {
            return Ok(Some(self.builder.build_load(
                type_.clone(),
                ptr.clone(),
                name,
            )?));
        } else {
            self.errors.push(FleetError::from_token(
                name_token,
                format!("Variable {name:?} is accessed, but not defined"),
                ErrorSeverity::Error,
            ));
            return Ok(Some(
                self.context
                    .i32_type()
                    .const_all_ones()
                    .as_basic_value_enum(),
            ));
        }
    }

    fn visit_unary_expression(
        &mut self,
        UnaryExpression {
            operator_token: _,
            operation,
            operand,
            id: _,
        }: &mut UnaryExpression,
    ) -> Self::ExpressionOutput {
        let value = self.visit_expression(operand)?.ok_or(format!(
            "Somehow passed a Unit value as an operand to {:?}",
            operation
        ))?;

        return match operation {
            UnaryOperation::BitwiseNot => Ok(Some(
                self.builder
                    .build_not::<IntValue>(value.into_int_value(), "bitwise_not")?
                    .into(),
            )),
            UnaryOperation::LogicalNot => Ok(Some(
                self.builder
                    .build_int_z_extend(
                        self.builder.build_int_compare::<IntValue>(
                            IntPredicate::EQ,
                            value.into_int_value(),
                            value.get_type().into_int_type().const_zero(),
                            "logical_not",
                        )?,
                        value.get_type().into_int_type(),
                        "cast",
                    )?
                    .into(),
            )),
            UnaryOperation::Negate => Ok(Some(
                self.builder
                    .build_int_neg::<IntValue>(value.into_int_value(), "negate")?
                    .into(),
            )),
        };
    }

    fn visit_binary_expression(
        &mut self,
        BinaryExpression {
            left,
            operator_token: _,
            operation,
            right,
            id: _,
        }: &mut BinaryExpression,
    ) -> Self::ExpressionOutput {
        let left_value = self.visit_expression(left)?.ok_or(format!(
            "Somehow passed a Unit value as an operand to {:?}",
            operation
        ))?;
        let mut right_value_gen = |self_: &mut Self| -> Result<BasicValueEnum<'a>> {
            Ok(self_.visit_expression(right)?.ok_or(format!(
                "Somehow passed a Unit value as an operand to {:?}",
                operation
            ))?)
        };

        return match operation {
            BinaryOperation::Add => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_add::<IntValue>(
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "add",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::Subtract => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_sub(
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "sub",
                        )?
                        .into(),
                ))
            }

            BinaryOperation::Multiply => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_mul(
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "mul",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::Divide => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_signed_div(
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "div",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::Modulo => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_signed_rem(
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "mod",
                        )?
                        .into(),
                ))
            }

            BinaryOperation::LessThan => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_z_extend(
                            self.builder.build_int_compare::<IntValue>(
                                IntPredicate::SLT,
                                left_value.into_int_value(),
                                right_value.into_int_value(),
                                "less_than",
                            )?,
                            left_value.get_type().into_int_type(),
                            "extent_from_i1",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::LessThanOrEqual => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_z_extend(
                            self.builder.build_int_compare::<IntValue>(
                                IntPredicate::SLE,
                                left_value.into_int_value(),
                                right_value.into_int_value(),
                                "less_than_or_equal",
                            )?,
                            left_value.get_type().into_int_type(),
                            "extent_from_i1",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::GreaterThan => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_z_extend(
                            self.builder.build_int_compare::<IntValue>(
                                IntPredicate::SGT,
                                left_value.into_int_value(),
                                right_value.into_int_value(),
                                "greater_than",
                            )?,
                            left_value.get_type().into_int_type(),
                            "extent_from_i1",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::GreaterThanOrEqual => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_z_extend(
                            self.builder.build_int_compare::<IntValue>(
                                IntPredicate::SGE,
                                left_value.into_int_value(),
                                right_value.into_int_value(),
                                "greater_than_or_equal",
                            )?,
                            left_value.get_type().into_int_type(),
                            "extent_from_i1",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::Equal => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_z_extend(
                            self.builder.build_int_compare::<IntValue>(
                                IntPredicate::EQ,
                                left_value.into_int_value(),
                                right_value.into_int_value(),
                                "equal",
                            )?,
                            left_value.get_type().into_int_type(),
                            "extent_from_i1",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::NotEqual => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_z_extend(
                            self.builder.build_int_compare::<IntValue>(
                                IntPredicate::NE,
                                left_value.into_int_value(),
                                right_value.into_int_value(),
                                "not_equal",
                            )?,
                            left_value.get_type().into_int_type(),
                            "extent_from_i1",
                        )?
                        .into(),
                ))
            }

            BinaryOperation::LogicalAnd => {
                let current_block = self
                    .builder
                    .get_insert_block()
                    .expect("expected builder to be in a block");
                let right_block = self
                    .context
                    .insert_basic_block_after(current_block, "logical_and_rhs");
                let end_block = self
                    .context
                    .insert_basic_block_after(right_block, "logical_and_end");

                self.builder.position_at_end(current_block);
                self.builder.build_conditional_branch(
                    self.builder.build_int_compare::<IntValue>(
                        IntPredicate::EQ,
                        left_value.into_int_value(),
                        left_value.get_type().into_int_type().const_zero(),
                        "is_zero",
                    )?,
                    end_block,
                    right_block,
                )?;

                self.builder.position_at_end(right_block);
                let right_value = right_value_gen(self)?;
                let bool_right_value = self.builder.build_int_z_extend(
                    self.builder.build_int_compare(
                        IntPredicate::NE,
                        right_value.get_type().into_int_type().const_zero(),
                        right_value.into_int_value(),
                        "downcast_i1",
                    )?,
                    right_value.get_type().into_int_type(),
                    "upcast",
                )?;
                self.builder.build_unconditional_branch(end_block)?;
                // needed because right_value may have generated new basic blocks and
                // right_block isn't the branch source anymore
                let last_right_block = self
                    .builder
                    .get_insert_block()
                    .expect("expected builder to be in a block");

                self.builder.position_at_end(end_block);
                let result = self
                    .builder
                    .build_phi(left_value.get_type().into_int_type(), "logical_and_result")?;

                result.add_incoming(&[
                    (
                        &left_value.get_type().into_int_type().const_zero(),
                        current_block,
                    ),
                    (&bool_right_value, last_right_block),
                ]);

                Ok(Some(result.as_basic_value()))
            }
            BinaryOperation::LogicalOr => {
                let current_block = self
                    .builder
                    .get_insert_block()
                    .expect("expected builder to be in a block");
                let right_block = self
                    .context
                    .insert_basic_block_after(current_block, "logical_or_rhs");
                let end_block = self
                    .context
                    .insert_basic_block_after(right_block, "logical_or_end");

                self.builder.position_at_end(current_block);
                self.builder.build_conditional_branch(
                    self.builder.build_int_compare::<IntValue>(
                        IntPredicate::EQ,
                        left_value.into_int_value(),
                        left_value.get_type().into_int_type().const_zero(),
                        "is_zero",
                    )?,
                    right_block,
                    end_block,
                )?;

                self.builder.position_at_end(right_block);
                let right_value = right_value_gen(self)?;
                let bool_right_value = self.builder.build_int_z_extend(
                    self.builder.build_int_compare(
                        IntPredicate::NE,
                        right_value.get_type().into_int_type().const_zero(),
                        right_value.into_int_value(),
                        "downcast_i1",
                    )?,
                    right_value.get_type().into_int_type(),
                    "upcast",
                )?;
                self.builder.build_unconditional_branch(end_block)?;
                // needed because right_value may have generated new basic blocks and
                // right_block isn't the branch source anymore
                let last_right_block = self
                    .builder
                    .get_insert_block()
                    .expect("expected builder to be in a block");

                self.builder.position_at_end(end_block);
                let result = self
                    .builder
                    .build_phi(left_value.get_type().into_int_type(), "logical_or_result")?;

                result.add_incoming(&[
                    (
                        &left_value.get_type().into_int_type().const_int(1, false),
                        current_block,
                    ),
                    (&bool_right_value, last_right_block),
                ]);

                Ok(Some(result.as_basic_value()))
            }
        };
    }

    fn visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            name,
            name_token,
            equal_token: _,
            right,
            id: _,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let right_value = self.visit_expression(right)?.ok_or(format!(
            "Somehow tried assigning a Unit value to variable {name:?}",
        ))?;

        if let Some((_type, ptr)) = self.variable_scopes.get(name) {
            self.builder.build_store(ptr.clone(), right_value)?;
            return Ok(Some(right_value));
        } else {
            self.errors.push(FleetError::from_token(
                name_token,
                format!("Variable {name:?} is assigned, but not defined"),
                ErrorSeverity::Error,
            ));
            return Ok(Some(
                self.context
                    .i32_type()
                    .const_all_ones()
                    .as_basic_value_enum(),
            ));
        }
    }

    fn visit_i32_type(&mut self, _type: &mut I32Type) -> Self::TypeOutput {
        Ok(self.context.i32_type().into())
    }
}
