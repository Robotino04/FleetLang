use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use inkwell::{
    IntPredicate,
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, FunctionType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue,
    },
};
use itertools::Itertools;

use crate::{
    ast::{
        AstVisitor, BinaryExpression, BinaryOperation, BlockStatement, BreakStatement,
        ExpressionStatement, ForLoopStatement, FunctionCallExpression, FunctionDefinition,
        GroupingExpression, I32Type, IfStatement, NumberExpression, OnStatement, PerNodeData,
        Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SkipStatement, ThreadExecutor,
        UnaryExpression, UnaryOperation, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, WhileLoopStatement,
    },
    escape::unescape,
    infra::{ErrorSeverity, FleetError},
    passes::{
        function_termination_analysis::FunctionTermination,
        type_propagation::{Function, FunctionID, RuntimeType, Variable, VariableID},
    },
    tokenizer::SourceLocation,
};

type Result<T> = ::core::result::Result<T, Box<dyn Error>>;

#[derive(Clone, Debug)]
pub struct VariableStorage<'a>(PointerValue<'a>);

#[derive(Clone, Debug)]
pub struct FunctionLocation<'a>(FunctionValue<'a>);

impl<'a> Deref for VariableStorage<'a> {
    type Target = PointerValue<'a>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'a> DerefMut for VariableStorage<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct IrGenerator<'a, 'errors> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    errors: &'errors mut Vec<FleetError>,

    function_termination: PerNodeData<FunctionTermination>,
    variable_data: PerNodeData<Rc<RefCell<Variable>>>,
    variable_storage: HashMap<VariableID, VariableStorage<'a>>,
    function_data: PerNodeData<Rc<RefCell<Function>>>,
    function_locations: HashMap<FunctionID, FunctionLocation<'a>>,

    break_block: Option<BasicBlock<'a>>,
    skip_block: Option<BasicBlock<'a>>,
}
impl<'a, 'errors> IrGenerator<'a, 'errors> {
    pub fn new(
        context: &'a Context,
        errors: &'errors mut Vec<FleetError>,
        function_termination: PerNodeData<FunctionTermination>,
        variable_data: PerNodeData<Rc<RefCell<Variable>>>,
        function_data: PerNodeData<Rc<RefCell<Function>>>,
    ) -> Self {
        let module = context.create_module("module");
        let builder = context.create_builder();

        IrGenerator {
            context,
            module,
            builder,
            errors,
            function_termination,
            variable_data,
            variable_storage: HashMap::new(),
            function_data,
            function_locations: HashMap::new(),
            break_block: None,
            skip_block: None,
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

    fn runtime_type_to_llvm(&self, type_: RuntimeType) -> AnyTypeEnum<'a> {
        match type_ {
            RuntimeType::I32 => self.context.i32_type().into(),
            RuntimeType::Unit => self.context.void_type().into(),
            RuntimeType::Unknown => panic!("Unknown types should have caused errors earlier."),
        }
    }

    fn register_function(&mut self, function: &mut FunctionDefinition) -> Result<()> {
        let return_type_ir = self.visit_type(&mut function.return_type)?;
        let ir_function = self.module.add_function(
            &function.name,
            self.make_into_function_type(
                return_type_ir,
                function
                    .parameters
                    .iter()
                    .map(|(param, _comma)| {
                        self.variable_data
                            .get(&param.id)
                            .expect("parameters should have variables assigned by now")
                    })
                    .map(|var| match self.runtime_type_to_llvm(var.borrow().type_) {
                        AnyTypeEnum::ArrayType(array_type) => array_type.into(),
                        AnyTypeEnum::FloatType(float_type) => float_type.into(),
                        AnyTypeEnum::FunctionType(_function_type) => {
                            panic!("cannot have function types as parameter")
                        }
                        AnyTypeEnum::IntType(int_type) => int_type.into(),
                        AnyTypeEnum::PointerType(pointer_type) => pointer_type.into(),
                        AnyTypeEnum::StructType(struct_type) => struct_type.into(),
                        AnyTypeEnum::VectorType(vector_type) => vector_type.into(),
                        AnyTypeEnum::VoidType(_void_type) => {
                            panic!("cannot unit type as parameter")
                        }
                    })
                    .collect_vec()
                    .as_slice(),
                false,
            )?,
            None,
        );
        let ref_function = self
            .function_data
            .get(&function.id)
            .expect("Function data should exist before calling ir_generator");

        assert!(matches!(
            self.function_locations
                .insert(ref_function.borrow().id, FunctionLocation(ir_function)),
            None
        ));
        Ok(())
    }
}

impl<'a, 'errors> AstVisitor for IrGenerator<'a, 'errors> {
    type ProgramOutput = Result<Module<'a>>;
    type FunctionDefinitionOutput = Result<FunctionValue<'a>>;
    type SimpleBindingOutput = Result<PointerValue<'a>>;
    type StatementOutput = Result<()>;
    type ExecutorHostOutput = Result<()>;
    type ExecutorOutput = Result<()>;
    type ExpressionOutput = Result<Option<BasicValueEnum<'a>>>;

    type TypeOutput = Result<AnyTypeEnum<'a>>;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        eprintln!("Generating program");

        for f in &mut program.functions {
            self.register_function(f)?;
        }

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
        let ir_function = self
            .function_locations
            .get(
                &self
                    .function_data
                    .get(&function.id)
                    .expect("Function data should exist before calling ir_generator")
                    .borrow()
                    .id,
            )
            .expect("Functions should be registered before traversing the tree")
            .clone()
            .0;

        let entry = self.context.append_basic_block(ir_function, "entry");
        self.builder.position_at_end(entry);

        assert_eq!(
            ir_function.count_params() as usize,
            function.parameters.len(),
            "ir function has an incorrect number of parameters"
        );
        for ((param, _comma), ir_param) in function
            .parameters
            .iter_mut()
            .zip(ir_function.get_param_iter())
        {
            let ptr = self.visit_simple_binding(param)?;
            self.builder.build_store(ptr, ir_param)?;
        }

        self.visit_statement(&mut function.body)?;

        if *self
            .function_termination
            .get_node(&function.body)
            .expect("all function bodies should have been analyzed for termination")
            == FunctionTermination::Terminates
        {
            for block in ir_function.get_basic_blocks() {
                if block.get_terminator().is_none() {
                    self.builder.position_at_end(block);
                    self.builder.build_return(Some(
                        &return_type_ir.into_int_type().const_int(123, false),
                    ))?;
                }

                if block.get_first_use().is_none() && block != entry {
                    block
                        .remove_from_function()
                        .map_err(|_| "Removing unreachable block from a function failed")?;
                }
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

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let type_ = self.visit_type(&mut simple_binding.type_)?;

        let ptr = match type_ {
            AnyTypeEnum::ArrayType(array_type) => self
                .builder
                .build_alloca(array_type, &simple_binding.name)?,
            AnyTypeEnum::FloatType(float_type) => self
                .builder
                .build_alloca(float_type, &simple_binding.name)?,
            AnyTypeEnum::FunctionType(_function_type) => {
                panic!("Somehow tried to bind a function type")
            }
            AnyTypeEnum::IntType(int_type) => {
                self.builder.build_alloca(int_type, &simple_binding.name)?
            }
            AnyTypeEnum::PointerType(pointer_type) => self
                .builder
                .build_alloca(pointer_type, &simple_binding.name)?,
            AnyTypeEnum::StructType(struct_type) => self
                .builder
                .build_alloca(struct_type, &simple_binding.name)?,
            AnyTypeEnum::VectorType(vector_type) => self
                .builder
                .build_alloca(vector_type, &simple_binding.name)?,
            AnyTypeEnum::VoidType(_void_type) => {
                panic!("Somehow tried to bind a void type")
            }
        };

        let ref_variable = self
            .variable_data
            .get(&simple_binding.id)
            .expect(&format!(
                "Variable data for {:?} should exist before calling ir_generator",
                simple_binding.name,
            ))
            .borrow();

        assert!(
            matches!(
                self.variable_storage
                    .insert(ref_variable.id, VariableStorage(ptr)),
                None
            ),
            "Variable {:?} was already assigned some storage",
            simple_binding.name
        );

        return Ok(ptr);
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
        for substmt in body {
            self.visit_statement(substmt)?;
        }
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
        let next_block = self.context.insert_basic_block_after(
            self.builder
                .get_insert_block()
                .expect("Builder should always be in a block"),
            "after_return",
        );
        self.builder.position_at_end(next_block);
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

        let ptr = self.visit_simple_binding(&mut vardef_stmt.binding)?;
        self.builder.build_store(ptr, eval_value)?;

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
            .get_node(&*if_stmt.if_body)
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
                .get_node(body)
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
            if *self
                .function_termination
                .get_node(&**else_body)
                .expect(&format!(
                    "{:?} didn't get analyzed for termination",
                    else_body
                ))
                != FunctionTermination::Terminates
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

    fn visit_while_loop_statement(
        &mut self,
        WhileLoopStatement {
            while_token: _,
            condition,
            body,
            id: _,
        }: &mut WhileLoopStatement,
    ) -> Self::StatementOutput {
        let current_block = self
            .builder
            .get_insert_block()
            .expect("builder should always be in a function");
        let condition_block = self
            .context
            .insert_basic_block_after(current_block, "while_condition");
        let body_block = self
            .context
            .insert_basic_block_after(condition_block, "while_body");
        let end_block = self
            .context
            .insert_basic_block_after(body_block, "while_end");

        self.builder.build_unconditional_branch(condition_block)?;
        self.builder.position_at_end(condition_block);

        let cond_value = self
            .visit_expression(condition)?
            .expect("Somehow passed a Unit value as a while condition");

        let cond_booleanized = self.builder.build_int_compare(
            IntPredicate::EQ,
            cond_value.get_type().into_int_type().const_zero(),
            cond_value.into_int_value(),
            "while_condition",
        )?;

        self.builder
            .build_conditional_branch(cond_booleanized, end_block, body_block)?;

        self.builder.position_at_end(body_block);
        {
            let prev_skip = self.skip_block;
            let prev_break = self.break_block;

            self.skip_block = Some(condition_block);
            self.break_block = Some(end_block);

            self.visit_statement(body)?;

            self.skip_block = prev_skip;
            self.break_block = prev_break;
        }
        self.builder.build_unconditional_branch(condition_block)?;

        self.builder.position_at_end(end_block);

        return Ok(());
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
        let current_block = self
            .builder
            .get_insert_block()
            .expect("builder should always be in a function");
        let condition_block = self
            .context
            .insert_basic_block_after(current_block, "for_condition");
        let body_block = self
            .context
            .insert_basic_block_after(condition_block, "for_body");
        let incrementer_block = self
            .context
            .insert_basic_block_after(body_block, "for_incrementer");
        let end_block = self
            .context
            .insert_basic_block_after(incrementer_block, "for_end");

        self.visit_statement(initializer)?;

        self.builder.build_unconditional_branch(condition_block)?;
        self.builder.position_at_end(condition_block);

        let cond_value = condition
            .as_mut()
            .map(|cond| self.visit_expression(cond))
            .unwrap_or(Ok(Some(
                self.context
                    .i32_type()
                    .const_int(1, false)
                    .as_basic_value_enum(),
            )))?
            .expect("Somehow passed a Unit value as a while condition");

        let cond_booleanized = self.builder.build_int_compare(
            IntPredicate::EQ,
            cond_value.get_type().into_int_type().const_zero(),
            cond_value.into_int_value(),
            "for_condition",
        )?;

        self.builder
            .build_conditional_branch(cond_booleanized, end_block, body_block)?;

        self.builder.position_at_end(body_block);

        {
            let prev_skip = self.skip_block;
            let prev_break = self.break_block;

            self.skip_block = Some(incrementer_block);
            self.break_block = Some(end_block);

            self.visit_statement(body)?;

            self.skip_block = prev_skip;
            self.break_block = prev_break;
        }

        self.builder.build_unconditional_branch(incrementer_block)?;
        self.builder.position_at_end(incrementer_block);
        if let Some(inc) = incrementer {
            self.visit_expression(inc)?;
        }
        self.builder.build_unconditional_branch(condition_block)?;

        self.builder.position_at_end(end_block);

        return Ok(());
    }

    fn visit_break_statement(&mut self, break_stmt: &mut BreakStatement) -> Self::StatementOutput {
        if let Some(break_block) = self.break_block {
            self.builder.build_unconditional_branch(break_block)?;
            let next_block = self.context.insert_basic_block_after(
                self.builder
                    .get_insert_block()
                    .expect("Builder should always be in a block"),
                "after_break",
            );
            self.builder.position_at_end(next_block);
        } else {
            self.errors.push(FleetError::from_node(
                break_stmt.clone(),
                "Break statements can only appear inside of loops",
                ErrorSeverity::Error,
            ));
        }
        Ok(())
    }

    fn visit_skip_statement(&mut self, skip_stmt: &mut SkipStatement) -> Self::StatementOutput {
        if let Some(skip_block) = self.skip_block {
            self.builder.build_unconditional_branch(skip_block)?;
            let next_block = self.context.insert_basic_block_after(
                self.builder
                    .get_insert_block()
                    .expect("Builder should always be in a block"),
                "after_skip",
            );
            self.builder.position_at_end(next_block);
        } else {
            self.errors.push(FleetError::from_node(
                skip_stmt.clone(),
                "Skip statements can only appear inside of loops",
                ErrorSeverity::Error,
            ));
        }
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
            id,
        }: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let mut args: Vec<BasicMetadataValueEnum> = vec![];
        for (arg, _comma) in arguments {
            args.push(
                self.visit_expression(arg)?
                    .ok_or(format!(
                        "Somehow passed a Unit value as a function parameter near {:#?}",
                        name_token
                    ))?
                    .into(),
            );
        }

        let ir_function = self
            .function_locations
            .get(
                &self
                    .function_data
                    .get(id)
                    .expect("Function data should exist before calling ir_generator")
                    .borrow()
                    .id,
            )
            .expect("Functions should be registered before traversing the tree")
            .clone()
            .0;

        return Ok(self
            .builder
            .build_call(ir_function, &args[..], name)?
            .try_as_basic_value()
            .left()
            .map(|l| l.as_basic_value_enum()));
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
            name_token: _,
            id,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let var = self.variable_data.get(id).expect(&format!(
            "Variable data for {name:?} should exist before running ir_generator"
        ));

        let storage = self
            .variable_storage
            .get(&var.borrow().id)
            .unwrap_or_else(|| {
                panic!(
                    "Variables should have storage before being accessed.{}",
                    format!(
                        "\nVarData: {:#?}\nVarStorage: {:#?}\nThis ID: {id:?}",
                        self.variable_data, self.variable_storage
                    )
                )
            });
        return Ok(Some(match self.runtime_type_to_llvm(var.borrow().type_) {
            AnyTypeEnum::IntType(int_type) => {
                self.builder.build_load(int_type, storage.0.clone(), name)
            }
            _ => todo!(),
        }?));
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
            name_token: _,
            equal_token: _,
            right,
            id,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let right_value = self.visit_expression(right)?.ok_or(format!(
            "Somehow tried assigning a Unit value to variable {name:?}",
        ))?;

        let var = self.variable_data.get(id).expect(&format!(
            "Variable data for {name:?} should exist before calling ir_generator"
        ));
        let storage = self
            .variable_storage
            .get(&var.borrow().id)
            .expect("Variables should have storage before being accessed");

        self.builder.build_store(storage.0, right_value)?;
        return Ok(Some(right_value));
    }

    fn visit_i32_type(&mut self, _type: &mut I32Type) -> Self::TypeOutput {
        Ok(self.context.i32_type().into())
    }
}
