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
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNode, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, BoolExpression, BoolType,
        BreakStatement, CastExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionBody, FunctionCallExpression, FunctionDefinition, GroupingExpression,
        GroupingLValue, HasID, IdkType, IfStatement, IntType, NumberExpression, OnStatement,
        PerNodeData, Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SkipStatement,
        StatementFunctionBody, ThreadExecutor, UnaryExpression, UnaryOperation, UnitType,
        VariableAccessExpression, VariableAssignmentExpression, VariableDefinitionStatement,
        VariableLValue, WhileLoopStatement,
    },
    escape::unescape,
    infra::{ErrorSeverity, FleetError},
    passes::{
        function_termination_analysis::FunctionTermination,
        type_propagation::{
            Function, FunctionID, RuntimeType, TypeAnalysisData, UnionFindSet, UnionFindSetPtr,
            Variable, VariableID,
        },
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
impl DerefMut for VariableStorage<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

macro_rules! match_any_type_enum_as_basic_type {
    ($enum_val:expr, $inner_var:pat => $code:block, function($func_id:pat) => $func:block, unit($unit_id:pat) => $unit:block) => {
        match $enum_val {
            AnyTypeEnum::ArrayType($inner_var) => $code,
            AnyTypeEnum::FloatType($inner_var) => $code,
            AnyTypeEnum::FunctionType($func_id) => $func,
            AnyTypeEnum::IntType($inner_var) => $code,
            AnyTypeEnum::PointerType($inner_var) => $code,
            AnyTypeEnum::StructType($inner_var) => $code,
            AnyTypeEnum::VectorType($inner_var) => $code,
            AnyTypeEnum::VoidType($unit_id) => $unit,
        }
    };
}

pub struct IrGenerator<'a, 'errors, 'inputs> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    errors: &'errors mut Vec<FleetError>,

    function_termination: &'inputs PerNodeData<FunctionTermination>,
    variable_storage: HashMap<VariableID, VariableStorage<'a>>,
    function_locations: HashMap<FunctionID, FunctionLocation<'a>>,

    variable_data: &'inputs PerNodeData<Rc<RefCell<Variable>>>,
    function_data: &'inputs PerNodeData<Rc<RefCell<Function>>>,
    type_data: &'inputs PerNodeData<UnionFindSetPtr>,
    type_sets: &'inputs UnionFindSet<RuntimeType>,

    break_block: Option<BasicBlock<'a>>,
    skip_block: Option<BasicBlock<'a>>,
}
impl<'a, 'errors, 'inputs> IrGenerator<'a, 'errors, 'inputs> {
    pub fn new(
        context: &'a Context,
        errors: &'errors mut Vec<FleetError>,
        function_termination: &'inputs PerNodeData<FunctionTermination>,
        analysis_data: &'inputs TypeAnalysisData,
    ) -> Self {
        let module = context.create_module("module");
        let builder = context.create_builder();

        let TypeAnalysisData {
            type_data,
            type_sets,
            variable_data,
            function_data,
        } = &analysis_data;

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
            type_data,
            type_sets,
            break_block: None,
            skip_block: None,
        }
    }

    fn report_error<T>(&mut self, error: FleetError) -> Result<T> {
        self.errors.push(error.clone());
        Err(error.message.into())
    }

    fn make_into_function_type<'c>(
        &self,
        type_: AnyTypeEnum<'c>,
        param_types: &[BasicMetadataTypeEnum<'c>],
        is_var_args: bool,
    ) -> Result<FunctionType<'c>> {
        match_any_type_enum_as_basic_type!(
            type_,
            type_ => {
                Ok(type_.fn_type(param_types, is_var_args))
            },
            function(_) => {
                Err("Tried to make a function type into a function type again".into())
            },
            unit(void_type) => {
                Ok(void_type.fn_type(param_types, is_var_args))
            }
        )
    }

    fn runtime_type_to_llvm(
        &mut self,
        type_: RuntimeType,
        error_node: impl Into<AstNode> + Clone,
    ) -> Result<AnyTypeEnum<'a>> {
        Ok(match type_ {
            RuntimeType::I8 => self.context.i8_type().into(),
            RuntimeType::I16 => self.context.i16_type().into(),
            RuntimeType::I32 => self.context.i32_type().into(),
            RuntimeType::I64 => self.context.i64_type().into(),
            RuntimeType::UnsizedInt => {
                return self.report_error(FleetError::from_node(
                    error_node,
                    "Unsized ints should have caused errors earlier",
                    ErrorSeverity::Error,
                ));
            }
            RuntimeType::Unit => self.context.void_type().into(),
            RuntimeType::Boolean => self.context.bool_type().into(),
            RuntimeType::Unknown => {
                return self.report_error(FleetError::from_node(
                    error_node,
                    "Unknown types should have caused errors earlier",
                    ErrorSeverity::Error,
                ));
            }
            RuntimeType::Error => {
                return self.report_error(FleetError::from_node(
                    error_node,
                    "Error types should have caused errors earlier",
                    ErrorSeverity::Error,
                ));
            }
            RuntimeType::ArrayOf {
                subtype: inner_type,
                size,
            } => {
                let inner_type_ir =
                    self.runtime_type_to_llvm(*self.type_sets.get(inner_type), error_node.clone())?;

                let Some(size) = size else {
                    return self.report_error(FleetError::from_node(
                        error_node,
                        "Array doesn't have a known size",
                        ErrorSeverity::Error,
                    ));
                };

                match_any_type_enum_as_basic_type!(
                    inner_type_ir,
                    type_ => {
                        type_.array_type(size as u32)
                    },
                    function(_) => {
                        return self.report_error(FleetError::from_node(
                            error_node,
                            "Cannot have array of functions",
                            ErrorSeverity::Error,
                        ));
                    },
                    unit(_) => {
                        return self.report_error(FleetError::from_node(
                            error_node,
                            "Cannot have array of Unit",
                            ErrorSeverity::Error,
                        ));
                    }
                )
                .into()
            }
        })
    }

    fn register_function(&mut self, function: &mut FunctionDefinition) -> Result<()> {
        let return_type_ir = function
            .return_type
            .as_mut()
            .map(|t| self.visit_type(t))
            .unwrap_or_else(|| {
                let inferred_type = self
                    .function_data
                    .get(&function.get_id())
                    .expect("Function data should exist before calling ir_generator")
                    .borrow()
                    .return_type;
                self.runtime_type_to_llvm(*self.type_sets.get(inferred_type), function.clone())
            })?;

        let name = match &function.body {
            FunctionBody::Extern(ExternFunctionBody {
                at_token: _,
                extern_token: _,
                symbol,
                symbol_token: _,
                semicolon_token: _,
                id: _,
            }) => symbol.clone(),
            FunctionBody::Statement(_) => function.name.clone(),
        };

        let params = function
            .parameters
            .iter()
            .map(|(param, _comma)| {
                (
                    param.clone(),
                    self.variable_data
                        .get(&param.id)
                        .expect("parameters should have variables assigned by now")
                        .clone(),
                )
            })
            .collect_vec()
            .iter()
            .flat_map(|(param, var)| -> Result<_> {
                Ok(match_any_type_enum_as_basic_type!(
                    self.runtime_type_to_llvm(*self.type_sets.get(var.borrow().type_), param.clone())?,
                    type_ => {
                        type_.into()
                    },
                    function(_) => {
                        return self.report_error(FleetError::from_node(
                            param.clone(),
                            "cannot have function types as parameter",
                            ErrorSeverity::Error,
                        ));
                    },
                    unit(_) => {
                        return self.report_error(FleetError::from_node(
                            param.clone(),
                            "cannot have unit type as parameter",
                            ErrorSeverity::Error,
                        ));
                    }
                ))
            })
            .collect_vec();

        let ir_function = self.module.add_function(
            &name,
            self.make_into_function_type(return_type_ir, params.as_slice(), false)?,
            None,
        );
        let ref_function = self
            .function_data
            .get(&function.id)
            .expect("Function data should exist before calling ir_generator");

        assert!(
            self.function_locations
                .insert(ref_function.borrow().id, FunctionLocation(ir_function))
                .is_none()
        );
        Ok(())
    }
}

impl<'a> AstVisitor for IrGenerator<'a, '_, '_> {
    type ProgramOutput = Result<Module<'a>>;
    type FunctionDefinitionOutput = Result<FunctionValue<'a>>;
    type FunctionBodyOutput = Result<()>;
    type SimpleBindingOutput = Result<PointerValue<'a>>;
    type StatementOutput = Result<()>;
    type ExecutorHostOutput = Result<()>;
    type ExecutorOutput = Result<()>;
    type ExpressionOutput = Result<Option<BasicValueEnum<'a>>>;
    type LValueOutput = Result<PointerValue<'a>>;
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
                    .first()
                    .map_or(SourceLocation::start(), |f| f.close_paren_token.end),
                message: format!(
                    "LLVM module is invalid: {}\nModule dump:\n{}",
                    unescape(err.to_str().unwrap()),
                    self.module.print_to_string().to_str().unwrap()
                ),
                severity: ErrorSeverity::Error,
            });
        }

        Ok(self.module)
    }

    fn visit_function_definition(
        &mut self,
        function: &mut FunctionDefinition,
    ) -> Self::FunctionDefinitionOutput {
        eprintln!("Generating function {:?}", function.name);

        let return_type_ir = function
            .return_type
            .as_mut()
            .map(|t| self.visit_type(t))
            .unwrap_or_else(|| {
                let inferred_type = self
                    .function_data
                    .get(&function.get_id())
                    .expect("Function data should exist before calling ir_generator")
                    .borrow()
                    .return_type;
                self.runtime_type_to_llvm(*self.type_sets.get(inferred_type), function.clone())
            })?;
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

        self.visit_function_body(&mut function.body)?;

        if *self
            .function_termination
            .get_node(&function.body)
            .expect("all function bodies should have been analyzed for termination")
            == FunctionTermination::Terminates
        {
            for block in ir_function.get_basic_blocks() {
                if block.get_first_use().is_none() && block != entry {
                    block
                        .remove_from_function()
                        .map_err(|_| "Removing unreachable block from a function failed")?;
                    continue;
                }

                if block.get_terminator().is_none() {
                    self.builder.position_at_end(block);
                    match_any_type_enum_as_basic_type!(
                        return_type_ir,
                        type_ => {
                            self.builder
                                .build_return(Some(&type_.const_zero()))?;
                        },
                        function(_) => {
                            todo!();
                        },
                        unit(_) => {
                            self.builder.build_return(None)?;
                        }
                    );
                }
            }
        } else if matches!(return_type_ir, AnyTypeEnum::VoidType(_)) {
            for block in ir_function.get_basic_blocks() {
                if block.get_terminator().is_none() {
                    self.builder.position_at_end(block);
                    self.builder.build_return(None)?;
                }
            }
        } else {
            match &function.body {
                FunctionBody::Extern(_) => {}
                FunctionBody::Statement(_) => {
                    self.errors.push(FleetError::from_node(
                        function.body.clone(),
                        "All code paths must return.",
                        ErrorSeverity::Error,
                    ));
                }
            };
        }

        match &function.body {
            FunctionBody::Extern(ExternFunctionBody { .. }) => {
                for block in ir_function.get_basic_block_iter() {
                    block.remove_from_function().unwrap();
                }
            }
            FunctionBody::Statement(_) => {}
        };

        if !ir_function.verify(false) {
            self.errors.push(FleetError::from_node(
                function.clone(),
                "The IR generated for this function is invalid. See the ".to_string()
                    + "global error at the start of the file for more info",
                ErrorSeverity::Error,
            ));
        }

        Ok(ir_function)
    }

    fn visit_function_body(
        &mut self,
        function_body: &mut FunctionBody,
    ) -> Self::FunctionBodyOutput {
        match function_body {
            FunctionBody::Statement(statement_function_body) => {
                self.visit_statement_function_body(statement_function_body)
            }
            FunctionBody::Extern(extern_function_body) => {
                self.visit_extern_function_body(extern_function_body)
            }
        }
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.visit_statement(statement)?;
        Ok(())
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
        Ok(())
    }

    fn visit_simple_binding(
        &mut self,
        simple_binding: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let type_ = simple_binding
            .type_
            .as_mut()
            .map(|(_colon, type_)| self.visit_type(type_))
            .unwrap_or_else(|| {
                let inferred_type = self
                    .variable_data
                    .get(&simple_binding.id)
                    .expect("variable data should exist before calling ir_generator")
                    .borrow()
                    .type_;
                self.runtime_type_to_llvm(
                    *self.type_sets.get(inferred_type),
                    simple_binding.clone(),
                )
            })?;

        let ptr = match_any_type_enum_as_basic_type!(
            type_,
            type_ => {
                self
                    .builder
                    .build_alloca(type_, &simple_binding.name)?
            },
            function(_) => {
                return self.report_error(FleetError::from_node(
                    simple_binding.clone(),
                    "Somehow tried to bind a function type",
                    ErrorSeverity::Error,
                ));
            },
            unit(_) => {
                return self.report_error(FleetError::from_node(
                    simple_binding.clone(),
                    "Somehow tried to bind a void type",
                    ErrorSeverity::Error,
                ));
            }
        );

        let ref_variable = self
            .variable_data
            .get(&simple_binding.id)
            .unwrap_or_else(|| {
                panic!(
                    "Variable data for {:?} should exist before calling ir_generator",
                    simple_binding.name
                )
            })
            .borrow();

        assert!(
            self.variable_storage
                .insert(ref_variable.id, VariableStorage(ptr))
                .is_none(),
            "Variable {:?} was already assigned some storage",
            simple_binding.name
        );

        Ok(ptr)
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
        if let Some(retvalue) = &mut return_stmt.value {
            let ir_value = self.visit_expression(retvalue)?;

            self.builder.build_return(match &ir_value {
                Some(BasicValueEnum::ArrayValue(array_value)) => Some(array_value),
                Some(BasicValueEnum::IntValue(int_value)) => Some(int_value),
                Some(BasicValueEnum::FloatValue(float_value)) => Some(float_value),
                Some(BasicValueEnum::PointerValue(pointer_value)) => Some(pointer_value),
                Some(BasicValueEnum::StructValue(struct_value)) => Some(struct_value),
                Some(BasicValueEnum::VectorValue(vector_value)) => Some(vector_value),
                None => return Err("Unit functions should not return an expression".into()),
            })?;
        } else {
            self.builder.build_return(None)?;
        }
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
            .ok_or("Somehow passed using a void value as a variable initializer".to_string())?;

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
        let condition_value = condition_value.into_int_value();

        self.builder
            .build_conditional_branch(condition_value, body_block, next_block)?;

        self.builder.position_at_end(body_block);
        self.visit_statement(&mut if_stmt.if_body)?;

        if *self
            .function_termination
            .get_node(&*if_stmt.if_body)
            .unwrap_or_else(|| panic!("{:?} didn't get analyzed for termination", if_stmt.if_body))
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
            let condition_value = condition_value.into_int_value();

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
                .unwrap_or_else(|| panic!("{body:?} didn't get analyzed for termination"))
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
                .unwrap_or_else(|| panic!("{:?} didn't get analyzed for termination", else_body))
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

        self.builder.build_conditional_branch(
            cond_value.into_int_value(),
            body_block,
            end_block,
        )?;

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

        Ok(())
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

        self.builder.build_conditional_branch(
            cond_value.into_int_value(),
            body_block,
            end_block,
        )?;

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

        Ok(())
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

    fn visit_number_expression(&mut self, number: &mut NumberExpression) -> Self::ExpressionOutput {
        let number_clone = number.clone();
        let NumberExpression {
            value,
            token: _,
            id,
        } = number;
        let type_ = *self
            .type_data
            .get(id)
            .expect("type data should exist before running ir_generator");

        return Ok(Some(
            self.runtime_type_to_llvm(*self.type_sets.get(type_), number_clone)?
                .into_int_type()
                .const_int(*value as u64, false)
                .as_basic_value_enum(),
        ));
    }

    fn visit_bool_expression(
        &mut self,
        BoolExpression {
            value,
            token: _,
            id: _,
        }: &mut BoolExpression,
    ) -> Self::ExpressionOutput {
        Ok(Some(
            self.context
                .bool_type()
                .const_int(if *value { 1 } else { 0 }, false)
                .as_basic_value_enum(),
        ))
    }

    fn visit_array_expression(
        &mut self,
        expression: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        let expression_clone = expression.clone();
        let ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id,
        } = expression;

        let type_ @ RuntimeType::ArrayOf { .. } = self.type_sets.get(
            *self
                .type_data
                .get(id)
                .expect("type data should exist before running ir_generator"),
        ) else {
            return self.report_error(FleetError::from_node(
                expression_clone.clone(),
                "This array doesn't have an ArrayOf(_) type",
                ErrorSeverity::Error,
            ));
        };

        let mut array_ir = self
            .runtime_type_to_llvm(*type_, expression_clone)?
            .into_array_type()
            .get_undef();

        for (i, (item, _comma)) in elements.iter_mut().enumerate() {
            let item_ir = self
                .visit_expression(item)?
                .expect("Cannot have array of Unit");
            array_ir = self
                .builder
                .build_insert_value(array_ir, item_ir, i as u32, "init_array")?
                .into_array_value();
        }

        Ok(Some(array_ir.into()))
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

        Ok(self
            .builder
            .build_call(ir_function, &args[..], name)?
            .try_as_basic_value()
            .left()
            .map(|l| l.as_basic_value_enum()))
    }

    fn visit_array_index_expression(
        &mut self,
        ArrayIndexExpression {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayIndexExpression,
    ) -> Self::ExpressionOutput {
        let array_ir = self
            .visit_expression(array)?
            .expect("Cannot index into unit")
            .into_array_value();
        let index_ir = self
            .visit_expression(index)?
            .expect("Cannot index with unit")
            .into_int_value();

        let array_ptr = self
            .builder
            .build_alloca(array_ir.get_type(), "array_alloca")?;

        self.builder.build_store(array_ptr, array_ir)?;

        let index_ptr = unsafe {
            self.builder.build_gep(
                array_ir.get_type().get_element_type(),
                array_ptr,
                &[index_ir],
                "array_index",
            )
        }?;

        Ok(Some(self.builder.build_load(
            array_ir.get_type().get_element_type(),
            index_ptr,
            "load_from_array",
        )?))
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
        expr: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let expr_clone = expr.clone();
        let VariableAccessExpression {
            name,
            name_token: _,
            id,
        } = expr;

        let var = self
            .variable_data
            .get(id)
            .unwrap_or_else(|| {
                panic!("Variable data for {name:?} should exist before running ir_generator")
            })
            .clone();

        let Some(storage) = self.variable_storage.get(&var.borrow().id) else {
            return self.report_error(FleetError::from_node(
                expr_clone,
                format!(
                    "Variables should have storage before being accessed.\nVarData: {:#?}\nVarStorage: {:#?}\nThis ID: {id:?}",
                    self.variable_data, self.variable_storage
                ),
                ErrorSeverity::Error,
            ));
        };
        let storage = storage.clone();
        return match_any_type_enum_as_basic_type!(
            self.runtime_type_to_llvm(*self.type_sets.get(var.borrow().type_), expr_clone.clone())?,
            type_ => {
                Ok(Some(self.builder.build_load(type_, storage.0, name)?))
            },
            function(_) => {
                return self.report_error(FleetError::from_node(
                    expr_clone.clone(),
                    "Cannot load function",
                    ErrorSeverity::Error,
                ));
            },
            unit(_) => {
                return self.report_error(FleetError::from_node(
                    expr_clone.clone(),
                    "Cannot load Unit value",
                    ErrorSeverity::Error,
                ));
            }
        );
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

        match operation {
            UnaryOperation::BitwiseNot => Ok(Some(
                self.builder
                    .build_not::<IntValue>(value.into_int_value(), "bitwise_not")?
                    .into(),
            )),
            UnaryOperation::LogicalNot => Ok(Some(
                self.builder
                    .build_int_compare::<IntValue>(
                        IntPredicate::EQ,
                        value.into_int_value(),
                        value.into_int_value().get_type().const_zero(),
                        "logical_not",
                    )?
                    .into(),
            )),
            UnaryOperation::Negate => Ok(Some(
                self.builder
                    .build_int_neg::<IntValue>(value.into_int_value(), "negate")?
                    .into(),
            )),
        }
    }

    fn visit_cast_expression(
        &mut self,
        CastExpression {
            operand,
            as_token: _,
            type_,
            id,
        }: &mut CastExpression,
    ) -> Self::ExpressionOutput {
        let value = self.visit_expression(&mut *operand)?;
        let value_type = *self
            .type_data
            .get_node(&**operand)
            .expect("type data should exist before calling ir_generator");

        let target_type = *self
            .type_data
            .get(id)
            .expect("type data should exist before calling ir_generator");

        match *self.type_sets.get(target_type) {
            RuntimeType::I8 | RuntimeType::I16 | RuntimeType::I32 | RuntimeType::I64 => {
                let expected_type = self.visit_type(type_)?.into_int_type();
                let value = value
                    .expect("Somehow tried to cast Unit to bool")
                    .into_int_value();

                if expected_type.get_bit_width() <= value.get_type().get_bit_width() {
                    Ok(Some(
                        self.builder
                            .build_int_truncate_or_bit_cast(
                                value,
                                expected_type,
                                &format!("as_{target_type:?}"),
                            )?
                            .into(),
                    ))
                } else if self.type_sets.get(value_type).is_boolean() {
                    Ok(Some(
                        self.builder
                            .build_int_z_extend(
                                value,
                                expected_type,
                                &format!("as_{target_type:?}"),
                            )?
                            .into(),
                    ))
                } else {
                    Ok(Some(
                        self.builder
                            .build_int_s_extend(
                                value,
                                expected_type,
                                &format!("as_{target_type:?}"),
                            )?
                            .into(),
                    ))
                }
            }
            RuntimeType::ArrayOf {
                subtype: _,
                size: _,
            } => self.report_error(FleetError::from_node(
                type_.clone(),
                "cannot cast to array currently",
                ErrorSeverity::Error,
            )),
            RuntimeType::Boolean => {
                let expected_type = self.visit_type(type_)?.into_int_type();
                let value = value
                    .expect("Somehow tried to cast Unit to bool")
                    .into_int_value();

                Ok(Some(
                    self.builder
                        .build_int_truncate_or_bit_cast(value, expected_type, "as_bool")?
                        .into(),
                ))
            }
            RuntimeType::Unit => Ok(None),
            RuntimeType::Unknown => self.report_error(FleetError::from_node(
                type_.clone(),
                "ir_generator shouldn't run if there are unknown types",
                ErrorSeverity::Error,
            )),
            RuntimeType::Error => self.report_error(FleetError::from_node(
                type_.clone(),
                "ir_generator shouldn't run if there are error types",
                ErrorSeverity::Error,
            )),
            RuntimeType::UnsizedInt => self.report_error(FleetError::from_node(
                type_.clone(),
                "ir_generator shouldn't run if there are unsized int types",
                ErrorSeverity::Error,
            )),
        }
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

        match operation {
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
                        .build_int_compare::<IntValue>(
                            IntPredicate::SLT,
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "less_than",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::LessThanOrEqual => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::SLE,
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "less_than_or_equal",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::GreaterThan => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::SGT,
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "greater_than",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::GreaterThanOrEqual => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::SGE,
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "greater_than_or_equal",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::Equal => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::EQ,
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "equal",
                        )?
                        .into(),
                ))
            }
            BinaryOperation::NotEqual => {
                let right_value = right_value_gen(self)?;
                Ok(Some(
                    self.builder
                        .build_int_compare::<IntValue>(
                            IntPredicate::NE,
                            left_value.into_int_value(),
                            right_value.into_int_value(),
                            "not_equal",
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
                    left_value.into_int_value(),
                    right_block,
                    end_block,
                )?;

                self.builder.position_at_end(right_block);
                let right_value = right_value_gen(self)?;
                let bool_right_value = right_value.into_int_value();
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
                    left_value.into_int_value(),
                    end_block,
                    right_block,
                )?;

                self.builder.position_at_end(right_block);
                let right_value = right_value_gen(self)?;
                let bool_right_value = right_value.into_int_value();
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
        }
    }

    fn visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id: _,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let storage = self.visit_lvalue(lvalue)?;
        let right_value = self
            .visit_expression(right)?
            .ok_or("Somehow tried assigning a Unit value to an lvalue")?;

        self.builder.build_store(storage, right_value)?;
        Ok(Some(right_value))
    }

    fn visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name,
            name_token: _,
            id,
        }: &mut VariableLValue,
    ) -> Self::LValueOutput {
        let var = self.variable_data.get(id).unwrap_or_else(|| {
            panic!("Variable data for {name:?} should exist before calling ir_generator")
        });
        let storage = self
            .variable_storage
            .get(&var.borrow().id)
            .expect("Variables should have storage before being accessed")
            .0;
        Ok(storage)
    }

    fn visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id,
        }: &mut ArrayIndexLValue,
    ) -> Self::LValueOutput {
        let array_ir = self.visit_lvalue(array)?;
        let index_ir = self
            .visit_expression(index)?
            .expect("Cannot index with unit")
            .into_int_value();

        let infered_type = *self
            .type_data
            .get(id)
            .expect("type data should exist before calling ir_generator");

        let element_type =
            self.runtime_type_to_llvm(*self.type_sets.get(infered_type), *array.clone())?;

        let index_ptr = match_any_type_enum_as_basic_type!(
            element_type,
            type_ => {
                unsafe {
                    self.builder.build_gep(type_, array_ir, &[index_ir], "array_index_lvalue")
                }?
            },
            function(_) => {
                return self.report_error(FleetError::from_node(
                    *array.clone(),
                    "Cannot have array of functions",
                    ErrorSeverity::Error,
                ));
            },
            unit(_) => {
                return self.report_error(FleetError::from_node(
                    *array.clone(),
                    "Cannot have array of Unit",
                    ErrorSeverity::Error,
                ));
            }
        );

        Ok(index_ptr)
    }

    fn visit_grouping_lvalue(
        &mut self,
        GroupingLValue {
            open_paren_token: _,
            sublvalue,
            close_paren_token: _,
            id: _,
        }: &mut GroupingLValue,
    ) -> Self::LValueOutput {
        self.visit_lvalue(sublvalue)
    }

    fn visit_int_type(&mut self, type_: &mut IntType) -> Self::TypeOutput {
        let type_clone = type_.clone();
        let IntType {
            token: _,
            type_,
            id,
        } = type_;
        let infered_type = self
            .type_data
            .get(id)
            .expect("type data should exist before calling ir_generator");

        assert_eq!(
            *type_,
            *self.type_sets.get(*infered_type),
            "Inferred type (right) of IntType doesn't match its actual type (left)"
        );
        self.runtime_type_to_llvm(*type_, type_clone)
    }

    fn visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id,
        }: &mut UnitType,
    ) -> Self::TypeOutput {
        let infered_type = self
            .type_data
            .get(id)
            .expect("type data should exist before calling ir_generator");

        assert_eq!(
            RuntimeType::Unit,
            *self.type_sets.get(*infered_type),
            "Inferred type (right) of UnitType doesn't match its actual type (left)"
        );
        Ok(self.context.void_type().into())
    }

    fn visit_bool_type(&mut self, BoolType { token: _, id }: &mut BoolType) -> Self::TypeOutput {
        let infered_type = self
            .type_data
            .get(id)
            .expect("type data should exist before calling ir_generator");

        assert_eq!(
            RuntimeType::Boolean,
            *self.type_sets.get(*infered_type),
            "Inferred type (right) of BoolType doesn't match its actual type (left)"
        );
        Ok(self.context.bool_type().into())
    }

    fn visit_idk_type(&mut self, type_: &mut IdkType) -> Self::TypeOutput {
        let type_clone = type_.clone();
        let IdkType { token: _, id } = type_;
        let infered_type = *self
            .type_data
            .get(id)
            .expect("type data should exist before calling ir_generator");

        self.runtime_type_to_llvm(*self.type_sets.get(infered_type), type_clone)
    }

    fn visit_array_type(&mut self, array_type: &mut ArrayType) -> Self::TypeOutput {
        let array_type_clone = array_type.clone();
        let ArrayType {
            subtype: _,
            open_bracket_token: _,
            size: _,
            close_bracket_token: _,
            id,
        } = array_type;

        let infered_type = *self
            .type_data
            .get(id)
            .expect("type data should exist before calling ir_generator");

        self.runtime_type_to_llvm(*self.type_sets.get(infered_type), array_type_clone)
    }
}
