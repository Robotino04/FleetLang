use std::{collections::HashMap, error::Error};

use inkwell::{
    IntPredicate,
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, FunctionType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue},
};

use crate::{
    ast::{
        BinaryOperation, Expression, FunctionDefinition, Program, Statement, Type, UnaryOperation,
    },
    escape::unescape,
};

type Result<T> = ::core::result::Result<T, Box<dyn Error>>;

pub struct IrGenerator<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    functions: HashMap<String, FunctionValue<'a>>,
}
impl<'a> IrGenerator<'a> {
    pub fn new(context: &'a Context) -> Self {
        let module = context.create_module("module");
        let builder = context.create_builder();

        IrGenerator {
            context,
            module,
            builder,
            functions: HashMap::new(),
        }
    }

    pub fn generate_program_ir(&mut self, program: &Program) -> Result<&Module<'a>> {
        eprintln!("Generating program");

        for f in &program.functions {
            self.generate_function_ir(f)?;
        }

        self.module.verify().map_err(|err| {
            format!(
                "LLVM module is invalid: {}\nModule dump:\n{}",
                unescape(err.to_str().unwrap()),
                self.module.print_to_string().to_str().unwrap()
            )
        })?;

        return Ok(&self.module);
    }

    fn generate_type(&mut self, type_: &Type) -> AnyTypeEnum<'a> {
        match type_ {
            Type::I32 { token: _ } => self.context.i32_type().into(),
        }
    }

    fn make_into_function_type<'b>(
        &self,
        type_: AnyTypeEnum<'b>,
        param_types: &[BasicMetadataTypeEnum<'b>],
        is_var_args: bool,
    ) -> Result<FunctionType<'b>> {
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

    fn generate_function_ir(&mut self, function: &FunctionDefinition) -> Result<FunctionValue<'a>> {
        eprintln!("Generating function {:?}", function.name);

        let return_type_ir = self.generate_type(&function.return_type);
        let ir_function = self.module.add_function(
            &function.name,
            self.make_into_function_type(return_type_ir, &[], false)?,
            None,
        );

        let entry = self.context.append_basic_block(ir_function, "entry");
        self.builder.position_at_end(entry);

        self.generate_statement_ir(&function.body)?;

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

    fn generate_statement_ir(&mut self, stmt: &Statement) -> Result<()> {
        eprintln!("Generating statement");
        match stmt {
            Statement::Expression {
                expression,
                semicolon_token: _,
            } => {
                self.generate_expression_ir(expression)?;
                Ok(())
            }
            Statement::On {
                on_token: _,
                open_paren_token: _,
                executor: _,
                close_paren_token: _,
                body: _,
            } => todo!(),
            Statement::Block {
                open_brace_token: _,
                body,
                close_brace_token: _,
            } => {
                for substmt in body {
                    self.generate_statement_ir(substmt)?;
                }
                Ok(())
            }
            Statement::Return {
                return_token: _,
                value,
                semicolon_token: _,
            } => {
                let ir_value = self.generate_expression_ir(value)?;

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
        }
    }
    fn generate_expression_ir(&mut self, expr: &Expression) -> Result<Option<BasicValueEnum<'a>>> {
        eprintln!("Generating expression");
        match expr {
            Expression::Number { value, token: _ } => {
                return Ok(Some(
                    self.context
                        .i32_type()
                        .const_int(*value as u64, false)
                        .into(),
                ));
            }
            Expression::FunctionCall {
                name,
                name_token,
                open_paren_token: _,
                arguments,
                close_paren_token: _,
            } => {
                let mut args: Vec<BasicMetadataValueEnum> = vec![];
                for arg in arguments {
                    args.push(
                        self.generate_expression_ir(arg)?
                            .ok_or(format!(
                                "Somehow passed a Unit value as a function parameter near {:#?}",
                                name_token
                            ))?
                            .into(),
                    );
                }

                let f = self
                    .functions
                    .get(name)
                    .ok_or(format!("Function {name:?} isn't defined, but called"))?;

                return Ok(self
                    .builder
                    .build_call(*f, &args[..], name)?
                    .try_as_basic_value()
                    .left()
                    .map(|l| l.as_basic_value_enum()));
            }
            Expression::Unary {
                operation,
                operand,
                operator_token: _,
            } => {
                let value = self.generate_expression_ir(operand)?.ok_or(format!(
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
            Expression::Binary {
                left,
                operator_token: _,
                operation,
                right,
            } => {
                let left_value = self.generate_expression_ir(&left)?.ok_or(format!(
                    "Somehow passed a Unit value as an operand to {:?}",
                    operation
                ))?;
                let right_value_gen = |self_: &mut Self| -> Result<BasicValueEnum<'a>> {
                    Ok(self_.generate_expression_ir(&right)?.ok_or(format!(
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
                        let result = self.builder.build_phi(
                            left_value.get_type().into_int_type(),
                            "logical_and_result",
                        )?;

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
                        let result = self.builder.build_phi(
                            left_value.get_type().into_int_type(),
                            "logical_or_result",
                        )?;

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
            Expression::Grouping {
                open_paren_token: _,
                subexpression,
                close_paren_token: _,
            } => self.generate_expression_ir(subexpression),
        }
    }
}
