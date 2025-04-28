use std::{collections::HashMap, error::Error};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, FunctionType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue},
};

use crate::{
    ast::{Expression, FunctionDefinition, Program, Statement, Type},
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
                "LLVM module is invalid: {}",
                unescape(err.to_str().unwrap())
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
        eprintln!("Generating statement {:#?}", stmt);
        match stmt {
            Statement::Expression(expression) => {
                self.generate_expression_ir(expression)?;
                Ok(())
            }
            Statement::On {
                on_token: _,
                executor: _,
                body: _,
            } => todo!(),
            Statement::Block(statements) => {
                for substmt in statements {
                    self.generate_statement_ir(substmt)?;
                }
                Ok(())
            }
            Statement::Return {
                return_token: _,
                value,
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
        eprintln!("Generating expression {:#?}", expr);
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
                arguments,
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
        }
    }
}
