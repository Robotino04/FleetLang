use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, FunctionType},
    values::{BasicValue, BasicValueEnum, FunctionValue},
};

use crate::ast::{Expression, FunctionDefinition, Program, Statement, Type};

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

    pub fn generate_program_ir(&mut self, program: &Program) -> &Module<'a> {
        eprintln!("Generating program");
        let exit_fn = self.module.add_function(
            "exit",
            self.context
                .void_type()
                .fn_type(&[self.context.i32_type().into()], false),
            None,
        );
        self.functions.insert("exit".to_string(), exit_fn);

        for f in &program.functions {
            self.generate_function_ir(f);
        }

        return &self.module;
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
    ) -> FunctionType<'b> {
        match type_ {
            AnyTypeEnum::ArrayType(array_type) => array_type.fn_type(param_types, is_var_args),
            AnyTypeEnum::FloatType(float_type) => float_type.fn_type(param_types, is_var_args),
            AnyTypeEnum::FunctionType(_) => {
                panic!("Tried to make a function type into a function type again")
            }
            AnyTypeEnum::IntType(int_type) => int_type.fn_type(param_types, is_var_args),
            AnyTypeEnum::PointerType(pointer_type) => {
                pointer_type.fn_type(param_types, is_var_args)
            }
            AnyTypeEnum::StructType(struct_type) => struct_type.fn_type(param_types, is_var_args),
            AnyTypeEnum::VectorType(vector_type) => vector_type.fn_type(param_types, is_var_args),
            AnyTypeEnum::VoidType(void_type) => void_type.fn_type(param_types, is_var_args),
        }
    }

    fn generate_function_ir(&mut self, function: &FunctionDefinition) -> FunctionValue<'a> {
        eprintln!("Generating function {:?}", function.name);

        let return_type_ir = self.generate_type(&function.return_type);
        let ir_function = self.module.add_function(
            &function.name,
            self.make_into_function_type(return_type_ir, &[], false),
            None,
        );

        let entry = self.context.append_basic_block(ir_function, "entry");
        self.builder.position_at_end(entry);

        self.generate_statement_ir(&function.body);

        match function.return_type {
            Type::I32 { token: _ } => {
                self.builder
                    .build_return(Some(&self.context.i32_type().const_int(0, false)))
                    .unwrap();
            } // Type::Unit { token } => { self.builder.build_return(None).unwrap(); }
        }

        return ir_function;
    }

    fn generate_statement_ir(&mut self, stmt: &Statement) {
        eprintln!("Generating statement {:#?}", stmt);
        match stmt {
            Statement::Expression(expression) => {
                self.generate_expression_ir(expression);
            }
            Statement::On {
                on_token: _,
                executor: _,
                body: _,
            } => todo!(),
            Statement::Block(statements) => {
                for substmt in statements {
                    self.generate_statement_ir(substmt);
                }
            }
        }
    }
    fn generate_expression_ir(&mut self, expr: &Expression) -> Option<BasicValueEnum<'a>> {
        eprintln!("Generating expression {:#?}", expr);
        match expr {
            Expression::Number { value, token: _ } => {
                return Some(
                    self.context
                        .i64_type()
                        .const_int(*value as u64, false)
                        .into(),
                );
            }
            Expression::FunctionCall {
                name,
                name_token: _,
                arguments,
            } => {
                let args = arguments
                    .iter()
                    .map(|arg| self.generate_expression_ir(arg).unwrap().into())
                    .collect::<Vec<_>>();

                let f = self.functions.get(name).unwrap();

                return self
                    .builder
                    .build_call(*f, &args[..], name)
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .map(|l| l.as_basic_value_enum());
            }
        }
    }
}
