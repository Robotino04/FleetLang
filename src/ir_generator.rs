use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
    },
};

use crate::ast::{Expression, Function, Program, Statement, TopLevelStatement};

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

        let functions = program
            .toplevel_statements
            .iter()
            .filter_map(|tls| {
                if let TopLevelStatement::FunctionDefinition(f) = tls {
                    Some(f)
                } else {
                    None
                }
            })
            .map(|f| self.generate_function_ir(f))
            .collect::<Vec<_>>();

        let main_function = Function {
            name: "main".to_string(),
            name_token: None,
            body: Statement::Block(
                program
                    .toplevel_statements
                    .iter()
                    .filter_map(|tls| match tls {
                        TopLevelStatement::LooseStatement(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect(),
            ),
        };
        let main_function_code = self.generate_function_ir(&main_function);

        return &self.module;
    }

    fn generate_function_ir(&mut self, function: &Function) -> FunctionValue<'a> {
        eprintln!("Generating function {:?}", function.name);
        let ir_function = self.module.add_function(
            &function.name,
            self.context.void_type().fn_type(&[], false),
            None,
        );

        let entry = self.context.append_basic_block(ir_function, "entry");
        self.builder.position_at_end(entry);

        self.generate_statement_ir(&function.body);

        self.builder.build_return(None).unwrap();

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
                executor,
                body,
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
