use std::cell::{Ref, RefCell, RefMut};

use indent::indent_all_by;
use indoc::formatdoc;

use itertools::Itertools;

use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, BreakStatement, CastExpression,
        CompilerExpression, Executor, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionCallExpression, FunctionDefinition, GPUExecutor, GroupingExpression,
        GroupingLValue, HasID, IdkType, IfStatement, LiteralExpression, LiteralKind, OnStatement,
        Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType, SkipStatement,
        StatementFunctionBody, StructAccessExpression, StructAccessLValue, StructExpression,
        StructMemberDefinition, StructMemberValue, StructType, ThreadExecutor, TopLevelStatement,
        TypeAlias, UnaryExpression, UnaryOperation, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    generate_glsl::GLSLCodeGenerator,
    infra::{Backend, ErrorKind, ErrorSeverity, Intrinsic, UnresolvedSymbol},
    passes::{
        pass_manager::{
            CCodeOutput, ConcreteFunctionData, ConcreteScopeData, ConcreteTypeData,
            ConcreteVariableData, Errors, GlobalState, Pass, PassError, PassFactory, PassResult,
            PrecompiledGlslFunctions, StatData, StructAliasMap,
        },
        runtime_type::ConcreteRuntimeType,
        scope_analysis::{ConcreteFunction, ConcreteVariable},
        top_level_binding_finder::TopLevelBindingFinder,
    },
};

pub struct CCodeGenerator<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,

    output: Option<RefMut<'state, CCodeOutput>>,

    node_stats: Ref<'state, StatData>,

    variable_data: Ref<'state, ConcreteVariableData>,
    function_data: Ref<'state, ConcreteFunctionData>,
    type_data: Ref<'state, ConcreteTypeData>,
    scope_data: Ref<'state, ConcreteScopeData>,
    glsl_functions: Ref<'state, PrecompiledGlslFunctions>,

    struct_aliases: RefCell<StructAliasMap>,
    temporary_counter: RefCell<u64>,
}

impl PassFactory for CCodeGenerator<'_> {
    type Output<'state> = CCodeGenerator<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        let program = state.check_named()?;
        let errors = state.check_named()?;
        let node_stats = state.check_named()?;

        let variable_data = state.check_named()?;
        let function_data = state.check_named()?;
        let type_data = state.check_named()?;
        let scope_data = state.check_named()?;
        let glsl_functions = state.check_named()?;

        let output = state.insert_default();

        Ok(Self::Output {
            errors: errors.get_mut(state),
            program: Some(program.get_mut(state)),

            output: Some(output.get_mut(state)),

            node_stats: node_stats.get(state),

            variable_data: variable_data.get(state),
            function_data: function_data.get(state),

            type_data: type_data.get(state),
            scope_data: scope_data.get(state),
            glsl_functions: glsl_functions.get(state),

            struct_aliases: Default::default(),
            temporary_counter: RefCell::new(0),
        })
    }
}
impl Pass for CCodeGenerator<'_> {
    fn run<'state>(mut self: Box<Self>) -> PassResult {
        if self
            .errors
            .iter()
            .any(|err| err.severity() == ErrorSeverity::Error)
        {
            return Err(PassError::InvalidInput {
                producing_pass: Self::name(),
                source: "Compilation has errors. Not continuing further.".into(),
            });
        }

        let mut program = self.program.take().unwrap();
        let mut output = self.output.take().unwrap();

        *output = CCodeOutput(self.visit_program(&mut program));

        Ok(())
    }
}

impl CCodeGenerator<'_> {
    fn as_struct_type(&self, body: String) -> String {
        let num_structs = self.struct_aliases.borrow().len();

        "struct ".to_string()
            + &self
                .struct_aliases
                .borrow_mut()
                .entry(body)
                .or_insert_with(|| (num_structs, self.unique_temporary("Struct")))
                .1
    }

    fn runtime_type_to_c(&self, type_: &ConcreteRuntimeType) -> (String, String) {
        match type_ {
            ConcreteRuntimeType::I8 => ("int8_t".to_string(), "".to_string()),
            ConcreteRuntimeType::I16 => ("int16_t".to_string(), "".to_string()),
            ConcreteRuntimeType::I32 => ("int32_t".to_string(), "".to_string()),
            ConcreteRuntimeType::I64 => ("int64_t".to_string(), "".to_string()),
            ConcreteRuntimeType::U8 => ("uint8_t".to_string(), "".to_string()),
            ConcreteRuntimeType::U16 => ("uint16_t".to_string(), "".to_string()),
            ConcreteRuntimeType::U32 => ("uint32_t".to_string(), "".to_string()),
            ConcreteRuntimeType::U64 => ("uint64_t".to_string(), "".to_string()),
            ConcreteRuntimeType::F32 => ("float".to_string(), "".to_string()),
            ConcreteRuntimeType::F64 => ("double".to_string(), "".to_string()),
            ConcreteRuntimeType::Boolean => ("bool".to_string(), "".to_string()),
            ConcreteRuntimeType::Unit => ("void".to_string(), "".to_string()),
            ConcreteRuntimeType::ArrayOf { subtype, size } => {
                let (type_, after_id) = self.runtime_type_to_c(subtype);
                let type_result = format!("{type_} value[{size}]{after_id};");

                (self.as_struct_type(type_result), "".to_string())
            }
            ConcreteRuntimeType::Struct {
                members,
                source_hash: _,
            } => (
                self.as_struct_type(
                    members
                        .iter()
                        .map(|(member, type_)| {
                            let (type_, after_id) = self.runtime_type_to_c(type_);
                            format!("{type_} {member}{after_id};")
                        })
                        .join("\n"),
                ),
                "".to_string(),
            ),
        }
    }
    fn runtime_type_to_byte_size(type_: &ConcreteRuntimeType) -> usize {
        match type_ {
            ConcreteRuntimeType::I8 | ConcreteRuntimeType::U8 => 1,
            ConcreteRuntimeType::I16 | ConcreteRuntimeType::U16 => 2,
            ConcreteRuntimeType::I32 | ConcreteRuntimeType::U32 => 4,
            ConcreteRuntimeType::I64 | ConcreteRuntimeType::U64 => 8,
            ConcreteRuntimeType::F32 => 4,
            ConcreteRuntimeType::F64 => 8,
            ConcreteRuntimeType::Boolean => 1,
            ConcreteRuntimeType::Unit => 0,
            ConcreteRuntimeType::ArrayOf { subtype, size } => {
                let subtype_size = Self::runtime_type_to_byte_size(subtype);
                size * subtype_size
            }
            ConcreteRuntimeType::Struct {
                members,
                source_hash: _,
            } => members
                .iter()
                .map(|(_member, type_)| Self::runtime_type_to_byte_size(type_))
                .sum(),
        }
    }

    fn generate_function_declaration(&self, function: &ConcreteFunction, mangle: bool) -> String {
        let params = function
            .parameter_types
            .iter()
            .map(|param| {
                let (type_, after_id) = self.runtime_type_to_c(&param.borrow().type_);
                type_ + " " + &self.mangle_variable(&param.borrow()) + &after_id
            })
            .join(", ");

        let (type_, after_id) = self.runtime_type_to_c(&function.return_type);
        type_
            + &after_id
            + " "
            + if mangle {
                self.mangle_function(&function.symbol.name)
            } else {
                function.symbol.name.clone()
            }
            .as_str()
            + "("
            + if function.parameter_types.is_empty() {
                "void"
            } else {
                &params
            }
            + ")"
    }

    fn mangle_variable(&self, var: &ConcreteVariable) -> String {
        format!("fleet_{}_{}", var.symbol.name, var.id.0)
    }
    fn mangle_function(&self, name: &str) -> String {
        format!("fleet_{name}")
    }
    fn unique_temporary(&self, name: &str) -> String {
        let count = *self.temporary_counter.borrow();
        *self.temporary_counter.borrow_mut() += 1;
        format!("temporary_{name}_{count}")
    }
}

#[derive(Default, Clone)]
pub struct PreStatementValue {
    pub pre_statements: String,
    pub out_value: String,
}

impl AstVisitor for CCodeGenerator<'_> {
    type ProgramOutput = String;
    type TopLevelOutput = String;
    type FunctionBodyOutput = String;
    type SimpleBindingOutput = String;
    type StatementOutput = String;
    type ExecutorHostOutput = String;
    type ExecutorOutput = String;
    type ExpressionOutput = PreStatementValue;
    type LValueOutput = PreStatementValue;
    type TypeOutput = String;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        let function_definitions = program
            .top_level_statements
            .iter_mut()
            .map(|tls| self.visit_top_level_statement(tls))
            .join("\n");

        let function_declarations = program
            .top_level_statements
            .iter()
            .filter_map(|f| {
                if let TopLevelStatement::Function(_) = f {
                    Some(
                        self.generate_function_declaration(
                            &self
                                .function_data
                                .get(&f.get_id())
                                .expect("Functions should be tracked before calling c_generator")
                                .borrow()
                                .clone(),
                            true,
                        ) + ";",
                    )
                } else {
                    None
                }
            })
            .join("\n");

        let struct_aliases = self
            .struct_aliases
            .borrow()
            .iter()
            .sorted_by_key(|(_body, (i, _name))| *i)
            .map(|(body, (_i, name))| format!("struct {name} {{\n{}\n}};", indent_all_by(4, body)))
            .join("\n");

        let needs_runtime = self
            .node_stats
            .get(&program.id)
            .expect("Program must have stats")
            .uses_gpu
            .at_least_maybe();

        let (runtime_include_str, runtime_init_str, runtime_deinit_str) = if needs_runtime {
            (
                "#include \"fl_runtime.h\"",
                "fl_runtime_init();",
                "fl_runtime_deinit();",
            )
        } else {
            ("", "", "")
        };

        formatdoc!(
            r##"
            #include <stdio.h>
            #include <stdlib.h>
            #include <stdint.h>
            #include <stdbool.h>
            #include <string.h>
            #include <math.h>

            {runtime_include_str}

            // structs
            {struct_aliases}

            // function declarations
            {function_declarations}

            // function definitions
            {function_definitions}

            void initialize_fleet(void) {{
                {runtime_init_str}
            }}
            void deinitialize_fleet(void) {{
                {runtime_deinit_str}
            }}

            int main(int argc, const char** argv) {{
                initialize_fleet();
                int retvalue = fleet_main();
                deinitialize_fleet();
                return retvalue;
            }}
            "##,
        )
    }

    fn visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name: _,
            name_token: _,
            equal_token: _,
            open_paren_token: _,
            parameters: _,
            close_paren_token: _,
            right_arrow_token: _,
            return_type: _,
            body,
            id,
        }: &mut FunctionDefinition,
    ) -> Self::TopLevelOutput {
        let function = self
            .function_data
            .get(id)
            .expect("Functions should be tracked before calling c_generator");

        let decl = self.generate_function_declaration(&function.borrow().clone(), true);
        let body = self.visit_function_body(body);
        decl + &body
    }

    fn visit_type_alias(&mut self, _type_alias: &mut TypeAlias) -> Self::TopLevelOutput {
        // type aliases get completely compiled out and aren't relevant after type propagation
        "".to_string()
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        format!("{{{}}}", self.visit_statement(statement))
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol,
            symbol_token: _,
            semicolon_token: _,
            id,
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        let parent_function = self.function_data.get(id).unwrap();
        let mut fake_extern_function = parent_function.borrow().clone();
        fake_extern_function.symbol.name = symbol.clone();

        formatdoc!(
            "
            {{
                extern {};
                return {symbol}({});
            }}\
            ",
            self.generate_function_declaration(&fake_extern_function, false),
            parent_function
                .borrow()
                .parameter_types
                .iter()
                .map(|param| self.mangle_variable(&param.borrow()))
                .join(",")
        )
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token: _,
            name: _,
            type_: _,
            id,
        }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let inferred_type = self
            .type_data
            .get(id)
            .expect("Bindings should have types before calling c_generator");

        let (type_, after_id) = self.runtime_type_to_c(inferred_type);

        let ref_var = self.variable_data.get(id).unwrap();

        format!(
            "{} {}{}",
            type_,
            self.mangle_variable(&ref_var.borrow()),
            after_id
        )
    }

    fn visit_expression_statement(
        &mut self,
        ExpressionStatement {
            expression,
            semicolon_token: _,
            id: _,
        }: &mut ExpressionStatement,
    ) -> Self::StatementOutput {
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(expression);
        format!("{pre_statements}{out_value};")
    }

    fn visit_on_statement(&mut self, stmt: &mut OnStatement) -> Self::StatementOutput {
        let OnStatement {
            on_token: _,
            executor,
            iterators,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id: _,
        } = stmt;

        let Executor::GPU(gpu_executor) = &**executor else {
            todo!()
        };

        let mut gpu_executor_clone = gpu_executor.clone();
        let GPUExecutor {
            host: _,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index: _,
            close_bracket_token: _,
            id: _,
        } = gpu_executor;

        let mut buffers = vec![];

        let iterator_end_values = iterators
            .iter_mut()
            .map(|it| self.visit_expression(&mut it.max_value))
            .collect_vec();

        let bindings = bindings
            .iter_mut()
            .map(|binding| {
                let mut tlbf = TopLevelBindingFinder::default();
                tlbf.visit_lvalue(&mut binding.0);
                tlbf.get_result().unwrap()
            })
            .collect_vec();

        let mut bindings = GLSLCodeGenerator::get_on_statement_bindings(
            &self.variable_data,
            &self.scope_data,
            &self.type_data,
            bindings,
            iterators
                .iter()
                .map(|it| {
                    self.variable_data
                        .get(&it.binding.id)
                        .expect("Variable data should exist before calling c_generator")
                        .clone()
                })
                .collect_vec(),
            body,
        );

        let (rw_allocations, rw_deallocations): (Vec<_>, Vec<_>) = bindings
            .rw
            .iter_mut()
            .map(|(type_, top_level_binding)| {
                let PreStatementValue {
                    pre_statements,
                    out_value,
                } = self.visit_variable_lvalue(top_level_binding);
                let size = Self::runtime_type_to_byte_size(type_);

                let temporary = self.unique_temporary("lvalue_binding");

                buffers.push(temporary.clone());

                (
                    formatdoc! {"
                        {pre_statements}void* {temporary} = &{out_value};
                        fl_runtime_allocate_gpu_backing({temporary}, {size});
                        fl_runtime_copy_to_backing({temporary});
                    "},
                    formatdoc! {"
                        fl_runtime_copy_from_backing({temporary});
                        fl_runtime_free_gpu_backing({temporary});
                    "},
                )
            })
            .unzip();

        let rw_allocations = rw_allocations.concat();
        let rw_deallocations = rw_deallocations.concat();

        let iterator_size_buffer = self.unique_temporary("iterator_sizes");
        let iterator_size_buffer_pre = format!(
            "uint32_t {iterator_size_buffer}[] = {{ {} }};",
            iterator_end_values
                .iter()
                .map(
                    |PreStatementValue {
                         pre_statements: _,
                         out_value,
                     }| format!("({out_value})")
                )
                .join(", ")
        );

        let (ro_allocations, ro_deallocations): (Vec<_>, Vec<_>) = bindings
            .ro
            .iter_mut()
            .map(|(type_, variable)| {
                (
                    self.mangle_variable(&variable.borrow()),
                    Self::runtime_type_to_byte_size(type_),
                )
            })
            .chain(Some((
                iterator_size_buffer,
                iterator_end_values.len()
                    * Self::runtime_type_to_byte_size(&ConcreteRuntimeType::I32),
            )))
            .collect_vec()
            .into_iter()
            .map(|(name, size)| {
                let temporary = self.unique_temporary("lvalue_binding");

                buffers.push(temporary.clone());

                (
                    formatdoc! {"
                        void* {temporary} = &{name};
                        fl_runtime_allocate_gpu_backing({temporary}, {size});
                        fl_runtime_copy_to_backing({temporary});
                    "},
                    formatdoc! {"
                        fl_runtime_free_gpu_backing({temporary});
                    "},
                )
            })
            .unzip();

        let PreStatementValue {
            pre_statements: dispatch_pre,
            out_value: dispatch_value,
        } = iterator_end_values
            .iter()
            .cloned()
            .reduce(
                |PreStatementValue {
                     pre_statements: pre1,
                     out_value: v1,
                 },
                 PreStatementValue {
                     pre_statements: pre2,
                     out_value: v2,
                 }| {
                    PreStatementValue {
                        pre_statements: pre1 + &pre2,
                        out_value: format!("(({v1}) * ({v2}))"),
                    }
                },
            )
            .expect("An on-statement must have at least one iterator");

        let ro_allocations = ro_allocations.concat();
        let ro_deallocations = ro_deallocations.concat();

        let buffers_name = self.unique_temporary("buffers");
        let buffers_str = format!("void* {buffers_name}[] = {{ {} }};", buffers.join(", "));
        let buffers_len = buffers.len();

        let glsl_errors = RefCell::new(Errors::default());

        let mut glsl_generator = GLSLCodeGenerator::new(
            glsl_errors.borrow_mut(),
            Ref::clone(&self.variable_data),
            Ref::clone(&self.function_data),
            Ref::clone(&self.type_data),
            Ref::clone(&self.node_stats),
        );

        let Ok(glsl_source) = glsl_generator.generate_on_statement_shader(
            &bindings,
            body,
            iterators,
            &mut gpu_executor_clone,
            (*self.glsl_functions).clone(),
        ) else {
            return "#error glsl generation failed completely\n".to_string();
        };

        #[cfg(feature = "gpu_backend")]
        let prepare_shader = || -> Option<(String, usize)> {
            let Ok(shaderc_output) =
                glsl_generator.compile_on_statement_shader(&glsl_source, &**body)
            else {
                return None;
            };

            let bytes_str = shaderc_output
                .as_binary()
                .iter()
                .map(|chunk| format!("0x{chunk:08x}"))
                .join(", ");

            Some((bytes_str, shaderc_output.len()))
        };
        #[cfg(not(feature = "gpu_backend"))]
        let prepare_shader = || -> Option<(String, usize)> {
            use crate::passes::find_node_bounds::find_node_bounds;

            drop(glsl_generator);
            self.errors.push(ErrorKind::GpuBackendDisabled {
                use_location: find_node_bounds(&**executor),
            });
            Some(("/* GPU backend missing */".to_string(), 42))
        };

        let prepare_output = prepare_shader();
        self.errors.append(&mut glsl_errors.borrow_mut());

        let Some((bytes_str, spirv_size)) = prepare_output else {
            return format!("#error malformed on-statement\n/*\n{glsl_source}\n*/\n");
        };

        let spirv_temporary = self.unique_temporary("spirv");

        formatdoc! {"
            // Read + Write allocations
            {rw_allocations}
            // Read only allocations
            {iterator_size_buffer_pre}
            {ro_allocations}
            {buffers_str}

            fl_runtime_bind_buffers(&{buffers_name}, {buffers_len});

            /*
            {glsl_source}
            */

            const uint32_t {spirv_temporary}[] = {{ {bytes_str} }};
            {dispatch_pre}fl_runtime_dispatch_shader({dispatch_value}, {spirv_temporary}, {spirv_size});

            // Read + Write deallocations
            {rw_deallocations}
            // Read only deallocations
            {ro_deallocations}
            "}
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
        formatdoc!(
            "
            {{
                {}
            }}\
            ",
            indent::indent_by(
                4,
                body.iter_mut()
                    .map(|stmt| self.visit_statement(stmt))
                    .join("\n"),
            )
        )
    }

    fn visit_return_statement(
        &mut self,
        ReturnStatement {
            return_token: _,
            value,
            semicolon_token: _,
            id: _,
        }: &mut ReturnStatement,
    ) -> Self::StatementOutput {
        let PreStatementValue {
            pre_statements,
            out_value,
        } = value
            .as_mut()
            .map(|expr| self.visit_expression(expr))
            .unwrap_or(PreStatementValue::default());
        format!("{pre_statements}return {out_value};")
    }

    fn visit_variable_definition_statement(
        &mut self,
        VariableDefinitionStatement {
            let_token: _,
            binding,
            equals_token: _,
            value,
            semicolon_token: _,
            id: _,
        }: &mut VariableDefinitionStatement,
    ) -> Self::StatementOutput {
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(&mut *value);
        format!(
            "{pre_statements}{} = ({out_value});",
            self.visit_simple_binding(binding),
        )
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
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(condition);

        let (elif_prestatements, elifs): (Vec<_>, Vec<_>) = elifs
            .iter_mut()
            .map(|(_token, condition, body)| {
                let PreStatementValue {
                    pre_statements,
                    out_value,
                } = self.visit_expression(condition);
                (
                    pre_statements,
                    formatdoc!(
                        "
                        else if ({out_value}) {{
                            {}
                        }}\
                        ",
                        indent::indent_by(4, self.visit_statement(body))
                    ),
                )
            })
            .unzip();

        formatdoc!(
            "
            {pre_statements}{}if ({out_value}) {{
                {}
            }}
            {}\
            ",
            elif_prestatements.concat(),
            indent::indent_by(4, self.visit_statement(&mut *if_body)),
            elifs.join("\n")
        ) + &else_
            .as_mut()
            .map(|(_token, body)| {
                formatdoc!(
                    "
                        else {{
                            {}
                        }}\
                        ",
                    self.visit_statement(&mut *body)
                )
            })
            .unwrap_or("".to_string())
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
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(condition);
        formatdoc!(
            "
            {pre_statements}while ({out_value}) {{
                {}
            }}\
            ",
            indent::indent_by(4, self.visit_statement(&mut *body))
        )
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
        let PreStatementValue {
            pre_statements: cond_pre_statements,
            out_value: cond_out_value,
        } = condition
            .as_mut()
            .map(|cond| self.visit_expression(cond))
            .unwrap_or(PreStatementValue::default());
        let PreStatementValue {
            pre_statements: inc_pre_statements,
            out_value: inc_out_value,
        } = incrementer
            .as_mut()
            .map(|inc| self.visit_expression(inc))
            .unwrap_or(PreStatementValue::default());
        formatdoc!(
            "
            {cond_pre_statements}{inc_pre_statements}for ({} {cond_out_value}; {inc_out_value}) {{
                {}
            }}\
            ",
            self.visit_statement(initializer),
            indent::indent_by(4, self.visit_statement(&mut *body))
        )
    }

    fn visit_break_statement(
        &mut self,
        BreakStatement {
            break_token: _,
            semicolon_token: _,
            id: _,
        }: &mut BreakStatement,
    ) -> Self::StatementOutput {
        "break;".to_string()
    }

    fn visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id: _,
        }: &mut SkipStatement,
    ) -> Self::StatementOutput {
        "continue;".to_string()
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

    fn visit_gpu_executor(&mut self, _executor: &mut GPUExecutor) -> Self::ExecutorOutput {
        todo!()
    }

    fn visit_literal_expression(
        &mut self,
        LiteralExpression {
            value,
            token: _,
            id,
        }: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        let format_float = |x: f64| {
            let nr = x.to_string();
            let zeroes = if x.round() == x { ".0" } else { "" };

            let suffix = if *self
                .type_data
                .get(id)
                .expect("type data must be available before calling c_generator")
                == ConcreteRuntimeType::F32
            {
                "f"
            } else {
                ""
            };

            nr + zeroes + suffix
        };

        PreStatementValue {
            pre_statements: "".to_string(),
            out_value: match value {
                LiteralKind::Number(value) => {
                    if self
                        .type_data
                        .get(id)
                        .expect("type data must be available before calling c_generator")
                        .is_float()
                    {
                        format_float(*value as f64)
                    } else {
                        value.to_string()
                    }
                }
                LiteralKind::Char(value) => (*value as u8).to_string(),
                LiteralKind::Float(value) => format_float(*value),
                LiteralKind::Bool(value) => value.to_string(),
            },
        }
    }

    fn visit_array_expression(
        &mut self,
        ArrayExpression {
            open_bracket_token: _,
            elements,
            close_bracket_token: _,
            id,
        }: &mut ArrayExpression,
    ) -> Self::ExpressionOutput {
        let inferred_type = self
            .type_data
            .get(id)
            .expect("Array expressions should have types before calling c_generator");

        let (type_, after_id) = self.runtime_type_to_c(inferred_type);

        let ConcreteRuntimeType::ArrayOf { .. } = inferred_type else {
            unreachable!("array expressions must have type ArrayOf(_)")
        };

        let (pre_statements, elements): (Vec<_>, Vec<_>) = elements
            .iter_mut()
            .map(|(element, _comma)| self.visit_expression(element))
            .map(
                |PreStatementValue {
                     pre_statements,
                     out_value,
                 }| (pre_statements, out_value),
            )
            .unzip();
        PreStatementValue {
            pre_statements: pre_statements.concat(),
            out_value: format!("(({type_}{after_id}){{ {} }})", elements.join(", ")),
        }
    }

    fn visit_struct_expression(
        &mut self,
        StructExpression {
            type_: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id,
        }: &mut StructExpression,
    ) -> Self::ExpressionOutput {
        let inferred_type = self
            .type_data
            .get(id)
            .expect("Struct expressions should have types before calling c_generator");

        let (type_, after_id) = self.runtime_type_to_c(inferred_type);

        let ConcreteRuntimeType::Struct { .. } = inferred_type else {
            unreachable!("struct expressions must have type Struct(_)")
        };

        let (pre_statements, values): (Vec<_>, Vec<_>) = members
            .iter_mut()
            .map(
                |(
                    StructMemberValue {
                        name,
                        name_token: _,
                        colon_token: _,
                        value,
                    },
                    _comma,
                )| {
                    let PreStatementValue {
                        pre_statements,
                        out_value,
                    } = self.visit_expression(value);

                    (pre_statements, format!(".{name} = {},", out_value))
                },
            )
            .unzip();

        PreStatementValue {
            pre_statements: pre_statements.join("\n"),
            out_value: format!("(({type_}{after_id}){{ {} }})", values.join("\n")),
        }
    }

    fn visit_function_call_expression(
        &mut self,
        FunctionCallExpression {
            name,
            name_token: _,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id: _,
        }: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let (pre_statements, args): (Vec<_>, Vec<_>) = arguments
            .iter_mut()
            .map(|(arg, _comma)| self.visit_expression(arg))
            .map(
                |PreStatementValue {
                     pre_statements,
                     out_value,
                 }| (pre_statements, out_value),
            )
            .unzip();
        PreStatementValue {
            pre_statements: pre_statements.concat(),
            out_value: format!(
                "{}({})",
                self.mangle_function(name),
                args.iter().map(|arg| format!("({arg})")).join(",")
            ),
        }
    }

    fn visit_compiler_expression(
        &mut self,
        expr: &mut CompilerExpression,
    ) -> Self::ExpressionOutput {
        let CompilerExpression {
            at_token: _,
            name,
            name_token,
            open_paren_token: _,
            arguments,
            close_paren_token: _,
            id,
        } = expr;

        let (pre_statements, args): (Vec<_>, Vec<_>) = arguments
            .iter_mut()
            .map(|(arg, _comma)| self.visit_expression(arg))
            .map(
                |PreStatementValue {
                     pre_statements,
                     out_value,
                 }| (pre_statements, out_value),
            )
            .unzip();

        match name.as_str() {
            "zero" => {
                let expected_type = self
                    .type_data
                    .get(id)
                    .expect("type data must exist before calling c_generator");
                let (type_, after_id) = self.runtime_type_to_c(expected_type);

                if expected_type.is_numeric() {
                    PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(({type_}{after_id})(0))"),
                    }
                } else if expected_type.is_boolean() {
                    PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: "false".to_string(),
                    }
                } else if let ConcreteRuntimeType::ArrayOf { .. }
                | ConcreteRuntimeType::Struct { .. } = expected_type
                {
                    let tmp = self.unique_temporary("zero");
                    let size = Self::runtime_type_to_byte_size(expected_type);
                    PreStatementValue {
                        pre_statements: formatdoc!(
                            "
                            {type_} {tmp}{after_id};
                            memset(&{tmp}, 0, {size});
                            "
                        ),
                        out_value: format!("({tmp})"),
                    }
                } else {
                    self.errors.push(ErrorKind::InvalidIntrinsicType {
                        backend: Backend::C,
                        intrinsic: Intrinsic::Zero,
                        intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                        type_: expected_type.clone(),
                    });
                    PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: "\n#error unimplemented type for @zero\n".to_string(),
                    }
                }
            }
            "sqrt" => {
                let expected_type = self
                    .type_data
                    .get(id)
                    .expect("type data must exist before calling c_generator");

                match expected_type {
                    ConcreteRuntimeType::F32 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(sqrtf({}))", args.first().unwrap()),
                    },
                    ConcreteRuntimeType::F64 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(sqrt({}))", args.first().unwrap()),
                    },
                    _ => {
                        self.errors.push(ErrorKind::InvalidIntrinsicType {
                            backend: Backend::C,
                            intrinsic: Intrinsic::Sqrt,
                            intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                            type_: expected_type.clone(),
                        });
                        PreStatementValue {
                            pre_statements: "".to_string(),
                            out_value: "\n#error unimplemented type for @sqrt\n".to_string(),
                        }
                    }
                }
            }
            "sin" => {
                let expected_type = self
                    .type_data
                    .get(id)
                    .expect("type data must exist before calling c_generator");

                match expected_type {
                    ConcreteRuntimeType::F32 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(sinf({}))", args.first().unwrap()),
                    },
                    ConcreteRuntimeType::F64 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(sin({}))", args.first().unwrap()),
                    },
                    _ => {
                        self.errors.push(ErrorKind::InvalidIntrinsicType {
                            backend: Backend::C,
                            intrinsic: Intrinsic::Sin,
                            intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                            type_: expected_type.clone(),
                        });
                        PreStatementValue {
                            pre_statements: "".to_string(),
                            out_value: "\n#error unimplemented type for @sin\n".to_string(),
                        }
                    }
                }
            }
            "cos" => {
                let expected_type = self
                    .type_data
                    .get(id)
                    .expect("type data must exist before calling c_generator");

                match expected_type {
                    ConcreteRuntimeType::F32 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(cosf({}))", args.first().unwrap()),
                    },
                    ConcreteRuntimeType::F64 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(cos({}))", args.first().unwrap()),
                    },
                    _ => {
                        self.errors.push(ErrorKind::InvalidIntrinsicType {
                            backend: Backend::C,
                            intrinsic: Intrinsic::Cos,
                            intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                            type_: expected_type.clone(),
                        });
                        PreStatementValue {
                            pre_statements: "".to_string(),
                            out_value: "\n#error unimplemented type for @cos\n".to_string(),
                        }
                    }
                }
            }
            "length" => {
                let array_type = self
                    .type_data
                    .get(&arguments[0].0.get_id())
                    .expect("type data must exist before calling c_generator");

                match array_type {
                    ConcreteRuntimeType::ArrayOf { subtype: _, size } => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("{size}"),
                    },
                    _ => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: "\n#error non-array type for @length\n".to_string(),
                    },
                }
            }
            "comptime" => PreStatementValue {
                pre_statements: pre_statements.first().unwrap().clone(),
                out_value: args.first().unwrap().clone(),
            },
            _ => {
                self.errors.push(ErrorKind::IntrinsicNotImplemented {
                    backend: Backend::C,
                    intrinsic: UnresolvedSymbol::from_token(name.clone(), name_token),
                });

                PreStatementValue {
                    pre_statements: "".to_string(),
                    out_value: "\n#error unimplemented compiler function\n".to_string(),
                }
            }
        }
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
        let PreStatementValue {
            pre_statements: array_pre_statements,
            out_value: array_out_value,
        } = self.visit_expression(&mut *array);
        let PreStatementValue {
            pre_statements: index_pre_statements,
            out_value: index_out_value,
        } = self.visit_expression(&mut *index);

        PreStatementValue {
            pre_statements: array_pre_statements + &index_pre_statements,
            out_value: format!("((({array_out_value}).value)[{index_out_value}])"),
        }
    }

    fn visit_struct_access_expression(
        &mut self,
        StructAccessExpression {
            value,
            dot_token: _,
            member_name,
            member_name_token: _,
            id: _,
        }: &mut StructAccessExpression,
    ) -> Self::ExpressionOutput {
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(&mut *value);

        PreStatementValue {
            pre_statements,
            out_value: format!("(({out_value}).{member_name})"),
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
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(&mut *subexpression);

        PreStatementValue {
            pre_statements,
            out_value: format!("({out_value})"),
        }
    }

    fn visit_variable_access_expression(
        &mut self,
        VariableAccessExpression {
            name: _,
            name_token: _,
            id,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let ref_var = self.variable_data.get(id).unwrap();

        PreStatementValue {
            pre_statements: "".to_string(),
            out_value: self.mangle_variable(&ref_var.borrow()),
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
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(&mut *operand);

        PreStatementValue {
            pre_statements,
            out_value: format!(
                "({}({out_value}))",
                match operation {
                    UnaryOperation::BitwiseNot => "~",
                    UnaryOperation::LogicalNot => "!",
                    UnaryOperation::Negate => "-",
                },
            ),
        }
    }

    fn visit_cast_expression(
        &mut self,
        CastExpression {
            operand,
            as_token: _,
            type_: _,
            id,
        }: &mut CastExpression,
    ) -> Self::ExpressionOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            self.type_data
                .get(id)
                .expect("types should be inferred before calling c_generator"),
        );

        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(&mut *operand);

        PreStatementValue {
            pre_statements,
            out_value: format!("(({type_}{after_id})({out_value}))"),
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
        let PreStatementValue {
            pre_statements: left_pre_statements,
            out_value: left_out_value,
        } = self.visit_expression(&mut *left);
        let PreStatementValue {
            pre_statements: right_pre_statements,
            out_value: right_out_value,
        } = self.visit_expression(&mut *right);

        PreStatementValue {
            pre_statements: left_pre_statements + &right_pre_statements,
            out_value: match operation {
                BinaryOperation::Add => {
                    format!("(({left_out_value}) + ({right_out_value}))")
                }
                BinaryOperation::Subtract => {
                    format!("(({left_out_value}) - ({right_out_value}))")
                }
                BinaryOperation::Multiply => {
                    format!("(({left_out_value}) * ({right_out_value}))")
                }
                BinaryOperation::Divide => {
                    format!("(({left_out_value}) / ({right_out_value}))")
                }
                BinaryOperation::Modulo => {
                    // TODO: handle doubles and floats separately
                    if self
                        .type_data
                        .get(&left.get_id())
                        .expect("type data must exist before calling c_generator")
                        .is_float()
                    {
                        format!("(fmod(({left_out_value}), ({right_out_value})))")
                    } else {
                        format!("(({left_out_value}) % ({right_out_value}))")
                    }
                }
                BinaryOperation::GreaterThan => {
                    format!("(({left_out_value}) > ({right_out_value}))")
                }
                BinaryOperation::GreaterThanOrEqual => {
                    format!("(({left_out_value}) >= ({right_out_value}))")
                }
                BinaryOperation::LessThan => {
                    format!("(({left_out_value}) < ({right_out_value}))")
                }
                BinaryOperation::LessThanOrEqual => {
                    format!("(({left_out_value}) <= ({right_out_value}))")
                }
                BinaryOperation::Equal => {
                    format!("(({left_out_value}) == ({right_out_value}))")
                }
                BinaryOperation::NotEqual => {
                    format!("(({left_out_value}) != ({right_out_value}))")
                }
                BinaryOperation::LogicalAnd => {
                    format!("(({left_out_value}) && ({right_out_value}))")
                }
                BinaryOperation::LogicalOr => {
                    format!("(({left_out_value}) || ({right_out_value}))")
                }
            },
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
        let PreStatementValue {
            pre_statements: rpre_statements,
            out_value: out_rvalue,
        } = self.visit_expression(&mut *right);

        let PreStatementValue {
            pre_statements: lpre_statements,
            out_value: out_lvalue,
        } = self.visit_lvalue(lvalue);

        PreStatementValue {
            pre_statements: lpre_statements + &rpre_statements,
            out_value: format!("(({out_lvalue}) = ({out_rvalue}))",),
        }
    }

    fn visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name: _,
            name_token: _,
            id,
        }: &mut VariableLValue,
    ) -> Self::LValueOutput {
        let ref_var = self.variable_data.get(id).unwrap();

        PreStatementValue {
            pre_statements: "".to_string(),
            out_value: self.mangle_variable(&ref_var.borrow()),
        }
    }

    fn visit_array_index_lvalue(
        &mut self,
        ArrayIndexLValue {
            array,
            open_bracket_token: _,
            index,
            close_bracket_token: _,
            id: _,
        }: &mut ArrayIndexLValue,
    ) -> Self::LValueOutput {
        let PreStatementValue {
            pre_statements: rpre_statements,
            out_value: out_rvalue,
        } = self.visit_expression(&mut *index);

        let PreStatementValue {
            pre_statements: lpre_statements,
            out_value: out_lvalue,
        } = self.visit_lvalue(array);
        PreStatementValue {
            pre_statements: lpre_statements + &rpre_statements,
            out_value: format!("((({out_lvalue}).value)[{out_rvalue}])",),
        }
    }

    fn visit_struct_access_lvalue(
        &mut self,
        StructAccessLValue {
            value,
            dot_token: _,
            member_name,
            member_name_token: _,
            id: _,
        }: &mut StructAccessLValue,
    ) -> Self::LValueOutput {
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_lvalue(value);

        PreStatementValue {
            pre_statements,
            out_value: format!("(({out_value}).{member_name})"),
        }
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
        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_lvalue(&mut *sublvalue);

        PreStatementValue {
            pre_statements,
            out_value: format!("({out_value})"),
        }
    }

    fn visit_simple_type(
        &mut self,
        SimpleType {
            token: _,
            type_: _,
            id,
        }: &mut SimpleType,
    ) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            self.type_data
                .get(id)
                .expect("type data should exist before calling c_generator"),
        );
        type_ + &after_id
    }

    fn visit_unit_type(
        &mut self,
        UnitType {
            open_paren_token: _,
            close_paren_token: _,
            id,
        }: &mut UnitType,
    ) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            self.type_data
                .get(id)
                .expect("type data should exist before calling c_generator"),
        );
        type_ + &after_id
    }

    fn visit_idk_type(&mut self, IdkType { token: _, id }: &mut IdkType) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            self.type_data
                .get(id)
                .expect("type data should exist before calling c_generator"),
        );
        type_ + &after_id
    }

    fn visit_array_type(
        &mut self,
        ArrayType {
            subtype: _,
            open_bracket_token: _,
            size: _,
            close_bracket_token: _,
            id,
        }: &mut ArrayType,
    ) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            self.type_data
                .get(id)
                .expect("type data should exist before calling c_generator"),
        );
        type_ + &after_id
    }

    fn visit_struct_type(
        &mut self,
        StructType {
            struct_token: _,
            open_brace_token: _,
            members,
            close_brace_token: _,
            id: _,
        }: &mut StructType,
    ) -> Self::TypeOutput {
        format!(
            "struct {{ {} }}",
            members
                .iter_mut()
                .map(
                    |(
                        StructMemberDefinition {
                            name,
                            name_token: _,
                            colon_token: _,
                            type_,
                        },
                        _comma,
                    )| {
                        self.visit_type(type_);
                        let inferred_type = self
                            .type_data
                            .get(&type_.get_id())
                            .expect("Bindings should have types before calling c_generator");

                        let (type_, after_id) = self.runtime_type_to_c(inferred_type);
                        format!("{type_} {name}{after_id}")
                    }
                )
                .join("\n")
        )
    }

    fn visit_alias_type(
        &mut self,
        AliasType {
            name: _,
            name_token: _,
            id,
        }: &mut AliasType,
    ) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            self.type_data
                .get(id)
                .expect("type data should exist before calling c_generator"),
        );
        type_ + &after_id
    }
}
