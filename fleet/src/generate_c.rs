use std::{cell::RefCell, rc::Rc};

use indoc::formatdoc;

use itertools::Itertools;

use crate::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, BoolExpression, BoolType,
        BreakStatement, CastExpression, Executor, ExpressionStatement, ExternFunctionBody,
        ForLoopStatement, FunctionCallExpression, FunctionDefinition, GPUExecutor,
        GroupingExpression, GroupingLValue, HasID, IdkType, IfStatement, IntType, NumberExpression,
        OnStatement, PerNodeData, Program, ReturnStatement, SelfExecutorHost, SimpleBinding,
        SkipStatement, StatementFunctionBody, ThreadExecutor, UnaryExpression, UnaryOperation,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    generate_glsl::GLSLCodeGenerator,
    infra::FleetError,
    passes::type_propagation::{Function, RuntimeType, UnionFindSet, UnionFindSetPtr, Variable},
};

pub struct CCodeGenerator<'inputs, 'errors> {
    errors: &'errors mut Vec<FleetError>,

    variable_data: &'inputs PerNodeData<Rc<RefCell<Variable>>>,
    function_data: &'inputs PerNodeData<Rc<RefCell<Function>>>,
    type_data: &'inputs PerNodeData<UnionFindSetPtr>,
    type_sets: &'inputs UnionFindSet<RuntimeType>,

    temporary_counter: u64,
}
impl<'inputs, 'errors> CCodeGenerator<'inputs, 'errors> {
    pub fn new(
        errors: &'errors mut Vec<FleetError>,
        variable_data: &'inputs PerNodeData<Rc<RefCell<Variable>>>,
        function_data: &'inputs PerNodeData<Rc<RefCell<Function>>>,
        type_data: &'inputs PerNodeData<UnionFindSetPtr>,
        type_sets: &'inputs UnionFindSet<RuntimeType>,
    ) -> Self {
        Self {
            errors,
            variable_data,
            function_data,
            type_data,
            type_sets,
            temporary_counter: 0,
        }
    }

    fn runtime_type_to_c(&self, type_: RuntimeType) -> (String, String) {
        match type_ {
            RuntimeType::I8 => ("int8_t".to_string(), "".to_string()),
            RuntimeType::I16 => ("int16_t".to_string(), "".to_string()),
            RuntimeType::I32 => ("int32_t".to_string(), "".to_string()),
            RuntimeType::I64 => ("int64_t".to_string(), "".to_string()),
            RuntimeType::UnsizedInt => {
                unreachable!("unsized ints should have caused errors before calling c_generator")
            }
            RuntimeType::Boolean => ("bool".to_string(), "".to_string()),
            RuntimeType::Unit => ("void".to_string(), "".to_string()),
            RuntimeType::Unknown => {
                unreachable!("unknown types should have caused errors before calling c_generator")
            }
            RuntimeType::Error => {
                unreachable!("unknown types should have caused errors before calling c_generator")
            }
            RuntimeType::ArrayOf { subtype, size } => {
                let (type_, after_id) = self.runtime_type_to_c(*self.type_sets.get(subtype));
                let Some(size) = size else {
                    unreachable!("arrays should all have a size before calling c_generator");
                };
                (type_, format!("[{size}]{after_id}"))
            }
        }
    }
    fn runtime_type_to_byte_size(&self, type_: RuntimeType) -> usize {
        match type_ {
            RuntimeType::I8 => 1,
            RuntimeType::I16 => 2,
            RuntimeType::I32 => 4,
            RuntimeType::I64 => 8,
            RuntimeType::UnsizedInt => {
                unreachable!("unsized ints should have caused errors before calling c_generator")
            }
            RuntimeType::Boolean => 1,
            RuntimeType::Unit => 0,
            RuntimeType::Unknown => {
                unreachable!("unknown types should have caused errors before calling c_generator")
            }
            RuntimeType::Error => {
                unreachable!("unknown types should have caused errors before calling c_generator")
            }
            RuntimeType::ArrayOf { subtype, size } => {
                let subtype_size = self.runtime_type_to_byte_size(*self.type_sets.get(subtype));
                let size = size.expect("arrays should all have a size before calling c_generator");
                size * subtype_size
            }
        }
    }

    fn generate_function_declaration(&self, function: &Function, mangle: bool) -> String {
        let params = function
            .parameter_types
            .iter()
            .map(|(param, name)| {
                let (type_, after_id) = self.runtime_type_to_c(*self.type_sets.get(*param));
                type_ + " " + &self.mangle_variable(name) + &after_id
            })
            .join(", ");

        let (type_, after_id) = self.runtime_type_to_c(*self.type_sets.get(function.return_type));
        type_
            + &after_id
            + " "
            + if mangle {
                self.mangle_function(&function.name)
            } else {
                function.name.clone()
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

    fn mangle_variable(&self, name: &str) -> String {
        format!("fleet_{name}")
    }
    fn mangle_function(&self, name: &str) -> String {
        format!("fleet_{name}")
    }
    fn unique_temporary(&mut self, name: &str) -> String {
        let count = self.temporary_counter;
        self.temporary_counter += 1;
        format!("temporary_{name}_{count}")
    }
}

#[derive(Default)]
pub struct PreStatementValue {
    pub pre_statements: String,
    pub out_value: String,
}

impl AstVisitor for CCodeGenerator<'_, '_> {
    type ProgramOutput = String;
    type FunctionDefinitionOutput = String;
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
            .functions
            .iter_mut()
            .map(|f| self.visit_function_definition(f))
            .join("\n");

        let function_declarations = program
            .functions
            .iter()
            .map(|f| {
                self.generate_function_declaration(
                    &self
                        .function_data
                        .get(&f.get_id())
                        .expect("Functions should be tracked before calling c_generator")
                        .borrow()
                        .clone(),
                    true,
                ) + ";"
            })
            .join("\n");

        formatdoc!(
            r##"
            #include <stdio.h>
            #include <stdlib.h>
            #include <stdint.h>
            #include <stdbool.h>
            #include <string.h>

            #include "fl_runtime.h"


            // function declarations
            {function_declarations}

            // function definitions
            {function_definitions}

            int main(int argc, const char** argv) {{
                fl_runtime_init();
                int retvalue = fleet_main();
                fl_runtime_deinit();
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
    ) -> Self::FunctionDefinitionOutput {
        let function = self
            .function_data
            .get(id)
            .expect("Functions should be tracked before calling c_generator");

        self.generate_function_declaration(&function.borrow().clone(), true)
            + self.visit_function_body(body).as_str()
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
        fake_extern_function.name = symbol.clone();

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
                .map(|(_param, name)| self.mangle_variable(name))
                .join(",")
        )
    }

    fn visit_simple_binding(
        &mut self,
        SimpleBinding {
            name_token: _,
            name,
            type_: _,
            id,
        }: &mut SimpleBinding,
    ) -> Self::SimpleBindingOutput {
        let inferred_type = *self.type_sets.get(
            *self
                .type_data
                .get(id)
                .expect("Bindings should have types before calling c_generator"),
        );

        let (type_, after_id) = self.runtime_type_to_c(inferred_type);

        format!("{} {}{}", type_, self.mangle_variable(name), after_id)
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
        format!("{}{};", pre_statements, out_value)
    }

    fn visit_on_statement(&mut self, stmt: &mut OnStatement) -> Self::StatementOutput {
        let OnStatement {
            on_token: _,
            executor,
            open_paren_token: _,
            bindings,
            close_paren_token: _,
            body,
            id: _,
        } = stmt;

        let Executor::GPU(gpu_executor) = executor else {
            todo!()
        };

        let mut gpu_executor_clone = gpu_executor.clone();
        let GPUExecutor {
            host: _,
            dot_token: _,
            gpus_token: _,
            open_bracket_token_1: _,
            gpu_index: _,
            close_bracket_token_1: _,
            open_bracket_token_2: _,
            iterator: _,
            equal_token: _,
            max_value: iterator_end_value,
            close_bracket_token_2: _,
            id: _,
        } = gpu_executor;

        let mut buffers = vec![];

        let (allocations, deallocations): (Vec<_>, Vec<_>) = bindings
            .iter_mut()
            .map(|(binding, _comma)| {
                let PreStatementValue {
                    pre_statements,
                    out_value,
                } = self.visit_lvalue(binding);
                let size = self.runtime_type_to_byte_size(
                    *self.type_sets.get(
                        *self
                            .type_data
                            .get_node(binding)
                            .expect("type data must be available before calling c_generator"),
                    ),
                );

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

        let allocations = allocations.concat();
        let deallocations = deallocations.concat();

        let buffers_name = self.unique_temporary("buffers");
        let buffers_str = format!("void* {buffers_name}[] = {{ {} }};", buffers.join(", "));
        let buffers_len = buffers.len();

        let glsl_generator = GLSLCodeGenerator::new(
            self.errors,
            self.variable_data,
            self.function_data,
            self.type_data,
            self.type_sets,
        );
        let Ok((unescaped_glsl, shaderc_output)) = glsl_generator.generate_on_statement_shader(
            bindings
                .iter_mut()
                .map(|(binding, _comma)| binding)
                .collect_vec(),
            body,
            &mut gpu_executor_clone,
        ) else {
            return "#error glsl generation failed completely\n".to_string();
        };
        let Ok(shaderc_output) = shaderc_output else {
            return format!("#error malformed on-statement\n/*\n{unescaped_glsl}\n*/\n");
        };

        let bytes_str = shaderc_output
            .as_binary()
            .iter()
            .map(|chunk| format!("0x{:08x}", chunk))
            .join(", ");

        let spirv_temporary = self.unique_temporary("spirv");
        let spirv_size = shaderc_output.len();

        let PreStatementValue {
            pre_statements: iterator_pre,
            out_value: iterator_value,
        } = self.visit_expression(iterator_end_value);

        formatdoc! {"
            {allocations}
            {buffers_str}
            fl_runtime_bind_buffers(&{buffers_name}, {buffers_len});

            /*
            {unescaped_glsl}
            */

            const uint32_t {spirv_temporary}[] = {{ {bytes_str} }};
            {iterator_pre}fl_runtime_dispatch_shader({iterator_value}, {spirv_temporary}, {spirv_size});

            {deallocations}
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
        format!("{}return {};", pre_statements, out_value)
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
        let ref_var = self
            .variable_data
            .get(&binding.get_id())
            .expect("var data must exist before calling c_generator")
            .clone();

        let type_ = self.type_sets.get(ref_var.borrow().type_);

        if let RuntimeType::ArrayOf {
            subtype: _,
            size: _,
        } = *type_
        {
            let num_bytes = self.runtime_type_to_byte_size(*type_);

            let lvalue_gen = self.visit_simple_binding(binding);
            let lvalue_temporary = self.mangle_variable(&ref_var.borrow().name);

            let rvalue_temporary = self.unique_temporary("rvalue");
            let PreStatementValue {
                pre_statements: rvalue_pre_statements,
                out_value: rvalue_out_value,
            } = self.visit_expression(&mut *value);
            let (rvalue_type, rvalue_postfix) = self.runtime_type_to_c(*type_);

            let rvalue_gen =
                format!("{rvalue_type} (*{rvalue_temporary}){rvalue_postfix} = {rvalue_out_value}");

            let memcpy = format!("memcpy(&{lvalue_temporary}, {rvalue_temporary}, {num_bytes})");

            formatdoc!(
                "
                {lvalue_gen};
                {rvalue_pre_statements}
                {{
                    {};
                    {};
                }}\
                ",
                indent::indent_by(4, rvalue_gen),
                indent::indent_by(4, memcpy),
            )
        } else {
            let PreStatementValue {
                pre_statements,
                out_value,
            } = self.visit_expression(&mut *value);
            format!(
                "{pre_statements}{} = ({out_value});",
                self.visit_simple_binding(binding),
            )
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

    fn visit_number_expression(
        &mut self,
        NumberExpression {
            value,
            token: _,
            id: _,
        }: &mut NumberExpression,
    ) -> Self::ExpressionOutput {
        PreStatementValue {
            pre_statements: "".to_string(),
            out_value: value.to_string(),
        }
    }

    fn visit_bool_expression(
        &mut self,
        BoolExpression {
            value,
            token: _,
            id: _,
        }: &mut BoolExpression,
    ) -> Self::ExpressionOutput {
        PreStatementValue {
            pre_statements: "".to_string(),
            out_value: value.to_string(),
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
        let inferred_type = *self.type_sets.get(
            *self
                .type_data
                .get(id)
                .expect("Array expressions should have types before calling c_generator"),
        );

        let (type_, after_id) = self.runtime_type_to_c(inferred_type);

        let RuntimeType::ArrayOf { subtype, size: _ } = inferred_type else {
            unreachable!("array expressions must have type ArrayOf(_)")
        };

        if let RuntimeType::ArrayOf { subtype: _, size } = self.type_sets.get(subtype) {
            let subtype = *self.type_sets.get(subtype);
            let size = size.expect("arrays must have their size set before calling c_generator");
            let (pre_statements, definitions, temporaries): (Vec<_>, Vec<_>, Vec<_>) = elements
                .iter_mut()
                .map(|(element, _comma)| {
                    let rvalue_temporary = self.unique_temporary("element");
                    let PreStatementValue {
                        pre_statements,
                        out_value,
                    } = self.visit_expression(element);
                    let (rvalue_type, rvalue_postfix) = self.runtime_type_to_c(subtype);

                    let rvalue_gen = format!(
                        "{rvalue_type} (*{rvalue_temporary}){rvalue_postfix} = {out_value};\n"
                    );

                    (pre_statements, rvalue_gen, rvalue_temporary)
                })
                .multiunzip();

            let out_temporary = self.unique_temporary("out");

            PreStatementValue {
                pre_statements: format!(
                    "{pre_statements}{type_} (*{out_temporary}){after_id};\n",
                    pre_statements = indent::indent_by(4, pre_statements.concat()),
                ),
                out_value: formatdoc!(
                    "
                    ({{
                        {definitions}
                        {out_temporary} = (&(({type_}{after_id}){{ {elements} }}));
                        {out_temporary};
                    }})\
                    ",
                    definitions = indent::indent_by(4, definitions.concat()),
                    elements = indent::indent_by(
                        4,
                        temporaries
                            .iter()
                            .flat_map(|tmp| {
                                (0..size)
                                    .map(|i| format!("((*({tmp}))[{i}])"))
                                    .collect_vec()
                            })
                            .join(", ")
                    ),
                ),
            }
        } else {
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
                out_value: format!("(&(({}{}){{ {} }}))", type_, after_id, elements.join(", ")),
            }
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
        let PreStatementValue {
            pre_statements: array_pre_statements,
            out_value: array_out_value,
        } = self.visit_expression(&mut *array);
        let PreStatementValue {
            pre_statements: index_pre_statements,
            out_value: index_out_value,
        } = self.visit_expression(&mut *index);

        if let RuntimeType::ArrayOf {
            subtype: _,
            size: _,
        } = self.type_sets.get(
            *self
                .type_data
                .get(id)
                .expect("type data must exist before calling c_generator"),
        ) {
            PreStatementValue {
                pre_statements: array_pre_statements + &index_pre_statements,
                out_value: format!("(&(*({array_out_value}))[{index_out_value}])"),
            }
        } else {
            PreStatementValue {
                pre_statements: array_pre_statements + &index_pre_statements,
                out_value: format!("((*({array_out_value}))[{index_out_value}])"),
            }
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
            name,
            name_token: _,
            id,
        }: &mut VariableAccessExpression,
    ) -> Self::ExpressionOutput {
        let ref_var = self
            .variable_data
            .get(id)
            .expect("var data must exist before calling c_generator")
            .clone();

        let type_ = self.type_sets.get(ref_var.borrow().type_);

        if let RuntimeType::ArrayOf {
            subtype: _,
            size: _,
        } = *type_
        {
            PreStatementValue {
                pre_statements: "".to_string(),
                out_value: format!("(&{})", self.mangle_variable(name)),
            }
        } else {
            PreStatementValue {
                pre_statements: "".to_string(),
                out_value: self.mangle_variable(name),
            }
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
            *self.type_sets.get(
                *self
                    .type_data
                    .get(id)
                    .expect("types should be inferred before calling c_generator"),
            ),
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
            out_value: format!(
                "(({left_out_value}) {} ({right_out_value}))",
                match operation {
                    BinaryOperation::Add => "+",
                    BinaryOperation::Subtract => "-",
                    BinaryOperation::Multiply => "*",
                    BinaryOperation::Divide => "/",
                    BinaryOperation::Modulo => "%",
                    BinaryOperation::GreaterThan => ">",
                    BinaryOperation::GreaterThanOrEqual => ">=",
                    BinaryOperation::LessThan => "<",
                    BinaryOperation::LessThanOrEqual => "<=",
                    BinaryOperation::Equal => "==",
                    BinaryOperation::NotEqual => "!=",
                    BinaryOperation::LogicalAnd => "&&",
                    BinaryOperation::LogicalOr => "||",
                },
            ),
        }
    }

    fn visit_variable_assignment_expression(
        &mut self,
        VariableAssignmentExpression {
            lvalue,
            equal_token: _,
            right,
            id,
        }: &mut VariableAssignmentExpression,
    ) -> Self::ExpressionOutput {
        let type_ = self.type_sets.get(
            *self
                .type_data
                .get(id)
                .expect("Types must exist before calling c_generator"),
        );
        if let RuntimeType::ArrayOf {
            subtype: _,
            size: _,
        } = *type_
        {
            let num_bytes = self.runtime_type_to_byte_size(*type_);

            let lvalue_temporary = self.unique_temporary("lvalue");
            let PreStatementValue {
                pre_statements: lvalue_pre_statements,
                out_value: lvalue,
            } = self.visit_lvalue(lvalue);
            let lvalue = format!("(&({lvalue}))");
            let (lvalue_type, lvalue_postfix) = self.runtime_type_to_c(*type_);

            let lvalue_gen =
                format!("{lvalue_type} (*{lvalue_temporary}){lvalue_postfix} = {lvalue}");

            let rvalue_temporary = self.unique_temporary("rvalue");
            let PreStatementValue {
                pre_statements: rvalue_pre_statements,
                out_value: rvalue_out_value,
            } = self.visit_expression(&mut *right);
            let (rvalue_type, rvalue_postfix) = self.runtime_type_to_c(*type_);

            let rvalue_pre = format!("{rvalue_type} (*{rvalue_temporary}){rvalue_postfix};\n");
            let rvalue_gen = format!("{rvalue_temporary} = {rvalue_out_value}");

            let memcpy = format!("memcpy({lvalue_temporary}, {rvalue_temporary}, {num_bytes})");

            PreStatementValue {
                pre_statements: lvalue_pre_statements + &rvalue_pre + &rvalue_pre_statements,
                out_value: formatdoc!(
                    "
                    ({{
                        {};
                        {};
                        {};
                        {};
                    }})\
                    ",
                    indent::indent_by(4, lvalue_gen),
                    indent::indent_by(4, rvalue_gen),
                    indent::indent_by(4, memcpy),
                    indent::indent_by(4, rvalue_temporary),
                ),
            }
        } else {
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
    }

    fn visit_variable_lvalue(
        &mut self,
        VariableLValue {
            name,
            name_token: _,
            id: _,
        }: &mut VariableLValue,
    ) -> Self::LValueOutput {
        PreStatementValue {
            pre_statements: "".to_string(),
            out_value: self.mangle_variable(name),
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
            out_value: format!("(({out_lvalue})[{out_rvalue}])",),
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

    fn visit_int_type(
        &mut self,
        IntType {
            token: _,
            type_: _,
            id,
        }: &mut IntType,
    ) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            *self.type_sets.get(
                *self
                    .type_data
                    .get(id)
                    .expect("type data should exist before calling c_generator"),
            ),
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
            *self.type_sets.get(
                *self
                    .type_data
                    .get(id)
                    .expect("type data should exist before calling c_generator"),
            ),
        );
        type_ + &after_id
    }

    fn visit_bool_type(&mut self, BoolType { token: _, id }: &mut BoolType) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            *self.type_sets.get(
                *self
                    .type_data
                    .get(id)
                    .expect("type data should exist before calling c_generator"),
            ),
        );
        type_ + &after_id
    }

    fn visit_idk_type(&mut self, IdkType { token: _, id }: &mut IdkType) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_c(
            *self.type_sets.get(
                *self
                    .type_data
                    .get(id)
                    .expect("type data should exist before calling c_generator"),
            ),
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
            *self.type_sets.get(
                *self
                    .type_data
                    .get(id)
                    .expect("type data should exist before calling c_generator"),
            ),
        );
        type_ + &after_id
    }
}
