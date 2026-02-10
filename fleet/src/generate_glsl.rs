use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{HashMap, HashSet},
    error::Error,
    rc::Rc,
};

use indent::{indent_all_by, indent_by};
use indoc::formatdoc;

use itertools::Itertools;
use log::warn;

#[cfg(feature = "gpu_backend")]
use crate::ast::AstNodeRef;
use crate::{
    ast::{
        AliasType, ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, BreakStatement, CastExpression,
        CompilerExpression, ExpressionStatement, ExternFunctionBody, ForLoopStatement,
        FunctionCallExpression, FunctionDefinition, GPUExecutor, GroupingExpression,
        GroupingLValue, HasID, IdkType, IfStatement, LiteralExpression, LiteralKind, OnStatement,
        OnStatementIterator, Program, ReturnStatement, SelfExecutorHost, SimpleBinding, SimpleType,
        SkipStatement, Statement, StatementFunctionBody, StructAccessExpression,
        StructAccessLValue, StructExpression, StructMemberDefinition, StructMemberValue,
        StructType, ThreadExecutor, TopLevelStatement, TypeAlias, UnaryExpression, UnaryOperation,
        UnitType, VariableAccessExpression, VariableAssignmentExpression,
        VariableDefinitionStatement, VariableLValue, WhileLoopStatement,
    },
    error_reporting::{
        Backend, ErrorKind, ErrorSeverity, Errors, GpuLimitation, InternalError, Intrinsic,
        UnresolvedSymbol,
    },
    passes::{
        find_node_bounds::find_node_bounds,
        pass_manager::{
            ConcreteFunctionData, ConcreteScopeData, ConcreteTypeData, ConcreteVariableData,
            GlobalState, Pass, PassError, PassFactory, PassResult, PrecompiledGlslFunctions,
            StatData, StructAliasMap,
        },
        runtime_type::ConcreteRuntimeType,
        scope_analysis::{ConcreteFunction, ConcreteVariable},
    },
};

type Result<T> = ::core::result::Result<T, Box<dyn Error>>;

pub struct GLSLCodeGenerator<'state> {
    errors: RefMut<'state, Errors>,
    program: Option<RefMut<'state, Program>>,

    output_functions: Option<RefMut<'state, PrecompiledGlslFunctions>>,

    variable_data: Ref<'state, ConcreteVariableData>,
    function_data: Ref<'state, ConcreteFunctionData>,
    type_data: Ref<'state, ConcreteTypeData>,

    stats: Ref<'state, StatData>,

    struct_aliases: RefCell<StructAliasMap>,
    temporary_counter: RefCell<u64>,
}

impl PassFactory for GLSLCodeGenerator<'_> {
    type Output<'state> = GLSLCodeGenerator<'state>;
    type Params = ();

    fn try_new<'state>(
        state: &'state mut GlobalState,
        _params: Self::Params,
    ) -> ::core::result::Result<Self::Output<'state>, String>
    where
        Self: Sized,
    {
        let program = state.check_named()?;
        let errors = state.check_named()?;
        let stats = state.check_named()?;

        let variable_data = state.check_named()?;
        let function_data = state.check_named()?;
        let type_data = state.check_named()?;

        let output_functions = state.insert_default();

        Ok(Self::Output {
            errors: errors.get_mut(state),
            program: Some(program.get_mut(state)),

            stats: stats.get(state),

            variable_data: variable_data.get(state),
            function_data: function_data.get(state),

            type_data: type_data.get(state),

            output_functions: Some(output_functions.get_mut(state)),

            struct_aliases: Default::default(),
            temporary_counter: RefCell::new(0),
        })
    }
}
impl Pass for GLSLCodeGenerator<'_> {
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

        let mut program = self
            .program
            .take()
            .expect("This GLSLCodeGenerator was constructed using ::new. It cannot run as a pass");

        let mut output_functions = self
            .output_functions
            .take()
            .expect("This GLSLCodeGenerator was constructed using ::new. It cannot run as a pass");

        if self
            .stats
            .get(&program.id)
            .expect("No stats available for program")
            .uses_gpu
            .at_least_maybe()
        {
            *output_functions =
                self.visit_program(&mut program)
                    .map_err(|err| PassError::CompilerError {
                        source: err,
                        producing_pass: Self::name(),
                    })?;
        } else {
            warn!("Skipping glsl pregen because nothing uses the gpu");
        }

        Ok(())
    }
}

pub struct OnStatementBindings {
    pub rw: Vec<(ConcreteRuntimeType, VariableLValue)>,
    pub ro: Vec<(ConcreteRuntimeType, Rc<RefCell<ConcreteVariable>>)>,
}

impl<'state> GLSLCodeGenerator<'state> {
    pub fn new(
        errors: RefMut<'state, Errors>,
        variable_data: Ref<'state, ConcreteVariableData>,
        function_data: Ref<'state, ConcreteFunctionData>,
        type_data: Ref<'state, ConcreteTypeData>,
        stats: Ref<'state, StatData>,
    ) -> Self {
        Self {
            errors,
            program: None,
            output_functions: None,
            variable_data,
            function_data,
            type_data,
            stats,

            temporary_counter: RefCell::new(0),
            struct_aliases: Default::default(),
        }
    }

    fn as_struct_type(&self, body: String) -> String {
        let num_structs = self.struct_aliases.borrow().len();

        self.struct_aliases
            .borrow_mut()
            .entry(body)
            .or_insert_with(|| (num_structs, self.unique_temporary("Struct")))
            .1
            .clone()
    }

    fn generate_buffer_definition(
        &self,
        mangled_name: &str,
        index: u32,
        type_: &ConcreteRuntimeType,
        modifiers: &str,
    ) -> String {
        let (pre_name, post_name) = self.runtime_type_to_glsl(type_);
        format!(
            "layout(std430, binding = {index}) buffer Input{index} {{ {modifiers} {pre_name} {mangled_name}{post_name}; }};",
        )
    }

    pub fn get_on_statement_bindings(
        variable_data: &ConcreteVariableData,
        scope_data: &ConcreteScopeData,
        type_data: &ConcreteTypeData,
        bindings: Vec<VariableLValue>,
        iterators: Vec<Rc<RefCell<ConcreteVariable>>>,
        main_body: &Statement,
    ) -> OnStatementBindings {
        let iterator_ids = iterators
            .iter()
            .map(|var| var.borrow().id)
            .collect::<HashSet<_>>();

        let r_buffer_definitions = scope_data
            .get(&main_body.get_id())
            .expect("scope data should exist before calling generate_glsl")
            .borrow()
            .vars_and_refs()
            .iter()
            .filter(|(_name, variable)| !iterator_ids.contains(&variable.borrow().id))
            .filter(|(_name, variable)| {
                // remove any rw bindings
                !bindings.iter().any(|el| {
                    variable_data
                        .get(&el.id)
                        .expect("variable data should exist before calling generate_glsl")
                        .borrow()
                        .id
                        == variable.borrow().id
                })
            })
            .map(|(_name, variable)| (variable.borrow().type_.clone(), variable.clone()))
            .collect_vec();

        let mut rw_uniqueness_set = HashSet::new();
        let rw_buffer_definitions = bindings
            .into_iter()
            .filter(|variable| {
                rw_uniqueness_set.insert(
                    variable_data
                        .get(&variable.id)
                        .expect("variable data should exist before calling generate_glsl")
                        .borrow()
                        .id,
                )
            })
            .filter(|variable| {
                !iterator_ids.contains(
                    &variable_data
                        .get(&variable.id)
                        .expect("variable data should exist before calling generate_glsl")
                        .borrow()
                        .id,
                )
            })
            .map(|binding| {
                (
                    type_data
                        .get(&binding.id)
                        .expect("type data must exist before calling c_generator")
                        .clone(),
                    binding,
                )
            })
            .collect_vec();

        OnStatementBindings {
            rw: rw_buffer_definitions,
            ro: r_buffer_definitions,
        }
    }

    pub fn generate_on_statement_shader(
        &mut self,
        bindings: &OnStatementBindings,
        main_body: &mut Statement,
        iterators: &mut [OnStatementIterator],
        gpu_executor: &mut GPUExecutor,

        PrecompiledGlslFunctions((
            glsl_functions,
            StructAliasMap(struct_aliases),
            prev_temporary_counter,
        )): PrecompiledGlslFunctions,
    ) -> Result<String> {
        let GPUExecutor {
            host: _,
            dot_token: _,
            gpus_token: _,
            open_bracket_token: _,
            gpu_index: _,
            close_bracket_token: _,
            id: _,
        } = gpu_executor;

        self.struct_aliases.borrow_mut().extend(struct_aliases);
        *self.temporary_counter.borrow_mut() = prev_temporary_counter;

        let iterator_sizes_str = "iterator_sizes";

        let body_str = match self.visit_statement(main_body) {
            Ok(body_str) => body_str,
            Err(err) => {
                self.errors.push(ErrorKind::InternalError(
                    InternalError::GlslGenerationFailed {
                        statement: find_node_bounds(&*main_body),
                        error: err.to_string(),
                    },
                ));
                return Err(err);
            }
        };

        let num_iterators = iterators.len();
        let glsl_iterators = iterators
            .iter_mut()
            .enumerate()
            .map(|(n, it)| {
                let type_ = self.runtime_type_to_glsl(
                    self.type_data
                        .get(&it.binding.id)
                        .expect("type data must exist before calling glsl_generator"),
                );
                format!(
                    "{} = {}(((gl_GlobalInvocationID.x) {}) {});",
                    self.visit_simple_binding(&mut it.binding),
                    type_.0 + &type_.1,
                    if n == 0 {
                        "".to_string()
                    } else {
                        format!(
                            "/ ({})",
                            (0..n)
                                .map(|i| format!("{iterator_sizes_str}[{i}]"))
                                .reduce(|a, b| format!("(({a}) * ({b}))"))
                                .unwrap_or("1".to_string())
                        )
                    },
                    if n == num_iterators - 1 {
                        "".to_string()
                    } else {
                        format!("% {iterator_sizes_str}[{n}]")
                    }
                )
            })
            .join("\n");

        let mut buffer_count = 0;

        let rw_buffer_definitions = bindings
            .rw
            .iter()
            .map(|(type_, binding)| {
                let ref_var = self.variable_data.get(&binding.id).unwrap();

                let res = self.generate_buffer_definition(
                    &self.mangle_variable(&ref_var.borrow()),
                    buffer_count,
                    type_,
                    "",
                );
                buffer_count += 1;
                res
            })
            .join("\n");

        let ro_buffer_definitions = bindings
            .ro
            .iter()
            .map(|(type_, variable)| {
                let res = self.generate_buffer_definition(
                    &self.mangle_variable(&variable.borrow()),
                    buffer_count,
                    type_,
                    "readonly",
                );
                buffer_count += 1;
                res
            })
            .join("\n");

        let num_iterators = iterators.len();

        let (function_declarations, function_definitions): (Vec<_>, Vec<_>) = self
            .stats
            .get(&main_body.get_id())
            .unwrap()
            .accessed_items
            .functions
            .iter()
            .map(|f| {
                let fid = f.borrow().id;
                Ok(glsl_functions.get(&fid).cloned().ok_or_else(|| {
                    format!(
                        "Function {:?} is used but not available on the gpu",
                        f.borrow().symbol
                    )
                })?)
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .unzip();

        let struct_aliases = self
            .struct_aliases
            .borrow()
            .iter()
            .sorted_by_key(|(_body, (i, _name))| *i)
            .map(|(body, (_i, name))| format!("struct {name} {{\n{}\n}};", indent_all_by(4, body)))
            .join("\n");

        let function_declarations = function_declarations.join("\n");
        let function_definitions = function_definitions.join("\n");

        let unescaped_glsl = formatdoc! {
            "
            #version 430
            layout(local_size_x = 256) in;

            // https://github.com/Darkyenus/glsl4idea/issues/175
            #extension GL_EXT_shader_explicit_arithmetic_types         : enable

            highp float;

            // structs
            {struct_aliases}

            // declarations
            {function_declarations}
            // definitions
            {function_definitions}

            // Read + Write buffers
            {rw_buffer_definitions}
            // Read only buffers
            {ro_buffer_definitions}

            // iterator size buffer
            layout(std430, binding = {buffer_count}) buffer Input{buffer_count} {{
                readonly uint32_t {iterator_sizes_str}[{num_iterators}];
            }};

            layout(push_constant) uniform PushConstants {{
                uint dispatch_size;
            }};

            void main() {{
                if (gl_GlobalInvocationID.x >= dispatch_size) {{
                    return;
                }}
                {}
                {}
            }}
            ",
            indent_by(4, glsl_iterators),
            indent_by(4, body_str),
        };

        Ok(unescaped_glsl)
    }

    #[cfg(feature = "gpu_backend")]
    pub fn compile_on_statement_shader<'node, I: Into<AstNodeRef<'node>>>(
        mut self,
        source: &str,
        error_node: I,
    ) -> Result<shaderc::CompilationArtifact> {
        let compiler = shaderc::Compiler::new().unwrap();
        let mut options = shaderc::CompileOptions::new().unwrap();
        options.set_generate_debug_info();
        Ok(compiler
            .compile_into_spirv(
                source,
                shaderc::ShaderKind::Compute,
                "fleet_temporary.comp",
                "main",
                Some(&options),
            )
            .inspect_err(|err| {
                self.errors
                    .push(ErrorKind::InternalError(InternalError::ShadercError {
                        statement: find_node_bounds(error_node),
                        glsl: source.to_string(),
                        error: err.to_string(),
                    }));
            })?)
    }

    fn runtime_type_to_glsl(&self, type_: &ConcreteRuntimeType) -> (String, String) {
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
                let (type_, after_id) = self.runtime_type_to_glsl(subtype);
                (type_, format!("[{size}]{after_id}"))
            }
            ConcreteRuntimeType::Struct {
                members,
                source_hash: _,
            } => (
                self.as_struct_type(
                    members
                        .iter()
                        .map(|(member, type_)| {
                            let (type_, after_id) = self.runtime_type_to_glsl(type_);
                            format!("{type_} {member}{after_id};")
                        })
                        .join("\n"),
                ),
                "".to_string(),
            ),
        }
    }
    #[allow(unused)]
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
                let (type_, after_id) = self.runtime_type_to_glsl(&param.borrow().type_);
                type_ + " " + &self.mangle_variable(&param.borrow()) + &after_id
            })
            .join(", ");

        let (type_, after_id) = self.runtime_type_to_glsl(&function.return_type);
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

#[derive(Default)]
pub struct PreStatementValue {
    pub pre_statements: String,
    pub out_value: String,
}

impl AstVisitor for GLSLCodeGenerator<'_> {
    type ProgramOutput = Result<PrecompiledGlslFunctions>;
    type TopLevelOutput = Result<Option<(String, String)>>;
    type FunctionBodyOutput = Result<String>;
    type SimpleBindingOutput = String;
    type StatementOutput = Result<String>;
    type ExecutorHostOutput = String;
    type ExecutorOutput = String;
    type ExpressionOutput = PreStatementValue;
    type LValueOutput = PreStatementValue;
    type TypeOutput = String;

    fn visit_program(mut self, program: &mut Program) -> Self::ProgramOutput {
        Ok(PrecompiledGlslFunctions((
            program
                .top_level_statements
                .iter_mut()
                .filter_map(|tls| {
                    if let TopLevelStatement::Function(_) = tls {
                        let id = self.function_data.get(&tls.get_id()).unwrap().borrow().id;
                        let tls_generated = match self.visit_top_level_statement(tls) {
                            Ok(x) => x,
                            Err(err) => return Some(Err(err)),
                        };
                        Some(Ok((id, tls_generated)))
                    } else {
                        None
                    }
                })
                .filter_map(|res| {
                    let (fid, opt) = match res {
                        Ok(ok) => ok,
                        Err(err) => return Some(Err(err)),
                    };

                    opt.map(|some| Ok((fid, some)))
                })
                .collect::<Result<HashMap<_, _>>>()?,
            self.struct_aliases.borrow().clone(),
            self.temporary_counter.into_inner(),
        )))
    }

    fn visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token: _,
            name,
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
            .expect("Functions should be tracked before calling glsl_generator");

        if self.stats.get(id).unwrap().uses_gpu.at_least_maybe() {
            warn!("Not generating GLSL for function {name:?} because it may use the gpu.");
            return Ok(None);
        }

        let declaration =
            self.generate_function_declaration(&function.borrow().clone(), name != "main");

        Ok(Some((
            declaration.clone() + ";",
            declaration + self.visit_function_body(body)?.as_str(),
        )))
    }

    fn visit_type_alias(&mut self, _type_alias: &mut TypeAlias) -> Self::TopLevelOutput {
        // type aliases get completely compiled out and aren't relevant after type propagation
        Ok(None)
    }

    fn visit_statement_function_body(
        &mut self,
        StatementFunctionBody { statement, id: _ }: &mut StatementFunctionBody,
    ) -> Self::FunctionBodyOutput {
        Ok(format!("{{{}}}", self.visit_statement(statement)?))
    }

    fn visit_extern_function_body(
        &mut self,
        ExternFunctionBody {
            at_token: _,
            extern_token: _,
            symbol: _,
            symbol_token: _,
            semicolon_token: _,
            id,
        }: &mut ExternFunctionBody,
    ) -> Self::FunctionBodyOutput {
        self.errors.push(ErrorKind::GpuLimitationUsed(
            GpuLimitation::ExternalFunction {
                function: self.function_data.get(id).unwrap().borrow().symbol.clone(),
            },
        ));
        Err("Tried using external function from gpu".into())
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
            .expect("Bindings should have types before calling glsl_generator");

        let (type_, after_id) = self.runtime_type_to_glsl(inferred_type);

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
        Ok(format!("{pre_statements}{out_value};"))
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::StatementOutput {
        self.errors
            .push(ErrorKind::GpuLimitationUsed(GpuLimitation::OnStatement {
                statement: find_node_bounds(&*on_stmt),
            }));
        Err("Tried using on-statement on GPU".into())
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
        Ok(formatdoc!(
            "
            {{
                {}
            }}\
            ",
            indent::indent_by(
                4,
                body.iter_mut()
                    .flat_map(|stmt| self.visit_statement(stmt))
                    .join("\n"),
            )
        ))
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
        Ok(format!("{pre_statements}return {out_value};"))
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
            .expect("var data must exist before calling glsl_generator")
            .clone();

        let type_ = &ref_var.borrow().type_;

        if let ConcreteRuntimeType::ArrayOf { subtype: _, size } = type_ {
            let lvalue_gen = self.visit_simple_binding(binding);
            let lvalue_temporary = self.mangle_variable(&ref_var.borrow());

            let rvalue_temporary = self.unique_temporary("rvalue");
            let PreStatementValue {
                pre_statements: rvalue_pre_statements,
                out_value: rvalue_out_value,
            } = self.visit_expression(&mut *value);
            let (rvalue_type, rvalue_postfix) = self.runtime_type_to_glsl(type_);

            let rvalue_gen =
                format!("{rvalue_type} {rvalue_temporary}{rvalue_postfix} = {rvalue_out_value}");

            let iterator = self.unique_temporary("i");

            let memcpy = formatdoc! {
                "
                for (int {iterator} = 0; {iterator} < {size}; {iterator}++) {{
                    {lvalue_temporary}[{iterator}] = {rvalue_temporary}[{iterator}];
                }}
                "
            };

            Ok(formatdoc!(
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
            ))
        } else {
            let PreStatementValue {
                pre_statements,
                out_value,
            } = self.visit_expression(&mut *value);
            Ok(format!(
                "{pre_statements}{} = ({out_value});",
                self.visit_simple_binding(binding),
            ))
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
                Ok((
                    pre_statements,
                    formatdoc!(
                        "
                        else if ({out_value}) {{
                            {}
                        }}\
                        ",
                        indent::indent_by(4, self.visit_statement(body)?)
                    ),
                ))
            })
            .collect::<Result<Vec<_>>>()?
            .iter()
            .cloned()
            .unzip();

        Ok(formatdoc!(
            "
            {pre_statements}{}if ({out_value}) {{
                {}
            }}
            {}\
            ",
            elif_prestatements.concat(),
            indent::indent_by(4, self.visit_statement(&mut *if_body)?),
            elifs.join("\n")
        ) + &else_.as_mut().map_or(Ok(String::new()), |(_token, body)| {
            self.visit_statement(&mut *body).map(|stmt| {
                formatdoc!(
                    "
                else {{
                    {}
                }}\
                ",
                    stmt
                )
            })
        })?)
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
        Ok(formatdoc!(
            "
            {pre_statements}while ({out_value}) {{
                {}
            }}\
            ",
            indent::indent_by(4, self.visit_statement(&mut *body)?)
        ))
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
        Ok(formatdoc!(
            "
            {cond_pre_statements}{inc_pre_statements}for ({} {cond_out_value}; {inc_out_value}) {{
                {}
            }}\
            ",
            self.visit_statement(initializer)?,
            indent::indent_by(4, self.visit_statement(&mut *body)?)
        ))
    }

    fn visit_break_statement(
        &mut self,
        BreakStatement {
            break_token: _,
            semicolon_token: _,
            id: _,
        }: &mut BreakStatement,
    ) -> Self::StatementOutput {
        Ok("break;".to_string())
    }

    fn visit_skip_statement(
        &mut self,
        SkipStatement {
            skip_token: _,
            semicolon_token: _,
            id: _,
        }: &mut SkipStatement,
    ) -> Self::StatementOutput {
        Ok("continue;".to_string())
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
            id: _,
        }: &mut LiteralExpression,
    ) -> Self::ExpressionOutput {
        PreStatementValue {
            pre_statements: "".to_string(),
            out_value: match value {
                LiteralKind::Number(value) => value.to_string(),
                LiteralKind::Char(value) => (*value as u8).to_string(),
                LiteralKind::Float(value) => value.to_string(),
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
            .expect("Array expressions should have types before calling glsl_generator");

        let (type_, after_id) = self.runtime_type_to_glsl(inferred_type);

        let ConcreteRuntimeType::ArrayOf { subtype, size: _ } = inferred_type else {
            unreachable!("array expressions must have type ArrayOf(_)")
        };

        if let ConcreteRuntimeType::ArrayOf { subtype: _, size } = &**subtype {
            let size = *size;

            let (rvalue_type, rvalue_postfix) = self.runtime_type_to_glsl(subtype);

            let (pre_statements, definitions, temporaries): (Vec<_>, Vec<_>, Vec<_>) = elements
                .iter_mut()
                .map(|(element, _comma)| {
                    let rvalue_temporary = self.unique_temporary("element");
                    let PreStatementValue {
                        pre_statements,
                        out_value,
                    } = self.visit_expression(element);

                    let rvalue_gen = format!(
                        "{rvalue_type} {rvalue_temporary}{rvalue_postfix} = {out_value};\n"
                    );

                    (pre_statements, rvalue_gen, rvalue_temporary)
                })
                .multiunzip();

            let out_temporary = self.unique_temporary("out");

            PreStatementValue {
                pre_statements: format!(
                    "{pre_statements}{type_} {out_temporary}{after_id};\n",
                    pre_statements = indent::indent_by(4, pre_statements.concat()),
                ),
                out_value: formatdoc!(
                    "
                    ({{
                        {definitions}
                        {out_temporary} = {type_}{after_id}({elements});
                        {out_temporary};
                    }})\
                    ",
                    definitions = indent::indent_by(4, definitions.concat()),
                    elements = indent::indent_by(
                        4,
                        temporaries
                            .iter()
                            .flat_map(|tmp| {
                                (0..size).map(|i| format!("({tmp})[{i}])")).collect_vec()
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
                out_value: format!("{}{}({})", type_, after_id, elements.join(", ")),
            }
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

        let (type_, after_id) = self.runtime_type_to_glsl(inferred_type);

        let ConcreteRuntimeType::Struct { .. } = inferred_type else {
            unreachable!("struct expressions must have type Struct(_)")
        };

        let (pre_statements, values): (Vec<_>, Vec<_>) = members
            .iter_mut()
            .map(
                |(
                    StructMemberValue {
                        name: _,
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

                    (pre_statements, out_value)
                },
            )
            .unzip();

        PreStatementValue {
            pre_statements: pre_statements.join("\n"),
            out_value: format!("({type_}{after_id}({}))", values.join(",\n")),
        }
    }

    fn visit_function_call_expression(
        &mut self,
        expr: &mut FunctionCallExpression,
    ) -> Self::ExpressionOutput {
        let FunctionCallExpression {
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

        if self.stats.get(id).unwrap().uses_gpu.at_least_maybe() {
            self.errors
                .push(ErrorKind::GpuLimitationUsed(GpuLimitation::GpuFunction {
                    function: self
                        .function_data
                        .get(id)
                        .unwrap()
                        .borrow()
                        .symbol
                        .clone()
                        .with_use(name_token.range.clone()),
                }));
        }

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

        let (_pre_statements, args): (Vec<_>, Vec<_>) = arguments
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
                    .expect("type data must exist before calling glsl_generator");
                let (type_, after_id) = self.runtime_type_to_glsl(expected_type);

                if expected_type.is_numeric() {
                    PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("({type_}{after_id}(0))"),
                    }
                } else if expected_type.is_boolean() {
                    PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: "false".to_string(),
                    }
                } else if let ConcreteRuntimeType::ArrayOf { subtype, size } = expected_type {
                    if !subtype.is_numeric() {
                        self.errors.push(ErrorKind::ComplexZeroGlsl {
                            zero: UnresolvedSymbol::from_token(name.clone(), name_token),
                            type_: expected_type.clone().into(),
                        });
                    }

                    let tmp = self.unique_temporary("zero");
                    PreStatementValue {
                        pre_statements: formatdoc!(
                            "
                            {type_} {tmp}{after_id};
                            for (int i = 0; i < {size}; i++) {{
                                {tmp}[i] = 0;
                            }}
                            "
                        ),
                        out_value: format!("(&{tmp})"),
                    }
                } else {
                    self.errors.push(ErrorKind::InvalidIntrinsicType {
                        backend: Backend::Glsl,
                        intrinsic: Intrinsic::Zero,
                        intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                        type_: expected_type.clone().into(),
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
                    .expect("type data must exist before calling glsl_generator");

                match expected_type {
                    ConcreteRuntimeType::F32 | ConcreteRuntimeType::F64 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(sqrt({}))", args.first().unwrap()),
                    },
                    _ => {
                        self.errors.push(ErrorKind::InvalidIntrinsicType {
                            backend: Backend::Glsl,
                            intrinsic: Intrinsic::Sqrt,
                            intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                            type_: expected_type.clone().into(),
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
                    .expect("type data must exist before calling glsl_generator");

                match expected_type {
                    ConcreteRuntimeType::F32 | ConcreteRuntimeType::F64 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(sin({}))", args.first().unwrap()),
                    },
                    _ => {
                        self.errors.push(ErrorKind::InvalidIntrinsicType {
                            backend: Backend::Glsl,
                            intrinsic: Intrinsic::Sin,
                            intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                            type_: expected_type.clone().into(),
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
                    .expect("type data must exist before calling glsl_generator");

                match expected_type {
                    ConcreteRuntimeType::F32 | ConcreteRuntimeType::F64 => PreStatementValue {
                        pre_statements: "".to_string(),
                        out_value: format!("(cos({}))", args.first().unwrap()),
                    },
                    _ => {
                        self.errors.push(ErrorKind::InvalidIntrinsicType {
                            backend: Backend::Glsl,
                            intrinsic: Intrinsic::Cos,
                            intrinsic_sym: UnresolvedSymbol::from_token(name.clone(), name_token),
                            type_: expected_type.clone().into(),
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
                    .expect("type data must exist before calling glsl_generator");

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
            _ => {
                self.errors.push(ErrorKind::IntrinsicNotImplemented {
                    backend: Backend::Glsl,
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
            out_value: format!("(({array_out_value})[{index_out_value}])"),
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
        let ref_var = self
            .variable_data
            .get(id)
            .expect("var data must exist before calling glsl_generator")
            .clone();

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
        let (type_, after_id) = self.runtime_type_to_glsl(
            self.type_data
                .get(id)
                .expect("types should be inferred before calling glsl_generator"),
        );

        let PreStatementValue {
            pre_statements,
            out_value,
        } = self.visit_expression(&mut *operand);

        PreStatementValue {
            pre_statements,
            out_value: format!("({type_}{after_id}({out_value}))"),
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

        use BinaryOperation as Bo;
        PreStatementValue {
            pre_statements: left_pre_statements + &right_pre_statements,
            out_value: match operation {
                Bo::Add => format!("(({left_out_value}) + ({right_out_value}))"),
                Bo::Subtract => format!("(({left_out_value}) - ({right_out_value}))"),
                Bo::Multiply => format!("(({left_out_value}) * ({right_out_value}))"),
                Bo::Divide => format!("(({left_out_value}) / ({right_out_value}))"),
                Bo::Modulo => {
                    if self
                        .type_data
                        .get(&left.get_id())
                        .expect("type data must exist before calling glsl_generator")
                        .is_float()
                    {
                        format!("(mod(({left_out_value}), ({right_out_value})))")
                    } else {
                        format!("(({left_out_value}) % ({right_out_value}))")
                    }
                }
                Bo::GreaterThan => format!("(({left_out_value}) > ({right_out_value}))"),
                Bo::GreaterThanOrEqual => format!("(({left_out_value}) >= ({right_out_value}))"),
                Bo::LessThan => format!("(({left_out_value}) < ({right_out_value}))"),
                Bo::LessThanOrEqual => format!("(({left_out_value}) <= ({right_out_value}))"),
                Bo::Equal => format!("(({left_out_value}) == ({right_out_value}))"),
                Bo::NotEqual => format!("(({left_out_value}) != ({right_out_value}))"),
                Bo::LogicalAnd => format!("(({left_out_value}) && ({right_out_value}))"),
                Bo::LogicalOr => format!("(({left_out_value}) || ({right_out_value}))"),
            },
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
        let type_ = self
            .type_data
            .get(id)
            .expect("Types must exist before calling glsl_generator");

        if let ConcreteRuntimeType::ArrayOf { subtype: _, size } = type_ {
            let size = *size;

            let (value_type, value_postfix) = self.runtime_type_to_glsl(type_);

            let lvalue_temporary = self.unique_temporary("lvalue");
            let PreStatementValue {
                pre_statements: lvalue_pre_statements,
                out_value: lvalue,
            } = self.visit_lvalue(lvalue);
            let lvalue = format!("({lvalue})");

            let lvalue_gen = format!("{value_type} ({lvalue_temporary}){value_postfix} = {lvalue}");

            let rvalue_temporary = self.unique_temporary("rvalue");
            let PreStatementValue {
                pre_statements: rvalue_pre_statements,
                out_value: rvalue_out_value,
            } = self.visit_expression(&mut *right);

            let rvalue_pre = format!("{value_type} {rvalue_temporary}{value_postfix};\n");
            let rvalue_gen = format!("{rvalue_temporary} = {rvalue_out_value}");

            let iterator = self.unique_temporary("i");

            let memcpy = formatdoc! {
                "
                for (int {iterator} = 0; {iterator} < {size}; {iterator}++) {{
                    {lvalue_temporary}[{iterator}] = {rvalue_temporary}[{iterator}];
                }}
                "
            };

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
                out_value: format!("(({out_lvalue}) = ({out_rvalue}))"),
            }
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
            out_value: format!("(({out_lvalue})[{out_rvalue}])",),
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
        let (type_, after_id) = self.runtime_type_to_glsl(
            self.type_data
                .get(id)
                .expect("type data should exist before calling glsl_generator"),
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
        let (type_, after_id) = self.runtime_type_to_glsl(
            self.type_data
                .get(id)
                .expect("type data should exist before calling glsl_generator"),
        );
        type_ + &after_id
    }

    fn visit_idk_type(&mut self, IdkType { token: _, id }: &mut IdkType) -> Self::TypeOutput {
        let (type_, after_id) = self.runtime_type_to_glsl(
            self.type_data
                .get(id)
                .expect("type data should exist before calling glsl_generator"),
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
        let (type_, after_id) = self.runtime_type_to_glsl(
            self.type_data
                .get(id)
                .expect("type data should exist before calling glsl_generator"),
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
            "struct {} {{ {} }}",
            self.unique_temporary("StructType"),
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

                        let (type_, after_id) = self.runtime_type_to_glsl(inferred_type);
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
        let (type_, after_id) = self.runtime_type_to_glsl(
            self.type_data
                .get(id)
                .expect("type data should exist before calling glsl_generator"),
        );
        type_ + &after_id
    }
}
