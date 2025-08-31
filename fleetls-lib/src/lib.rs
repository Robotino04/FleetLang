use fleet::{
    ast::{
        ArrayExpression, ArrayIndexExpression, ArrayIndexLValue, ArrayType, AstNode, AstVisitor,
        BinaryExpression, BinaryOperation, BlockStatement, CastExpression, CompilerExpression,
        ExpressionStatement, ExternFunctionBody, ForLoopStatement, FunctionCallExpression,
        FunctionDefinition, GPUExecutor, GroupingExpression, GroupingLValue, HasID, IdkType,
        IfStatement, LiteralExpression, LiteralKind, NodeID, OnStatement, ReturnStatement,
        SelfExecutorHost, SimpleBinding, SimpleType, StatementFunctionBody, ThreadExecutor,
        UnaryExpression, UnaryOperation, UnitType, VariableAccessExpression,
        VariableAssignmentExpression, VariableDefinitionStatement, VariableLValue,
        WhileLoopStatement,
    },
    infra::{
        self, ErrorSeverity, insert_c_passes, insert_compile_passes, insert_fix_passes,
        insert_minimal_pipeline,
    },
    passes::{
        find_containing_node::FindContainingNodePass,
        pass_manager::{
            Errors, FunctionData, InputSource, PassError, PassManager, ScopeData, StatData,
            TypeData, TypeSets, VariableData,
        },
        scope_analysis::ComptimeDeps,
    },
    tokenizer::{SourceLocation, Token},
};
use indoc::indoc;
use itertools::Itertools;
use std::{
    collections::HashMap,
    sync::{
        Arc, LazyLock, Mutex,
        atomic::{AtomicBool, Ordering},
    },
};
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::{Client, LanguageServer, lsp_types::*};

use crate::extract_semantic_tokens_pass::ExtractSemanticTokensPass;

mod extract_semantic_tokens_pass;

pub extern crate fleet;
pub extern crate tower_lsp_server;

pub struct BackgroundThreadState {
    pub semantic_tokens_refresh: AtomicBool,
}

pub struct Backend {
    pub client: Client,
    pub documents: std::sync::RwLock<HashMap<Uri, String>>,
    pub background_state: Arc<Mutex<BackgroundThreadState>>,
}

static SEMANTIC_TOKEN_TYPES: LazyLock<Vec<SemanticTokenType>> = std::sync::LazyLock::new(|| {
    vec![
        SemanticTokenType::CLASS,
        SemanticTokenType::COMMENT,
        SemanticTokenType::DECORATOR,
        SemanticTokenType::ENUM,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::EVENT,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::INTERFACE,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::MACRO,
        SemanticTokenType::METHOD,
        SemanticTokenType::MODIFIER,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::NUMBER,
        SemanticTokenType::OPERATOR,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::REGEXP,
        SemanticTokenType::STRING,
        SemanticTokenType::STRUCT,
        SemanticTokenType::TYPE,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::VARIABLE,
    ]
});
static SEMANTIC_TOKEN_MODIFIERS: LazyLock<Vec<SemanticTokenModifier>> =
    std::sync::LazyLock::new(|| {
        vec![
            SemanticTokenModifier::ABSTRACT,
            SemanticTokenModifier::ASYNC,
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFAULT_LIBRARY,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::DEPRECATED,
            SemanticTokenModifier::DOCUMENTATION,
            SemanticTokenModifier::MODIFICATION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::STATIC,
        ]
    });

#[derive(Copy, Clone, Debug)]
struct AnalysisData<'a> {
    type_data: &'a TypeData,
    type_sets: &'a TypeSets,
    function_data: &'a FunctionData,
    variable_data: &'a VariableData,
    scope_data: &'a ScopeData,
    comptime_deps: &'a ComptimeDeps,
}

impl Backend {
    fn get_type_as_hover(&self, id: NodeID, analysis_data: Option<AnalysisData>) -> String {
        let Some(AnalysisData {
            type_data,
            type_sets,
            function_data: _,
            variable_data: _,
            scope_data: _,
            comptime_deps: _,
        }) = analysis_data
        else {
            return "/* No type data available */".to_string();
        };
        let Some(type_) = type_data.get(&id) else {
            return "/* Node type missing */".to_string();
        };
        type_sets.get(*type_).stringify(type_sets)
    }
    fn generate_node_hover(
        &self,
        node: impl Into<AstNode>,
        analysis_data: Option<AnalysisData>,
    ) -> (String, String) {
        match node.into() {
            AstNode::Program(_) => ("".to_string(), "program".to_string()),
            AstNode::FunctionDefinition(FunctionDefinition {
                let_token: _,
                name,
                name_token: _,
                equal_token: _,
                open_paren_token: _,
                parameters,
                close_paren_token: _,
                right_arrow_token: _,
                return_type,
                body: _,
                id,
            }) => (
                format!(
                    "let {name} = ({}) -> {}",
                    Itertools::intersperse(
                        parameters.iter().map(|(param, _comma)| self
                            .generate_node_hover(param.clone(), analysis_data)
                            .0),
                        ", ".to_string()
                    )
                    .collect::<String>(),
                    return_type
                        .map(|t| self.generate_node_hover(t, analysis_data).0)
                        .unwrap_or_else(|| {
                            let Some(AnalysisData {
                                type_data: _,
                                type_sets,
                                function_data,
                                variable_data: _,
                                scope_data: _,
                                comptime_deps: _,
                            }) = analysis_data
                            else {
                                return "/* No type data available */".to_string();
                            };
                            let Some(ref_func) = function_data.get(&id) else {
                                return "/* Function doesn't exist */".to_string();
                            };
                            let Some(return_type) = ref_func.borrow().return_type else {
                                return "/* Not processed by ScopeAnalyzer */".to_string();
                            };
                            let return_type = type_sets.get(return_type);
                            return_type.stringify(type_sets)
                        })
                ),
                "function definition".to_string(),
            ),
            AstNode::ExternFunctionBody(ExternFunctionBody {
                at_token: _,
                extern_token: _,
                symbol,
                symbol_token: _,
                semicolon_token: _,
                id: _,
            }) => (
                format!("@extern \"{symbol}\""),
                "extern function body".to_string(),
            ),
            AstNode::StatementFunctionBody(StatementFunctionBody { statement, id: _ }) => (
                self.generate_node_hover(statement, analysis_data).0,
                "statement function body".to_string(),
            ),
            AstNode::SimpleBinding(SimpleBinding {
                name_token: _,
                name,
                type_,
                id,
            }) => (
                format!(
                    "{name}: {}",
                    type_
                        .map(|(_colon, type_)| self.generate_node_hover(type_, analysis_data).0)
                        .unwrap_or_else(|| {
                            let Some(AnalysisData {
                                type_data: _,
                                type_sets,
                                function_data: _,
                                variable_data,
                                scope_data: _,
                                comptime_deps: _,
                            }) = analysis_data
                            else {
                                return "/* No type data available */".to_string();
                            };
                            let Some(ref_var) = variable_data.get(&id) else {
                                return "/* Variable doesn't exist */".to_string();
                            };
                            let Some(type_) = ref_var.borrow().type_ else {
                                return "/* Variable wasn't processed by ScopeAnalyzer */"
                                    .to_string();
                            };

                            let type_ = type_sets.get(type_);
                            type_.stringify(type_sets)
                        })
                ),
                "simple binding".to_string(),
            ),
            AstNode::ExpressionStatement(ExpressionStatement {
                expression,
                semicolon_token: _,
                id: _,
            }) => (
                self.generate_node_hover(expression, analysis_data).0,
                "expression statement".to_string(),
            ),
            AstNode::OnStatement(OnStatement {
                on_token: _,
                executor,
                iterators,
                open_paren_token: _,
                bindings,
                close_paren_token: _,
                body: _,
                id: _,
            }) => (
                format!(
                    "on {}{} ({})",
                    self.generate_node_hover(executor, analysis_data).0,
                    iterators
                        .iter()
                        .map(|it| format!(
                            "[{} = {}]",
                            self.generate_node_hover(it.binding.clone(), analysis_data)
                                .0,
                            self.generate_node_hover(it.max_value.clone(), analysis_data)
                                .0,
                        ))
                        .collect::<String>(),
                    bindings
                        .iter()
                        .map(|(binding, _comma)| {
                            self.generate_node_hover(binding.clone(), analysis_data).0
                        })
                        .join(", "),
                ),
                "`on` statement".to_string(),
            ),
            AstNode::BlockStatement(BlockStatement { .. }) => ("".to_string(), "block".to_string()),
            AstNode::ReturnStatement(ReturnStatement { id, .. }) => (
                format!("return {}", self.get_type_as_hover(id, analysis_data)),
                "`return` statement".to_string(),
            ),
            AstNode::VariableDefinitionStatement(VariableDefinitionStatement {
                let_token: _,
                binding,
                equals_token: _,
                value: _,
                semicolon_token: _,
                id: _,
            }) => (
                format!(
                    "let {} = ...", // TODO: once we have consteval, display that here
                    self.generate_node_hover(binding, analysis_data).0
                ),
                "variable definition".to_string(),
            ),
            AstNode::IfStatement(IfStatement { .. }) => {
                ("".to_string(), "`if` statement".to_string())
            }
            AstNode::WhileLoopStatement(WhileLoopStatement { .. }) => {
                ("".to_string(), "`while` loop".to_string())
            }
            AstNode::ForLoopStatement(ForLoopStatement {
                for_token: _,
                open_paren_token: _,
                initializer,
                condition,
                second_semicolon_token: _,
                incrementer,
                close_paren_token: _,
                body: _,
                id: _,
            }) => {
                let condition_type = condition
                    .map(|cond| self.get_type_as_hover(cond.get_id(), analysis_data))
                    .unwrap_or("".to_string());
                let incrementer_type = incrementer
                    .map(|cond| self.get_type_as_hover(cond.get_id(), analysis_data))
                    .unwrap_or("".to_string());
                (
                    format!(
                        "for ({}; {}; {})",
                        self.generate_node_hover(*initializer, analysis_data).0,
                        condition_type,
                        incrementer_type
                    ),
                    "`for` loop".to_string(),
                )
            }
            AstNode::BreakStatement(_break_statement) => {
                ("".to_string(), "`break` statement".to_string())
            }
            AstNode::SkipStatement(_skip_statement) => {
                ("".to_string(), "`skip` statement".to_string())
            }
            AstNode::SelfExecutorHost(SelfExecutorHost { .. }) => {
                ("self".to_string(), "`self` executor host".to_string())
            }
            AstNode::ThreadExecutor(ThreadExecutor {
                host,
                dot_token: _,
                thread_token: _,
                open_bracket_token: _,
                index,
                close_bracket_token: _,
                id: _,
            }) => (
                format!(
                    "{}.threads[{}]",
                    self.generate_node_hover(host, analysis_data).0,
                    self.generate_node_hover(index, analysis_data).0
                ),
                "thread executor".to_string(),
            ),
            AstNode::GPUExecutor(GPUExecutor {
                host,
                dot_token: _,
                gpus_token: _,
                open_bracket_token: _,
                gpu_index,
                close_bracket_token: _,
                id: _,
            }) => (
                format!(
                    "{}.gpus[{}]",
                    self.generate_node_hover(host, analysis_data).0,
                    self.generate_node_hover(gpu_index, analysis_data).0,
                ),
                "gpu executor".to_string(),
            ),
            AstNode::UnaryExpression(UnaryExpression {
                operator_token: _,
                operation,
                operand,
                id,
            }) => (
                {
                    let inner_type = self.get_type_as_hover(operand.get_id(), analysis_data);
                    let outer_type = self.get_type_as_hover(id, analysis_data);
                    match operation {
                        UnaryOperation::BitwiseNot => {
                            format!("bitwise negation (~{inner_type}) => {outer_type}")
                        }
                        UnaryOperation::LogicalNot => {
                            format!("logical negation (!{inner_type}) => {outer_type}")
                        }
                        UnaryOperation::Negate => {
                            format!("arithmetic negation (-{inner_type}) => {outer_type}")
                        }
                    }
                },
                "unary expression".to_string(),
            ),
            AstNode::CastExpression(CastExpression {
                operand,
                as_token: _,
                type_: _,
                id,
            }) => (
                {
                    let from_type = self.get_type_as_hover(operand.get_id(), analysis_data);
                    let to_type = self.get_type_as_hover(id, analysis_data);
                    format!("{from_type} as {to_type}")
                },
                "type cast".to_string(),
            ),
            AstNode::LiteralExpression(LiteralExpression {
                value,
                token: _,
                id,
            }) => (
                match value {
                    LiteralKind::Number(value) => value.to_string(),
                    LiteralKind::Float(value) => value.to_string(),
                    LiteralKind::Bool(value) => value.to_string(),
                },
                format!("literal ({})", self.get_type_as_hover(id, analysis_data)),
            ),
            AstNode::ArrayExpression(ArrayExpression {
                open_bracket_token: _,
                elements: _,
                close_bracket_token: _,
                id,
            }) => (
                self.get_type_as_hover(id, analysis_data).to_string(),
                "array literal".to_string(),
            ),
            AstNode::BinaryExpression(BinaryExpression {
                left,
                operator_token: _,
                operation,
                right,
                id,
            }) => {
                let left_type = self.get_type_as_hover(left.get_id(), analysis_data);
                let right_type = self.get_type_as_hover(right.get_id(), analysis_data);
                let result_type = self.get_type_as_hover(id, analysis_data);
                let (name, op) = match operation {
                    BinaryOperation::Add => ("addition", "+"),
                    BinaryOperation::Subtract => ("subtraction", "-"),
                    BinaryOperation::Multiply => ("multiplication", "*"),
                    BinaryOperation::Divide => ("division", "/"),
                    BinaryOperation::Modulo => ("modulo", "%"),
                    BinaryOperation::GreaterThan => ("greater than", ">"),
                    BinaryOperation::GreaterThanOrEqual => ("greater than or equal", ">="),
                    BinaryOperation::LessThan => ("less than", "<"),
                    BinaryOperation::LessThanOrEqual => ("less than or equal", "<="),
                    BinaryOperation::Equal => ("equal", "=="),
                    BinaryOperation::NotEqual => ("not equal", "!="),
                    BinaryOperation::LogicalAnd => ("logical and", "&&"),
                    BinaryOperation::LogicalOr => ("logical or", "||"),
                };

                (
                    format!("{name} ({left_type} {op} {right_type} => {result_type})"),
                    "binary expression".to_string(),
                )
            }
            AstNode::GroupingExpression(GroupingExpression {
                open_paren_token: _,
                subexpression,
                close_paren_token: _,
                id: _,
            }) => (
                format!(
                    "({})",
                    self.generate_node_hover(*subexpression, analysis_data).0
                ),
                "expression grouping".to_string(),
            ),
            AstNode::FunctionCallExpression(FunctionCallExpression {
                name,
                name_token: _,
                open_paren_token: _,
                arguments: _,
                close_paren_token: _,
                id,
            }) => {
                let (parameters, return_type) = (|| {
                    let Some(AnalysisData {
                        type_data: _,
                        type_sets,
                        function_data,
                        variable_data: _,
                        scope_data: _,
                        comptime_deps: _,
                    }) = analysis_data
                    else {
                        return (
                            "/* No type data available */".to_string(),
                            "/* No type data available */".to_string(),
                        );
                    };
                    let Some(ref_func) = function_data.get(&id) else {
                        return (
                            "/* Function doesn't exist */".to_string(),
                            "/* Function doesn't exist */".to_string(),
                        );
                    };
                    let Some(return_type) = ref_func.borrow().return_type else {
                        return (
                            "/* Return type not processed by ScopeAnalyzer */".to_string(),
                            "/* Return type not processed by ScopeAnalyzer */".to_string(),
                        );
                    };
                    let return_type = type_sets.get(return_type).stringify(type_sets);
                    let parameters = if let Some(param_types) =
                        ref_func.borrow().parameter_types.as_ref()
                    {
                        Itertools::intersperse(
                            param_types.iter().map(|(param, name)| {
                                name.clone() + ": " + &type_sets.get(*param).stringify(type_sets)
                            }),
                            ", ".to_string(),
                        )
                        .collect::<String>()
                    } else {
                        "/* Param types not processed by ScopeAnalyzer */".to_string()
                    };

                    (parameters, return_type)
                })();

                (
                    format!("let {name} = ({parameters}) -> {return_type}"),
                    "function call".to_string(),
                )
            }
            AstNode::CompilerExpression(CompilerExpression {
                at_token: _,
                name,
                name_token: _,
                open_paren_token: _,
                arguments: _,
                close_paren_token: _,
                id,
            }) => {
                let (parameters, return_type) = (|| {
                    let Some(AnalysisData {
                        type_data: _,
                        type_sets,
                        function_data,
                        variable_data: _,
                        scope_data: _,
                        comptime_deps: _,
                    }) = analysis_data
                    else {
                        return (
                            "/* No type data available */".to_string(),
                            "/* No type data available */".to_string(),
                        );
                    };
                    let Some(ref_func) = function_data.get(&id) else {
                        return (
                            "/* Compiler function doesn't exist */".to_string(),
                            "/* Compiler function doesn't exist */".to_string(),
                        );
                    };
                    let Some(return_type) = ref_func.borrow().return_type else {
                        return (
                            "/* Return type not processed by ScopeAnalyzer */".to_string(),
                            "/* Return type not processed by ScopeAnalyzer */".to_string(),
                        );
                    };
                    let return_type = type_sets.get(return_type).stringify(type_sets);
                    let parameters = if let Some(param_types) =
                        ref_func.borrow().parameter_types.as_ref()
                    {
                        Itertools::intersperse(
                            param_types.iter().map(|(param, name)| {
                                name.clone() + ": " + &type_sets.get(*param).stringify(type_sets)
                            }),
                            ", ".to_string(),
                        )
                        .collect::<String>()
                    } else {
                        "/* Param types not processed by ScopeAnalyzer */".to_string()
                    };

                    (parameters, return_type)
                })();

                (
                    format!("@{name}({parameters}) -> {return_type}"),
                    "compiler expression".to_string(),
                )
            }
            AstNode::ArrayIndexExpression(ArrayIndexExpression {
                array,
                open_bracket_token: _,
                index,
                close_bracket_token: _,
                id,
            }) => (
                format!(
                    "({})[{}] => {}",
                    self.get_type_as_hover(array.get_id(), analysis_data),
                    self.get_type_as_hover(index.get_id(), analysis_data),
                    self.get_type_as_hover(id, analysis_data)
                ),
                "array index".to_string(),
            ),
            AstNode::VariableAccessExpression(VariableAccessExpression {
                name,
                name_token: _,
                id,
            }) => {
                let type_ = self.get_type_as_hover(id, analysis_data);
                // TODO: once we have consteval, display the value here
                (
                    format!("let {name}: {type_} = ..."),
                    "variable access".to_string(),
                )
            }
            AstNode::VariableAssignmentExpression(VariableAssignmentExpression {
                lvalue,
                equal_token: _,
                right: _,
                id: _,
            }) => {
                // TODO: once we have consteval, display the value here
                (
                    self.generate_node_hover(lvalue, analysis_data).0,
                    "variable assignment".to_string(),
                )
            }
            AstNode::VariableLValue(VariableLValue {
                name,
                name_token: _,
                id,
            }) => {
                // TODO: once we have consteval, display the value here
                let type_ = self.get_type_as_hover(id, analysis_data);
                (
                    format!("let {name}: {type_} = ..."),
                    "variable lvalue".to_string(),
                )
            }
            AstNode::ArrayIndexLValue(ArrayIndexLValue {
                array,
                open_bracket_token: _,
                index,
                close_bracket_token: _,
                id: _,
            }) => {
                // TODO: once we have consteval, display the value here
                (
                    format!(
                        "let {}[{}] = ...",
                        self.get_type_as_hover(array.get_id(), analysis_data),
                        self.get_type_as_hover(index.get_id(), analysis_data)
                    ),
                    "array index lvalue".to_string(),
                )
            }
            AstNode::GroupingLValue(GroupingLValue {
                open_paren_token: _,
                sublvalue,
                close_paren_token: _,
                id: _,
            }) => (
                format!(
                    "({})",
                    self.generate_node_hover(*sublvalue, analysis_data).0
                ),
                "lvalue grouping".to_string(),
            ),
            AstNode::SimpleType(SimpleType {
                token: _,
                type_,
                id: _,
            }) => (
                analysis_data
                    .map(|data| type_.stringify(data.type_sets))
                    .unwrap_or("/* No type data available */".to_string()),
                "type".to_string(),
            ),
            AstNode::UnitType(UnitType {
                open_paren_token: _,
                close_paren_token: _,
                id: _,
            }) => ("()".to_string(), "type".to_string()),
            AstNode::IdkType(IdkType { token: _, id }) => {
                let type_ = self.get_type_as_hover(id, analysis_data);
                (type_.to_string(), "idk type".to_string())
            }
            AstNode::ArrayType(ArrayType {
                subtype: _,
                open_bracket_token: _,
                size: _,
                close_bracket_token: _,
                id,
            }) => {
                let type_ = self.get_type_as_hover(id, analysis_data);
                (type_.to_string(), "array type".to_string())
            }
        }
    }

    fn full_hover_text(
        &self,
        node_hierarchy: &[AstNode],
        stats: Option<&StatData>,
        analysis_data: Option<AnalysisData>,
        hovered_token: Option<&Token>,
    ) -> String {
        format!(
            indoc! {r##"
                        ```rust
                        {}
                        ```

                        ---- Debug Stats ----
                        ```rust
                        evaluation_time: {}
                        evaluation_deps: {}

                        {}
                        ```

                        ---- Current Scope ----
                        ```rust
                        {}
                        ```

                        ---- Token ----
                        ```rust
                        {:#?}
                        ```
                    "##},
            node_hierarchy
                .last()
                .map(|node| self.generate_node_hover(node.clone(), analysis_data))
                .map_or("No AST Node".to_string(), |(info, debug)| format!(
                    "{info} // {debug}"
                )),
            analysis_data
                .as_ref()
                .and_then(|analysis| analysis.comptime_deps.get(&node_hierarchy.last()?.get_id()))
                .map_or(
                    "No evaluation time data available".to_string(),
                    |(eval_time, _deps)| format!("{eval_time:?}")
                ),
            analysis_data
                .as_ref()
                .and_then(|analysis| analysis.comptime_deps.get(&node_hierarchy.last()?.get_id()))
                .map_or(
                    "No evaluation time data available".to_string(),
                    |(_eval_time, deps)| format!("{deps:#?}")
                ),
            stats
                .as_ref()
                .and_then(|stat| stat.get(&node_hierarchy.last()?.get_id()).cloned())
                .map_or("No stats available".to_string(), |stat| format!(
                    "{stat:#?}"
                )),
            analysis_data
                .as_ref()
                .and_then(|analysis| analysis
                    .scope_data
                    .get(&node_hierarchy.last()?.get_id())
                    .cloned())
                .map_or("No scope available".to_string(), |scope| format!(
                    "{scope:#?}"
                )),
            hovered_token,
        )
    }

    fn read_document(&self, text_document: &TextDocumentIdentifier) -> Option<String> {
        Some(self.documents.read().ok()?.get(&text_document.uri)?.clone())
    }
}

impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
                    DiagnosticOptions {
                        identifier: Some("fleetls".to_string()),
                        inter_file_dependencies: false,
                        workspace_diagnostics: false,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                            legend: SemanticTokensLegend {
                                token_types: SEMANTIC_TOKEN_TYPES.clone(),
                                token_modifiers: SEMANTIC_TOKEN_MODIFIERS.clone(),
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: None,
                        will_save_wait_until: None,
                        save: None,
                    },
                )),
                document_formatting_provider: Some(OneOf::Left(true)),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "FleetLS".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, params: InitializedParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let Some(src) = self.read_document(&params.text_document) else {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::InvalidParams,
                message: format!(
                    "The requested document {:?} doesn't exist on the server.",
                    params.text_document
                )
                .into(),
                data: None,
            });
        };

        let src: &str = &src;
        let mut pm = PassManager::default();
        insert_minimal_pipeline(&mut pm);
        insert_fix_passes(&mut pm);
        insert_compile_passes(&mut pm);
        insert_c_passes(&mut pm);

        #[cfg(feature = "llvm_backend")]
        {
            use fleet::ir_generator::IrGenerator;

            pm.insert::<IrGenerator>();
            // don't optimize in the lsp
            //pm.insert::<LLVMOptimizerPass>();
        };

        let errors = pm.state.insert_default::<Errors>();
        pm.state.insert(InputSource(src.to_string()));

        if let Err(err @ PassError::CompilerError { .. }) = pm.run() {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::ServerError(0),
                message: format!("Compilation failed: {err}").into(),
                data: None,
            });
        };

        let errors = errors.get(&pm.state).0.clone();

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: errors
                        .iter()
                        .map(|error| Diagnostic {
                            range: Range {
                                start: Position {
                                    line: (error.start.line - 1) as u32,
                                    character: error.start.column as u32,
                                },
                                end: Position {
                                    line: (error.end.line - 1) as u32,
                                    character: error.end.column as u32,
                                },
                            },
                            severity: Some(match error.severity {
                                ErrorSeverity::Error => DiagnosticSeverity::ERROR,
                                ErrorSeverity::Warning => DiagnosticSeverity::WARNING,
                                ErrorSeverity::Note => DiagnosticSeverity::HINT,
                            }),
                            code: None,
                            code_description: None,
                            source: Some("FleetLS".to_string()),
                            message: error.message.clone(),
                            related_information: None,
                            tags: None,
                            data: None,
                        })
                        .collect(),
                },
            }),
        ))
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.documents.write().unwrap().insert(
            params.text_document.uri,
            params.content_changes[0].text.clone(),
        );

        self.background_state
            .lock()
            .unwrap()
            .semantic_tokens_refresh
            .store(true, Ordering::Relaxed);
    }
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.documents
            .write()
            .unwrap()
            .insert(params.text_document.uri, params.text_document.text);

        self.background_state
            .lock()
            .unwrap()
            .semantic_tokens_refresh
            .store(true, Ordering::Relaxed);
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        self.documents
            .write()
            .unwrap()
            .remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let Some(src) = self.read_document(&params.text_document_position_params.text_document)
        else {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::InvalidParams,
                message: format!(
                    "The requested document {:?} doesn't exist on the server.",
                    params.text_document_position_params.text_document
                )
                .into(),
                data: None,
            });
        };

        let mut pm = PassManager::default();
        insert_minimal_pipeline(&mut pm);
        insert_compile_passes(&mut pm);

        pm.state.insert_default::<Errors>();
        pm.state.insert(InputSource(src.to_string()));

        if let Err(err @ PassError::CompilerError { .. }) = pm.run() {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::ServerError(0),
                message: format!("Compilation failed: {err}").into(),
                data: None,
            });
        };

        let analysis_data = (
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
        );

        let analysis_data = if let (
            Some(type_data),
            Some(type_sets),
            Some(function_data),
            Some(variable_data),
            Some(scope_data),
            Some(comptime_deps),
        ) = &analysis_data
        {
            Some(AnalysisData {
                type_data,
                type_sets,
                function_data,
                variable_data,
                scope_data,
                comptime_deps,
            })
        } else {
            None
        };

        let cpos = params.text_document_position_params.position;
        let find_pass = FindContainingNodePass::new(SourceLocation {
            index: 0,
            line: cpos.line as usize + 1,
            column: cpos.character as usize,
        });

        if let Ok((node_hierarchy, hovered_token)) =
            find_pass.visit_program(&mut pm.state.get_mut().unwrap())
        {
            Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: self.full_hover_text(
                        &node_hierarchy,
                        pm.state.get().as_deref(),
                        analysis_data,
                        hovered_token.as_ref(),
                    ),
                }),
                range: hovered_token.map(|token| Range {
                    start: Position {
                        line: token.start.line as u32 - 1,
                        character: token.start.column as u32,
                    },
                    end: Position {
                        line: token.end.line as u32 - 1,
                        character: token.end.column as u32,
                    },
                }),
            }))
        } else {
            Ok(None)
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let Some(src) = self.read_document(&params.text_document) else {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::InvalidParams,
                message: format!(
                    "The requested document {:?} doesn't exist on the server.",
                    params.text_document
                )
                .into(),
                data: None,
            });
        };

        let mut pm = PassManager::default();
        insert_minimal_pipeline(&mut pm);

        pm.state.insert_default::<Errors>();
        pm.state.insert(InputSource(src.to_string()));

        if let Err(err @ PassError::CompilerError { .. }) = pm.run() {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::ServerError(0),
                message: format!("Compilation failed: {err}").into(),
                data: None,
            });
        };

        let semantic_tokens =
            ExtractSemanticTokensPass::new(&SEMANTIC_TOKEN_TYPES, &SEMANTIC_TOKEN_MODIFIERS)
                .visit_program(&mut pm.state.get_mut().unwrap());

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let Some(src) = self.read_document(&params.text_document) else {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::InvalidParams,
                message: format!(
                    "The requested document {:?} doesn't exist on the server.",
                    params.text_document
                )
                .into(),
                data: None,
            });
        };

        let doc_end = SourceLocation::end(&src);

        // even invalid input shouldn't be formatted
        let new_text = match infra::format(src) {
            Err(err) => {
                return Err(tower_lsp_server::jsonrpc::Error {
                    code: tower_lsp_server::jsonrpc::ErrorCode::ServerError(0),
                    message: format!("Compilation failed: {err}").into(),
                    data: None,
                });
            }
            Ok(new_text) => new_text,
        };

        self.documents
            .write()
            .unwrap()
            .insert(params.text_document.uri, new_text.clone());

        self.background_state
            .lock()
            .unwrap()
            .semantic_tokens_refresh
            .store(true, Ordering::Relaxed);

        Ok(Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: doc_end.line /* +1 -1 */ as u32,
                    character: 0,
                },
            },
            new_text,
        }]))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        eprintln!("{}", "-".repeat(80));
        eprintln!("{params:#?}");

        let Some(src) = self.read_document(&params.text_document_position_params.text_document)
        else {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::InvalidParams,
                message: format!(
                    "The requested document {:?} doesn't exist on the server.",
                    params.text_document_position_params.text_document
                )
                .into(),
                data: None,
            });
        };

        let mut pm = PassManager::default();
        insert_minimal_pipeline(&mut pm);
        insert_fix_passes(&mut pm);
        insert_compile_passes(&mut pm);

        pm.state.insert_default::<Errors>();
        pm.state.insert(InputSource(src.to_string()));

        if let Err(err @ PassError::CompilerError { .. }) = pm.run() {
            return Err(tower_lsp_server::jsonrpc::Error {
                code: tower_lsp_server::jsonrpc::ErrorCode::ServerError(0),
                message: format!("Compilation failed: {err}").into(),
                data: None,
            });
        };

        let analysis_data = (
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
            pm.state.get(),
        );

        let analysis_data = if let (
            Some(type_data),
            Some(type_sets),
            Some(function_data),
            Some(variable_data),
            Some(scope_data),
            Some(comptime_deps),
        ) = &analysis_data
        {
            Some(AnalysisData {
                type_data,
                type_sets,
                function_data,
                variable_data,
                scope_data,
                comptime_deps,
            })
        } else {
            None
        };

        let cpos = params.text_document_position_params.position;
        let cpos_sl = SourceLocation {
            index: 0,
            line: cpos.line as usize + 1,
            column: cpos.character as usize,
        };
        let find_pass = FindContainingNodePass::new(cpos_sl);

        if let Ok((node_hierarchy, _hovered_token)) =
            find_pass.visit_program(&mut pm.state.get_mut().unwrap())
        {
            let mut prev_node = None;
            let mut function_call = None;
            for node in node_hierarchy.iter().rev() {
                if let AstNode::FunctionCallExpression(fcall @ FunctionCallExpression { .. }) = node
                {
                    function_call = Some(fcall.clone());
                    break;
                }
                prev_node = Some(node);
            }

            let Some(function_call) = function_call else {
                return Ok(None);
            };

            let label = self
                .generate_node_hover(function_call.clone(), analysis_data)
                .0;

            let Some(analysis_data) = analysis_data else {
                return Ok(None);
            };

            let Some(ref_func) = analysis_data.function_data.get(&function_call.id) else {
                return Ok(None);
            };
            let ref_func_borrow = ref_func.borrow();
            let Some(parameter_types) = ref_func_borrow.parameter_types.as_ref() else {
                return Ok(None);
            };

            let active_parameter = prev_node
                .map(|argument| {
                    function_call
                        .arguments
                        .iter()
                        .find_position(|(arg, _token)| arg.get_id() == argument.get_id())
                        .map(|(i, _)| i as u32)
                        .unwrap_or(if cpos_sl <= function_call.open_paren_token.start {
                            0
                        } else {
                            function_call
                                .arguments
                                .len()
                                .min(parameter_types.len().saturating_sub(1))
                                as u32
                        })
                })
                .unwrap_or(if cpos_sl <= function_call.open_paren_token.start {
                    0
                } else {
                    function_call
                        .arguments
                        .len()
                        .min(parameter_types.len().saturating_sub(1)) as u32
                });

            Ok(Some(SignatureHelp {
                signatures: vec![SignatureInformation {
                    label: label.clone(),
                    documentation: None,
                    parameters: Some(
                        ref_func
                            .borrow()
                            .parameter_types
                            .iter()
                            .enumerate()
                            .map(|(param_i, _param)| ParameterInformation {
                                // TODO: don't parse label positions out of the string
                                label: ParameterLabel::LabelOffsets([
                                    label
                                        .clone()
                                        .chars()
                                        .enumerate()
                                        .filter(|(_i, c)| matches!(*c, ',' | '(' | ')'))
                                        .nth(param_i)
                                        .map(|(i, c)| i + if matches!(c, ',') { 2 } else { 1 })
                                        .unwrap_or(0) as u32,
                                    label
                                        .clone()
                                        .chars()
                                        .enumerate()
                                        .filter(|(_i, c)| matches!(*c, ',' | '(' | ')'))
                                        .nth(param_i + 1)
                                        .map(|(i, _)| i)
                                        .unwrap_or(0) as u32,
                                ]),
                                documentation: None,
                            })
                            .collect(),
                    ),
                    active_parameter: Some(active_parameter),
                }],
                active_signature: Some(0),
                active_parameter: None,
            }))
        } else {
            Ok(None)
        }
    }
}

impl BackgroundThreadState {
    pub async fn run_background_thread(self_: &Arc<Mutex<Self>>, client: &Client) {
        if self_
            .lock()
            .unwrap()
            .semantic_tokens_refresh
            .load(Ordering::Relaxed)
        {
            self_
                .lock()
                .unwrap()
                .semantic_tokens_refresh
                .store(false, Ordering::Relaxed);
            let _ = client.semantic_tokens_refresh().await;
        }
    }
}
