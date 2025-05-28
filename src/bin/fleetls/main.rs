use std::collections::HashMap;
use std::env::args;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::sync::LazyLock;

use fleet::ast::{
    AstNode, AstVisitor, BinaryOperation, BlockStatement, Executor, ExecutorHost, Expression,
    ExpressionStatement, FunctionDefinition, IfStatement, OnStatement, ReturnStatement, Type,
    UnaryOperation, VariableDefinitionStatement,
};
use fleet::infra::{CompileStatus, ErrorSeverity, compile_program, format_program};
use fleet::passes::find_containing_node::FindContainingNodePass;
use fleet::tokenizer::{SourceLocation, Token, Trivia, TriviaKind};
use indoc::indoc;
use inkwell::context::Context;
use tower_lsp::jsonrpc::Result;
use tower_lsp::{Client, LspService, Server};
use tower_lsp::{LanguageServer, lsp_types::*};

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: std::sync::RwLock<HashMap<Url, String>>,
}

static SEMANTIC_TOKEN_TYPES: LazyLock<Vec<SemanticTokenType>> = std::sync::LazyLock::new(|| {
    vec![
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::TYPE,
        SemanticTokenType::CLASS,
        SemanticTokenType::ENUM,
        SemanticTokenType::INTERFACE,
        SemanticTokenType::STRUCT,
        SemanticTokenType::TYPE_PARAMETER,
        SemanticTokenType::PARAMETER,
        SemanticTokenType::VARIABLE,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::EVENT,
        SemanticTokenType::FUNCTION,
        SemanticTokenType::METHOD,
        SemanticTokenType::MACRO,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::MODIFIER,
        SemanticTokenType::COMMENT,
        SemanticTokenType::STRING,
        SemanticTokenType::NUMBER,
        SemanticTokenType::REGEXP,
        SemanticTokenType::OPERATOR,
        SemanticTokenType::DECORATOR,
    ]
});
static SEMANTIC_TOKEN_MODIFIERS: LazyLock<Vec<SemanticTokenModifier>> =
    std::sync::LazyLock::new(|| {
        vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::STATIC,
            SemanticTokenModifier::DEPRECATED,
            SemanticTokenModifier::ABSTRACT,
            SemanticTokenModifier::ASYNC,
            SemanticTokenModifier::MODIFICATION,
            SemanticTokenModifier::DOCUMENTATION,
            SemanticTokenModifier::DEFAULT_LIBRARY,
        ]
    });

fn token_delta_line(prev_start: SourceLocation, start: SourceLocation) -> u32 {
    (start.line - prev_start.line) as u32
}
fn token_delta_start(prev_start: SourceLocation, start: SourceLocation) -> u32 {
    if start.line == prev_start.line {
        (start.column - prev_start.column) as u32
    } else {
        start.column as u32
    }
}
fn token_length(start: SourceLocation, end: SourceLocation) -> u32 {
    (end.index - start.index) as u32
}

impl Backend {
    fn generate_node_hover(&self, node: impl Into<AstNode>) -> (String, String) {
        match node.into() {
            AstNode::Program(_) => ("".to_string(), "program".to_string()),
            AstNode::FunctionDefinition(FunctionDefinition {
                let_token: _,
                name,
                name_token: _,
                equal_token: _,
                open_paren_token: _,
                close_paren_token: _,
                right_arrow_token: _,
                return_type,
                body: _,
                id: _,
            }) => (
                format!("{name} = () -> {}", self.generate_node_hover(return_type).0),
                "function definition".to_string(),
            ),
            AstNode::ExpressionStatement(ExpressionStatement {
                expression,
                semicolon_token: _,
                id: _,
            }) => (
                self.generate_node_hover(expression).0,
                "expression statement".to_string(),
            ),
            AstNode::OnStatement(OnStatement {
                on_token: _,
                open_paren_token: _,
                executor,
                close_paren_token: _,
                body: _,
                id: _,
            }) => (
                format!("on ({})", self.generate_node_hover(executor).0),
                "`on` statement".to_string(),
            ),
            AstNode::BlockStatement(BlockStatement { .. }) => ("".to_string(), "block".to_string()),
            // TODO: once we have type inference, show the value type here
            AstNode::ReturnStatement(ReturnStatement { .. }) => {
                ("return".to_string(), "`return` statement".to_string())
            }
            AstNode::VariableDefinitionStatement(VariableDefinitionStatement {
                let_token: _,
                name_token: _,
                name,
                colon_token: _,
                type_,
                equals_token: _,
                value: _,
                semicolon_token: _,
                id: _,
            }) => (
                format!(
                    "let {name}: {} = ...", // TODO: once we have consteval, display that here
                    self.generate_node_hover(type_).0
                ),
                "variable definition".to_string(),
            ),
            AstNode::IfStatement(IfStatement { .. }) => {
                ("".to_string(), "`if` statement".to_string())
            }

            AstNode::ExecutorHost(ExecutorHost::Self_ { .. }) => {
                ("self".to_string(), "`self` executor host".to_string())
            }
            AstNode::Executor(Executor::Thread {
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
                    self.generate_node_hover(host).0,
                    self.generate_node_hover(index).0
                ),
                "thread executor".to_string(),
            ),
            AstNode::Expression(Expression::Unary {
                operator_token: _,
                operation,
                operand: _,
                id: _,
            }) => {
                // TODO: once we have type inference, display operand type here
                (
                    match operation {
                        UnaryOperation::BitwiseNot => "bitwise negation (~)",
                        UnaryOperation::LogicalNot => "logical negation (!)",
                        UnaryOperation::Negate => "arithmetic negation (-)",
                    }
                    .to_string(),
                    "unary expression".to_string(),
                )
            }
            // TODO: once we have type inference, display type here
            AstNode::Expression(Expression::Number {
                value,
                token: _,
                id: _,
            }) => (value.to_string(), "number literal".to_string()),
            AstNode::Expression(Expression::Binary {
                left: _,
                operator_token: _,
                operation,
                right: _,
                id: _,
            }) => {
                // TODO: once we have type inference, display operand types here
                (
                    match operation {
                        BinaryOperation::Add => "addition (+)",
                        BinaryOperation::Subtract => "subtraction (-)",
                        BinaryOperation::Multiply => "multiplication (*)",
                        BinaryOperation::Divide => "division (/)",
                        BinaryOperation::Modulo => "module (%)",
                        BinaryOperation::GreaterThan => "greater than (>)",
                        BinaryOperation::GreaterThanOrEqual => "greater than or equal (>=)",
                        BinaryOperation::LessThan => "less than (<)",
                        BinaryOperation::LessThanOrEqual => "less than or equal (<=)",
                        BinaryOperation::Equal => "equal (==)",
                        BinaryOperation::NotEqual => "not equal (!=)",
                        BinaryOperation::LogicalAnd => "logical and (&&)",
                        BinaryOperation::LogicalOr => "logical or (||)",
                    }
                    .to_string(),
                    "binary expression".to_string(),
                )
            }
            AstNode::Expression(Expression::Grouping {
                open_paren_token: _,
                subexpression,
                close_paren_token: _,
                id: _,
            }) => (
                format!("({})", self.generate_node_hover(*subexpression).0),
                "expression grouping".to_string(),
            ),

            AstNode::Expression(Expression::FunctionCall {
                name,
                name_token: _,
                open_paren_token: _,
                arguments: _,
                close_paren_token: _,
                id: _,
            }) => {
                // TODO: once we have proper semantic analysis, display the function types here
                (
                    format!("let {name} = () -> ..."),
                    "function call".to_string(),
                )
            }
            AstNode::Expression(Expression::VariableAccess {
                name,
                name_token: _,
                id: _,
            }) => {
                // TODO: once we have proper semantic analysis, display the variable types here
                // TODO: once we have consteval, display the value here
                (
                    format!("let {name}: ... = ..."),
                    "variable access".to_string(),
                )
            }
            AstNode::Expression(Expression::VariableAssignment {
                name,
                name_token: _,
                equal_token: _,
                right: _,
                id: _,
            }) => {
                // TODO: once we have proper semantic analysis, display the variable types here
                (
                    format!("let {name}: ..."),
                    "variable assignment".to_string(),
                )
            }
            AstNode::Type(Type::I32 { token: _, id: _ }) => (format!("i32"), "type".to_string()),
        }
    }
}

struct ExtractSemanticTokensPass {
    previous_token_start: SourceLocation,
    semantic_tokens: Vec<SemanticToken>,
}

impl ExtractSemanticTokensPass {
    pub fn new() -> Self {
        Self {
            previous_token_start: SourceLocation::start(),
            semantic_tokens: vec![],
        }
    }
}

impl ExtractSemanticTokensPass {
    fn build_semantic_token(
        &mut self,
        token: &Token,
        token_type: SemanticTokenType,
        token_modifiers: Vec<SemanticTokenModifier>,
    ) {
        self.build_comment_tokens_from_trivia(&token.leading_trivia);
        self.semantic_tokens.push(SemanticToken {
            delta_line: token_delta_line(self.previous_token_start, token.start),
            delta_start: token_delta_start(self.previous_token_start, token.start),
            length: token_length(token.start, token.end),
            token_type: self.find_token_type_index(token_type),
            token_modifiers_bitset: self.build_token_modifier_bitset(token_modifiers),
        });
        self.previous_token_start = token.start.clone();
        self.build_comment_tokens_from_trivia(&token.trailing_trivia);
    }

    fn build_comment_tokens_from_trivia(&mut self, trivia: &Vec<Trivia>) {
        for Trivia { kind, start, end } in trivia {
            if let TriviaKind::LineComment(content) | TriviaKind::BlockComment(content) = kind {
                let mut mod_start = start.clone();
                for line in content.split("\n") {
                    let mut length = line.chars().count();
                    if mod_start.line == end.line {
                        length += 2 // the comment end
                    }
                    if mod_start.line == start.line {
                        length += 2 // the comment start
                    }
                    self.semantic_tokens.push(SemanticToken {
                        delta_line: token_delta_line(self.previous_token_start, mod_start),
                        delta_start: token_delta_start(self.previous_token_start, mod_start),
                        length: length as u32,
                        token_type: self.find_token_type_index(SemanticTokenType::COMMENT),
                        token_modifiers_bitset: 0,
                    });
                    self.previous_token_start = mod_start;
                    mod_start.line += 1;
                    mod_start.column = 0;
                    mod_start.index += length + 1; // the newline
                }
            }
        }
    }

    fn build_comment_tokens_only(&mut self, token: &Token) {
        self.build_comment_tokens_from_trivia(&token.leading_trivia);
        self.build_comment_tokens_from_trivia(&token.trailing_trivia);
    }

    fn find_token_type_index(&self, token_type: SemanticTokenType) -> u32 {
        SEMANTIC_TOKEN_TYPES
            .iter()
            .position(|t| t.as_str() == token_type.as_str())
            .unwrap_or(0) as u32
    }
    fn build_token_modifier_bitset(&self, token_modifiers: Vec<SemanticTokenModifier>) -> u32 {
        token_modifiers
            .iter()
            .map(|modifier| {
                1u32 << (SEMANTIC_TOKEN_MODIFIERS
                    .iter()
                    .position(|t| t.as_str() == modifier.as_str())
                    .unwrap_or(0))
            })
            .reduce(|a, b| a | b)
            .unwrap_or(0)
    }
}

impl AstVisitor for ExtractSemanticTokensPass {
    type SubOutput = ();
    type Output = Vec<SemanticToken>;

    fn visit_program(mut self, program: &mut fleet::ast::Program) -> Self::Output {
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }
        return self.semantic_tokens;
    }

    fn visit_function_definition(
        &mut self,
        FunctionDefinition {
            let_token,
            name: _,
            name_token,
            equal_token,
            open_paren_token,
            close_paren_token,
            right_arrow_token,
            return_type,
            body,
            id: _,
        }: &mut FunctionDefinition,
    ) {
        self.build_semantic_token(let_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_semantic_token(
            name_token,
            SemanticTokenType::FUNCTION,
            vec![
                SemanticTokenModifier::DEFINITION,
                SemanticTokenModifier::DECLARATION,
            ],
        );
        self.build_comment_tokens_only(equal_token);
        self.build_comment_tokens_only(open_paren_token);
        self.build_comment_tokens_only(close_paren_token);
        self.build_comment_tokens_only(right_arrow_token);
        self.visit_type(return_type);
        self.visit_statement(body);
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &mut ExpressionStatement,
    ) -> Self::SubOutput {
        self.visit_expression(&mut expr_stmt.expression);
        self.build_comment_tokens_only(&mut expr_stmt.semicolon_token);
    }

    fn visit_on_statement(&mut self, on_stmt: &mut OnStatement) -> Self::SubOutput {
        self.build_semantic_token(&mut on_stmt.on_token, SemanticTokenType::KEYWORD, vec![]);
        self.build_comment_tokens_only(&mut on_stmt.open_paren_token);
        self.visit_executor(&mut on_stmt.executor);
        self.build_comment_tokens_only(&mut on_stmt.close_paren_token);
        self.visit_statement(&mut on_stmt.body);
    }

    fn visit_block_statement(&mut self, block: &mut BlockStatement) -> Self::SubOutput {
        self.build_comment_tokens_only(&mut block.open_brace_token);
        for stmt in &mut block.body {
            self.visit_statement(stmt);
        }
        self.build_comment_tokens_only(&mut block.close_brace_token);
    }

    fn visit_return_statement(&mut self, return_stmt: &mut ReturnStatement) -> Self::SubOutput {
        self.build_semantic_token(
            &mut return_stmt.return_token,
            SemanticTokenType::KEYWORD,
            vec![],
        );
        self.visit_expression(&mut return_stmt.value);
        self.build_comment_tokens_only(&mut return_stmt.semicolon_token);
    }

    fn visit_variable_definition_statement(
        &mut self,
        vardef_stmt: &mut VariableDefinitionStatement,
    ) -> Self::SubOutput {
        self.build_semantic_token(
            &mut vardef_stmt.let_token,
            SemanticTokenType::KEYWORD,
            vec![],
        );
        self.build_semantic_token(
            &mut vardef_stmt.name_token,
            SemanticTokenType::VARIABLE,
            vec![SemanticTokenModifier::DEFINITION],
        );
        self.build_comment_tokens_only(&mut vardef_stmt.colon_token);
        self.visit_type(&mut vardef_stmt.type_);
        self.build_comment_tokens_only(&mut vardef_stmt.equals_token);
        self.visit_expression(&mut vardef_stmt.value);
        self.build_comment_tokens_only(&mut vardef_stmt.semicolon_token);
    }

    fn visit_if_statement(&mut self, if_stmt: &mut IfStatement) -> Self::SubOutput {
        self.build_semantic_token(&mut if_stmt.if_token, SemanticTokenType::KEYWORD, vec![]);
        self.visit_expression(&mut if_stmt.condition);
        self.visit_statement(&mut if_stmt.if_body);
        for (token, condition, body) in &mut if_stmt.elifs {
            self.build_semantic_token(token, SemanticTokenType::KEYWORD, vec![]);
            self.visit_expression(condition);
            self.visit_statement(body);
        }
        if let Some((token, body)) = &mut if_stmt.else_ {
            self.build_semantic_token(token, SemanticTokenType::KEYWORD, vec![]);
            self.visit_statement(body);
        }
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_ { token, id: _ } => {
                self.build_comment_tokens_only(token);
            }
        }
    }

    fn visit_executor(&mut self, executor: &mut Executor) {
        match executor {
            Executor::Thread {
                host,
                dot_token,
                thread_token,
                open_bracket_token,
                index,
                close_bracket_token,
                id: _,
            } => {
                self.visit_executor_host(host);
                self.build_comment_tokens_only(dot_token);
                self.build_semantic_token(thread_token, SemanticTokenType::VARIABLE, vec![]);
                self.build_comment_tokens_only(open_bracket_token);
                self.visit_expression(index);
                self.build_comment_tokens_only(close_bracket_token);
            }
        }
    }

    fn visit_expression(&mut self, expression: &mut Expression) {
        match expression {
            Expression::Number {
                value: _,
                token,
                id: _,
            } => {
                self.build_semantic_token(token, SemanticTokenType::NUMBER, vec![]);
            }
            Expression::FunctionCall {
                name: _,
                name_token,
                open_paren_token,
                arguments,
                close_paren_token,
                id: _,
            } => {
                self.build_semantic_token(name_token, SemanticTokenType::FUNCTION, vec![]);
                self.build_comment_tokens_only(open_paren_token);
                for arg in arguments {
                    self.visit_expression(arg);
                }
                self.build_comment_tokens_only(close_paren_token);
            }
            Expression::Grouping {
                open_paren_token,
                subexpression,
                close_paren_token,
                id: _,
            } => {
                self.build_comment_tokens_only(open_paren_token);
                self.visit_expression(subexpression);
                self.build_comment_tokens_only(close_paren_token);
            }
            Expression::VariableAccess {
                name: _,
                name_token,
                id: _,
            } => {
                self.build_semantic_token(name_token, SemanticTokenType::VARIABLE, vec![]);
            }
            Expression::Unary {
                operator_token,
                operation: _,
                operand,
                id: _,
            } => {
                self.build_comment_tokens_only(operator_token);
                self.visit_expression(operand);
            }
            Expression::Binary {
                left,
                operator_token,
                operation: _,
                right,
                id: _,
            } => {
                self.visit_expression(left);
                self.build_comment_tokens_only(operator_token);
                self.visit_expression(right);
            }
            Expression::VariableAssignment {
                name: _,
                name_token,
                equal_token,
                right,
                id: _,
            } => {
                self.build_semantic_token(
                    name_token,
                    SemanticTokenType::VARIABLE,
                    vec![SemanticTokenModifier::MODIFICATION],
                );
                self.build_comment_tokens_only(equal_token);
                self.visit_expression(right);
            }
        }
    }

    fn visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { token, id: _ } => {
                self.build_semantic_token(token, SemanticTokenType::TYPE, vec![]);
            }
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
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
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "FleetLS".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").into()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> Result<DocumentDiagnosticReportResult> {
        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        let _program = res
            .status
            .program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?;

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: res
                        .errors
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
        self.documents.write().unwrap().insert(
            params.text_document.uri,
            params.content_changes[0].text.clone(),
        );
    }
    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.documents
            .write()
            .unwrap()
            .insert(params.text_document.uri, params.text_document.text);
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents
            .write()
            .unwrap()
            .remove(&params.text_document.uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document_position_params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        let mut program = res
            .status
            .program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?
            .clone();

        let cpos = params.text_document_position_params.position;
        let find_pass = FindContainingNodePass::new(SourceLocation {
            index: 0,
            line: cpos.line as usize + 1,
            column: cpos.character as usize,
        });

        let terminations = res.status.function_terminations().cloned();

        if let Ok((node_hierarchy, hovered_token)) = find_pass.visit_program(&mut program) {
            Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        indoc! {r##"
                        {}

                        ---- Debug Stats ----
                        {}

                        ---- Token ----
                        ```rust
                        {:#?}
                        ```
                    "##},
                        node_hierarchy
                            .last()
                            .map(|node| self.generate_node_hover(node.clone()))
                            .map_or("No AST Node".to_string(), |(info, debug)| format!(
                                "{info} // {debug}"
                            )),
                        terminations
                            .map(|ts| ts.get(node_hierarchy.last()?).cloned())
                            .flatten()
                            .map_or("No termination info available".to_string(), |t| format!(
                                "{t:?}"
                            )),
                        hovered_token,
                    ),
                }),
                range: Some(Range {
                    start: Position {
                        line: hovered_token.start.line as u32 - 1,
                        character: hovered_token.start.column as u32,
                    },
                    end: Position {
                        line: hovered_token.end.line as u32 - 1,
                        character: hovered_token.end.column as u32,
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
        eprintln!("---------------------------------------");
        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        let mut program = res
            .status
            .program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?
            .clone();

        let semantic_tokens = ExtractSemanticTokensPass::new().visit_program(&mut program);

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: semantic_tokens,
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile_program(&context, text.as_str());

        if match res.status {
            CompileStatus::TokenizerFailure { .. } => true,
            CompileStatus::ParserFailure { .. } => true,
            CompileStatus::TokenizerOrParserErrors { .. } => true,
            CompileStatus::IrGeneratorFailure { .. } => false,
            CompileStatus::IrGeneratorErrors { .. } => false,
            CompileStatus::Success { .. } => false,
        } {
            return Err(tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Code has parse errors. Not formatting.".into(),
                data: None,
            });
        }

        let program = res
            .status
            .program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?;

        return Ok(Some(vec![TextEdit {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: text.chars().filter(|c| *c == '\n').count() as u32,
                    character: text.split('\n').last().unwrap().chars().count() as u32,
                },
            },
            new_text: format_program(program.clone()),
        }]));
    }
}

#[tokio::main]
async fn main() {
    if args().any(|arg| arg == "--stdio") {
        let (service, loopback_socket) = LspService::new(|client| Backend {
            client,
            documents: Default::default(),
        });

        Server::new(tokio::io::stdin(), tokio::io::stdout(), loopback_socket)
            .serve(service)
            .await;
    } else {
        let socket = tokio::net::TcpSocket::new_v4().unwrap();
        socket.set_reuseaddr(true).unwrap();
        socket
            .bind(SocketAddr::new(IpAddr::V4(Ipv4Addr::LOCALHOST), 1234))
            .unwrap();
        let listener = socket.listen(5).unwrap();

        loop {
            let (client_connection, _client_addr) = listener.accept().await.unwrap();

            tokio::spawn(async move {
                let (service, loopback_socket) = LspService::new(|client| Backend {
                    client,
                    documents: Default::default(),
                });

                let (read_half, write_half) = tokio::io::split(client_connection);
                Server::new(read_half, write_half, loopback_socket)
                    .serve(service)
                    .await;
            });
        }
    }
}
