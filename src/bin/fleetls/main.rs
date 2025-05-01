use std::collections::HashMap;
use std::env::args;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::process::exit;
use std::sync::LazyLock;

use fleet::ast::{
    AstNode, AstVisitor, Executor, ExecutorHost, Expression, FunctionDefinition, Statement, Type,
};
use fleet::infra::compile;
use fleet::pretty_print::pretty_print;
use fleet::tokenizer::{SourceLocation, Token, Trivia, TriviaKind};
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
            if !matches!(
                kind,
                TriviaKind::LineComment(_) | TriviaKind::BlockComment(_)
            ) {
                continue;
            }

            self.semantic_tokens.push(SemanticToken {
                delta_line: token_delta_line(self.previous_token_start, *start),
                delta_start: token_delta_start(self.previous_token_start, *start),
                length: token_length(*start, *end),
                token_type: self.find_token_type_index(SemanticTokenType::COMMENT),
                token_modifiers_bitset: 0,
            });
            self.previous_token_start = *start;
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
    fn visit_program(&mut self, program: &mut fleet::ast::Program) {
        for f in &mut program.functions {
            self.visit_function_definition(f);
        }
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

    fn visit_statement(&mut self, statement: &mut Statement) {
        match statement {
            Statement::Expression {
                expression,
                semicolon_token,
            } => {
                self.visit_expression(expression);
                self.build_comment_tokens_only(semicolon_token);
            }
            Statement::On {
                on_token,
                open_paren_token,
                executor,
                close_paren_token,
                body,
            } => {
                self.build_semantic_token(on_token, SemanticTokenType::KEYWORD, vec![]);
                self.build_comment_tokens_only(open_paren_token);
                self.visit_executor(executor);
                self.build_comment_tokens_only(close_paren_token);
                self.visit_statement(body);
            }
            Statement::Block {
                open_brace_token,
                body,
                close_brace_token,
            } => {
                self.build_comment_tokens_only(open_brace_token);
                for stmt in body {
                    self.visit_statement(stmt);
                }
                self.build_comment_tokens_only(close_brace_token);
            }
            Statement::Return {
                return_token,
                value,
                semicolon_token,
            } => {
                self.build_semantic_token(return_token, SemanticTokenType::KEYWORD, vec![]);
                self.visit_expression(value);
                self.build_comment_tokens_only(semicolon_token);
            }
        }
    }

    fn visit_executor_host(&mut self, executor_host: &mut ExecutorHost) {
        match executor_host {
            ExecutorHost::Self_ { token } => {
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
            Expression::Number { value: _, token } => {
                self.build_semantic_token(token, SemanticTokenType::NUMBER, vec![]);
            }
            Expression::FunctionCall {
                name: _,
                name_token,
                open_paren_token,
                arguments,
                close_paren_token,
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
            } => {
                self.build_comment_tokens_only(open_paren_token);
                self.visit_expression(subexpression);
                self.build_comment_tokens_only(close_paren_token);
            }
            Expression::Unary {
                operator_token,
                operation: _,
                operand,
            } => {
                self.build_comment_tokens_only(operator_token);
                self.visit_expression(operand);
            }
            Expression::Binary {
                left,
                operator_token,
                operation: _,
                right,
            } => {
                self.visit_expression(left);
                self.build_comment_tokens_only(operator_token);
                self.visit_expression(right);
            }
        }
    }

    fn visit_type(&mut self, type_: &mut Type) {
        match type_ {
            Type::I32 { token } => {
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
        let res = compile(&context, text.as_str());

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
                            severity: Some(DiagnosticSeverity::ERROR),
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

    async fn hover(&self, _params: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "Test 3".to_string(),
            }),
            range: None,
        }))
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
        let res = compile(&context, text.as_str());

        let mut program = res
            .status
            .program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?
            .clone();

        let mut extract_tokens_pass = ExtractSemanticTokensPass::new();
        extract_tokens_pass.visit_program(&mut program);

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: extract_tokens_pass.semantic_tokens,
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        exit(0);
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
        let res = compile(&context, text.as_str());

        if !res.errors.is_empty() {
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
            new_text: pretty_print(AstNode::Program(program.clone())),
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
            let (mut client_connection, _client_addr) = listener.accept().await.unwrap();
            let (service, loopback_socket) = LspService::new(|client| Backend {
                client,
                documents: Default::default(),
            });

            let (read_half, write_half) = client_connection.split();
            Server::new(read_half, write_half, loopback_socket)
                .serve(service)
                .await;
        }
    }
}
