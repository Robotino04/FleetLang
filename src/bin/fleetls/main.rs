use std::collections::HashMap;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::process::exit;
use std::sync::LazyLock;

use fleet::ast::{
    AstNode, Executor, ExecutorHost, Expression, FunctionDefinition, Statement, Type,
};
use fleet::infra::compile;
use fleet::passes::ast_pass::AstPass;
use fleet::pretty_print::pretty_print;
use fleet::tokenizer::{SourceLocation, Token, TokenType};
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

fn token_delta_line(a: &Token, b: &Token) -> u32 {
    (b.start.line - a.start.line) as u32
}
fn token_delta_start(a: &Token, b: &Token) -> u32 {
    if b.start.line == a.start.line {
        (b.start.column - a.start.column) as u32
    } else {
        b.start.column as u32
    }
}
fn token_length(a: &Token) -> u32 {
    (a.end.index - a.start.index) as u32
}

struct ExtractSemanticTokensPass {
    previous_token: Token,
}

impl ExtractSemanticTokensPass {
    fn build_semantic_token(
        &mut self,
        token: &Token,
        token_type: SemanticTokenType,
        token_modifiers: Vec<SemanticTokenModifier>,
    ) -> SemanticToken {
        let res = SemanticToken {
            delta_line: token_delta_line(&self.previous_token, token),
            delta_start: token_delta_start(&self.previous_token, token),
            length: token_length(token),
            token_type: self.find_token_type_index(token_type),
            token_modifiers_bitset: self.build_token_modifier_bitset(token_modifiers),
        };
        self.previous_token = token.clone();
        return res;
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

impl AstPass for ExtractSemanticTokensPass {
    type Output = Vec<SemanticToken>;

    fn run(&mut self, node: AstNode) -> Self::Output {
        match node {
            AstNode::Program(program) => program
                .functions
                .iter()
                .map(|f| self.run(f.clone().into()))
                .flatten()
                .collect(),
            AstNode::FunctionDefinition(FunctionDefinition {
                name: _,
                let_token,
                name_token,
                return_type,
                body,
            }) => {
                let mut tokens = vec![
                    self.build_semantic_token(&let_token, SemanticTokenType::KEYWORD, vec![]),
                    self.build_semantic_token(
                        &name_token,
                        SemanticTokenType::FUNCTION,
                        vec![
                            SemanticTokenModifier::DEFINITION,
                            SemanticTokenModifier::DECLARATION,
                        ],
                    ),
                ];
                tokens.extend(self.run(return_type.into()));
                tokens.extend(self.run(body.into()));
                return tokens;
            }
            AstNode::Type(Type::I32 { token }) => {
                vec![self.build_semantic_token(&token, SemanticTokenType::TYPE, vec![])]
            }
            AstNode::Statement(Statement::Expression(expression)) => {
                self.run(AstNode::Expression(expression))
            }

            AstNode::Statement(Statement::On {
                on_token,
                executor,
                body,
            }) => {
                let mut tokens =
                    vec![self.build_semantic_token(&on_token, SemanticTokenType::KEYWORD, vec![])];
                tokens.extend(self.run(executor.into()));
                tokens.extend(self.run((*body).into()));
                return tokens;
            }
            AstNode::Statement(Statement::Block(body)) => body
                .iter()
                .map(|stmt| self.run(stmt.clone().into()))
                .flatten()
                .collect(),
            AstNode::Statement(Statement::Return {
                return_token,
                value,
            }) => {
                let mut tokens = vec![self.build_semantic_token(
                    &return_token,
                    SemanticTokenType::KEYWORD,
                    vec![],
                )];
                tokens.extend(self.run(value.into()));
                return tokens;
            }
            AstNode::ExecutorHost(ExecutorHost::Self_ { token }) => {
                vec![self.build_semantic_token(&token, SemanticTokenType::KEYWORD, vec![])]
            }
            AstNode::Executor(Executor::Thread {
                thread_token,
                index,
                host,
            }) => {
                let mut tokens = self.run(host.into());
                tokens.push(self.build_semantic_token(
                    &thread_token,
                    SemanticTokenType::VARIABLE,
                    vec![],
                ));
                tokens.extend(self.run(index.into()));
                return tokens;
            }
            AstNode::Expression(Expression::Number { value: _, token }) => {
                vec![self.build_semantic_token(&token, SemanticTokenType::NUMBER, vec![])]
            }
            AstNode::Expression(Expression::FunctionCall {
                name: _,
                name_token,
                arguments,
            }) => {
                let mut tokens = vec![self.build_semantic_token(
                    &name_token,
                    SemanticTokenType::FUNCTION,
                    vec![],
                )];
                tokens.extend(
                    arguments
                        .iter()
                        .map(|arg| self.run(arg.clone().into()))
                        .flatten(),
                );
                return tokens;
            }
            AstNode::Expression(Expression::Unary {
                operator_token,
                operation: _,
                operand,
            }) => {
                let mut tokens = vec![self.build_semantic_token(
                    &operator_token,
                    SemanticTokenType::FUNCTION,
                    vec![],
                )];
                tokens.extend(self.run((*operand).into()));
                return tokens;
            }
            AstNode::Expression(Expression::Binary {
                left,
                operator_token,
                operation: _,
                right,
            }) => {
                let mut tokens = self.run((*left).into());
                tokens.push(self.build_semantic_token(
                    &operator_token,
                    SemanticTokenType::OPERATOR,
                    vec![],
                ));
                tokens.extend(self.run((*right).into()));
                return tokens;
            }
            AstNode::Expression(Expression::Grouping { subexpression }) => {
                self.run((*subexpression).into())
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
        println!("---------------------------------------");
        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let context = Context::create();
        let res = compile(&context, text.as_str());

        let program = res
            .status
            .program()
            .ok_or_else(|| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?;

        let lsp_tokens = ExtractSemanticTokensPass {
            // a fake token to guarantee correct character counts if the first real token
            // isn't at index 0
            previous_token: Token {
                type_: TokenType::Semicolon,
                start: SourceLocation {
                    index: 0,
                    line: 1,
                    column: 0,
                },
                end: SourceLocation {
                    index: 0,
                    line: 1,
                    column: 0,
                },
            },
        }
        .run(program.clone().into());

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: lsp_tokens,
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
