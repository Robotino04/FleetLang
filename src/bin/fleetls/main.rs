use std::collections::HashMap;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};
use std::process::exit;
use std::sync::LazyLock;

use fleet::ast::{
    AstNode, Executor, ExecutorHost, Expression, FunctionDefinition, Statement, Type,
};
use fleet::parser::{ParseError, Parser};
use fleet::pretty_print::pretty_print;
use fleet::tokenizer::{SourceLocation, Token, TokenType, Tokenizer};
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
        SemanticTokenType::new("brace"),
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

impl Backend {
    fn get_lsp_tokens(
        &self,
        previous_token: &mut Token,
        tokens: &mut Vec<SemanticToken>,
        node: AstNode,
    ) {
        match node {
            AstNode::Program(program) => {
                for f in program.functions {
                    self.get_lsp_tokens(previous_token, tokens, AstNode::FunctionDefinition(f));
                }
            }
            AstNode::FunctionDefinition(FunctionDefinition {
                name: _,
                let_token,
                name_token,
                return_type,
                body,
            }) => {
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &let_token,
                    SemanticTokenType::KEYWORD,
                    vec![],
                ));
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &name_token,
                    SemanticTokenType::FUNCTION,
                    vec![
                        SemanticTokenModifier::DEFINITION,
                        SemanticTokenModifier::DECLARATION,
                    ],
                ));
                self.get_lsp_tokens(previous_token, tokens, AstNode::Type(return_type));
                self.get_lsp_tokens(previous_token, tokens, AstNode::Statement(body));
            }
            AstNode::Type(Type::I32 { token }) => {
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &token,
                    SemanticTokenType::TYPE,
                    vec![],
                ));
            }
            AstNode::Statement(Statement::Expression(expression)) => {
                self.get_lsp_tokens(previous_token, tokens, AstNode::Expression(expression))
            }
            AstNode::Statement(Statement::On {
                on_token,
                executor,
                body,
            }) => {
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &on_token,
                    SemanticTokenType::KEYWORD,
                    vec![],
                ));
                self.get_lsp_tokens(previous_token, tokens, AstNode::Executor(executor));
                self.get_lsp_tokens(previous_token, tokens, AstNode::Statement(*body));
            }
            AstNode::Statement(Statement::Block(body)) => {
                for stmt in body {
                    self.get_lsp_tokens(previous_token, tokens, AstNode::Statement(stmt));
                }
            }
            AstNode::Statement(Statement::Return {
                return_token,
                value,
            }) => {
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &return_token,
                    SemanticTokenType::KEYWORD,
                    vec![],
                ));
                self.get_lsp_tokens(previous_token, tokens, AstNode::Expression(value));
            }
            AstNode::ExecutorHost(ExecutorHost::Self_ { token }) => {
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &token,
                    SemanticTokenType::KEYWORD,
                    vec![],
                ));
            }
            AstNode::Executor(Executor::Thread {
                thread_token,
                index,
                host,
            }) => {
                self.get_lsp_tokens(previous_token, tokens, AstNode::ExecutorHost(host));
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &thread_token,
                    SemanticTokenType::VARIABLE,
                    vec![],
                ));
                self.get_lsp_tokens(previous_token, tokens, AstNode::Expression(index));
            }
            AstNode::Expression(Expression::Number { value: _, token }) => {
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &token,
                    SemanticTokenType::NUMBER,
                    vec![],
                ));
            }
            AstNode::Expression(Expression::FunctionCall {
                name: _,
                name_token,
                arguments,
            }) => {
                tokens.push(self.build_semantic_token(
                    previous_token,
                    &name_token,
                    SemanticTokenType::FUNCTION,
                    vec![],
                ));
                for arg in arguments {
                    self.get_lsp_tokens(previous_token, tokens, AstNode::Expression(arg));
                }
            }
        }
    }

    fn build_semantic_token(
        &self,
        previous_token: &mut Token,
        token: &Token,
        token_type: SemanticTokenType,
        token_modifiers: Vec<SemanticTokenModifier>,
    ) -> SemanticToken {
        let res = SemanticToken {
            delta_line: token_delta_line(previous_token, token),
            delta_start: token_delta_start(previous_token, token),
            length: token_length(token),
            token_type: self.find_token_type_index(token_type),
            token_modifiers_bitset: self.build_token_modifier_bitset(token_modifiers),
        };
        *previous_token = token.clone();
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

        let mut tokenizer = Tokenizer::new(text);
        let tokens = tokenizer
            .tokenize()
            .map_err(|_| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Tokenization failed".into(),
                data: None,
            })?;

        let mut parser = Parser::new(tokens.clone());
        let _program = parser
            .parse_program()
            .map_err(|_| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?;

        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: parser
                        .errors()
                        .iter()
                        .cloned()
                        .chain(tokens.iter().filter_map(|tok| match tok.type_.clone() {
                            TokenType::UnknownCharacters(_) => Some(ParseError {
                                start: tok.start,
                                end: tok.end,
                                message: "Unrecognozed characters".to_string(),
                            }),
                            _ => None,
                        }))
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

        let mut tokenizer = Tokenizer::new(text);
        let tokens = tokenizer
            .tokenize()
            .map_err(|_| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Tokenization failed".into(),
                data: None,
            })?;

        let program =
            Parser::new(tokens.clone())
                .parse_program()
                .map_err(|_| tower_lsp::jsonrpc::Error {
                    code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                    message: "Parsing failed completely".into(),
                    data: None,
                })?;

        let mut prev_token = Token {
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
        };

        let mut lsp_tokens = vec![];

        self.get_lsp_tokens(&mut prev_token, &mut lsp_tokens, AstNode::Program(program));

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

        let mut tokenizer = Tokenizer::new(text.clone());
        let tokens = tokenizer
            .tokenize()
            .map_err(|_| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Tokenization failed".into(),
                data: None,
            })?;

        if tokens
            .iter()
            .any(|tok| matches!(tok.type_, TokenType::UnknownCharacters(_)))
        {
            return Err(tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Code has token errors. Not formatting.".into(),
                data: None,
            });
        }

        let mut parser = Parser::new(tokens.clone());
        let program = parser
            .parse_program()
            .map_err(|_| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Parsing failed completely".into(),
                data: None,
            })?;

        if !parser.errors().is_empty() {
            return Err(tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Code has parse errors. Not formatting.".into(),
                data: None,
            });
        }

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
            new_text: pretty_print(AstNode::Program(program)),
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
