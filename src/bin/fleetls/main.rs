use std::collections::HashMap;
use std::process::exit;
use std::sync::LazyLock;

use fleet::parser::Parser;
use fleet::pretty_print::PrettyPrint;
use fleet::tokenizer::{TokenType, Tokenizer};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: std::sync::RwLock<HashMap<Url, String>>,
}

static SEMANTIC_TOKEN_TYPES: LazyLock<Vec<SemanticTokenType>> = std::sync::LazyLock::new(|| {
    vec![
        SemanticTokenType::VARIABLE,
        SemanticTokenType::NUMBER,
        SemanticTokenType::KEYWORD,
    ]
});

fn find_token_type_index(token_type: SemanticTokenType) -> u32 {
    SEMANTIC_TOKEN_TYPES
        .iter()
        .position(|t| t.as_str() == token_type.as_str())
        .unwrap_or(0) as u32
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                                token_modifiers: vec![],
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
        Ok(DocumentDiagnosticReportResult::Report(
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: None,
                    items: vec![Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 0,
                            },
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("FleetLS".to_string()),
                        message: "FleetLS is running".to_string(),
                        related_information: None,
                        tags: None,
                        data: None,
                    }],
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
        let text = self
            .documents
            .read()
            .unwrap()
            .get(&params.text_document.uri)
            .unwrap()
            .clone();

        let tokens = Tokenizer::new(text)
            .tokenize()
            .map_err(|_| tower_lsp::jsonrpc::Error {
                code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                message: "Tokenization failed".into(),
                data: None,
            })?;

        let mut prev_token = tokens.first().unwrap();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens
                .iter()
                .map(|token| {
                    let r = Some(SemanticToken {
                        delta_line: (token.start.line - prev_token.start.line) as u32,
                        delta_start: if prev_token.start.line == token.start.line {
                            (token.start.column - prev_token.start.column) as u32
                        } else {
                            token.start.column as u32
                        },
                        length: (token.end.index - token.start.index) as u32,
                        token_type: match token.type_ {
                            TokenType::Keyword(_) => {
                                find_token_type_index(SemanticTokenType::KEYWORD)
                            }
                            TokenType::Identifier(_) => {
                                find_token_type_index(SemanticTokenType::VARIABLE)
                            }
                            TokenType::Number(_) => {
                                find_token_type_index(SemanticTokenType::NUMBER)
                            }
                            TokenType::OpenBrace
                            | TokenType::CloseBrace
                            | TokenType::OpenParen
                            | TokenType::CloseParen
                            | TokenType::OpenBracket
                            | TokenType::CloseBracket
                            | TokenType::Semicolon
                            | TokenType::Dot => return None,
                        },
                        token_modifiers_bitset: 0,
                    });
                    prev_token = token;
                    return r;
                })
                .filter(|x| *x != None)
                .map(|x| x.unwrap())
                .collect(),
        })))
    }

    async fn shutdown(&self) -> Result<()> {
        exit(0);
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

        let tokens =
            Tokenizer::new(text.clone())
                .tokenize()
                .map_err(|_| tower_lsp::jsonrpc::Error {
                    code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                    message: "Tokenization failed".into(),
                    data: None,
                })?;

        let program =
            Parser::new(tokens)
                .parse_program()
                .map_err(|_| tower_lsp::jsonrpc::Error {
                    code: tower_lsp::jsonrpc::ErrorCode::ParseError,
                    message: "Parsing failed".into(),
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
            new_text: program.pretty_print(),
        }]));
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Default::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
