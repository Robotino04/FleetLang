import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import { IntoServer, FromServer } from "./codec";
import {
  Declaration,
  DeclarationParams,
  Definition,
  DefinitionParams,
  DidChangeTextDocumentParams,
  DidCloseTextDocumentParams,
  DidOpenTextDocumentParams,
  DocumentDiagnosticParams,
  DocumentDiagnosticReport,
  DocumentFormattingParams,
  Hover,
  HoverParams,
  InitializeParams,
  Location,
  MarkupContent,
  Range,
  SemanticTokensParams,
  SemanticTokensPartialResult,
  SignatureHelp,
  SignatureHelpParams,
  TextEdit,
} from "vscode-languageserver-protocol";

export class MonacoLspTransport {
  private readonly intoServer: IntoServer;
  private readonly fromServer: FromServer;
  private readonly editor: monaco.editor.IStandaloneCodeEditor;

  constructor(
    editor: monaco.editor.IStandaloneCodeEditor,
    intoServer: IntoServer,
    fromServer: FromServer,
  ) {
    this.editor = editor;
    this.intoServer = intoServer;
    this.fromServer = fromServer;
    this.setup();
  }

  private async setup() {
    console.debug("[LSP TRANSPORT SETUP]", this.editor);

    // Send initialize request and wait for response
    const initializeResponse = await this.sendRequest("initialize", {
      processId: null,
      rootUri: null,
      capabilities: {},
      workspaceFolders: null,
    } as InitializeParams)!;
    console.debug("[LSP INITIALIZE RESPONSE]", initializeResponse);

    // Now safe to send didOpen
    const model = this.editor.getModel()!;
    const uri = model.uri.toString();
    this.intoServer.send({
      jsonrpc: "2.0",
      method: "textDocument/didOpen",
      params: {
        textDocument: {
          uri,
          languageId: model.getLanguageId(),
          version: 1,
          text: model.getValue(),
        },
      } as DidOpenTextDocumentParams,
    } as any);

    // Listen for model disposal to send didClose
    model.onWillDispose(() => {
      this.intoServer.send({
        jsonrpc: "2.0",
        method: "textDocument/didClose",
        params: {
          textDocument: { uri },
        } as DidCloseTextDocumentParams,
      } as any);
    });

    const transform_range = (range: Range) =>
      new monaco.Range(
        range.start.line + 1,
        range.start.character + 1,
        range.end.line + 1,
        range.end.character + 1,
      );

    // Register Monaco providers for LSP features
    // Semantic Tokens Provider
    monaco.languages.registerDocumentSemanticTokensProvider(
      model.getLanguageId(),
      {
        getLegend: () => ({
          tokenTypes: [
            "class",
            "comment",
            "decorator",
            "enum",
            "enumMember",
            "event",
            "function",
            "interface",
            "keyword",
            "macro",
            "method",
            "modifier",
            "namespace",
            "number",
            "operator",
            "parameter",
            "property",
            "regexp",
            "string",
            "struct",
            "type",
            "typeParameter",
            "variable",
          ],
          tokenModifiers: [
            "abstract",
            "async",
            "declaration",
            "defaultLibrary",
            "definition",
            "deprecated",
            "documentation",
            "modification",
            "readonly",
            "static",
          ],
        }),
        provideDocumentSemanticTokens: async (
          model,
          _lastResultId,
          _token,
        ): Promise<monaco.languages.SemanticTokens> => {
          const uri = model.uri.toString();
          const response = await this.sendRequest(
            "textDocument/semanticTokens/full",
            {
              textDocument: { uri },
            } as SemanticTokensParams,
          );
          const result = response?.result as SemanticTokensPartialResult;
          if (result) {
            return { data: Uint32Array.from(result.data) };
          }
          return { data: new Uint32Array() };
        },
        releaseDocumentSemanticTokens: () => {},
      },
    );

    monaco.languages.registerDeclarationProvider(model.getLanguageId(), {
      provideDeclaration: async (
        model,
        position,
        _token,
      ): Promise<monaco.languages.Definition | null> => {
        const uri = model.uri.toString();
        const response = await this.sendRequest("textDocument/declaration", {
          textDocument: { uri },
          position: {
            line: position.lineNumber - 1,
            character: position.column - 1,
          },
        } as DeclarationParams);
        const result = response?.result as Declaration;

        const transform_location = (def: Location) =>
          ({
            range: transform_range(def.range),
            uri: monaco.Uri.parse(def.uri),
          }) as monaco.languages.Location;

        if (result) {
          if (Array.isArray(result)) {
            return result.map(transform_location);
          } else {
            return transform_location(result);
          }
        } else {
          return null;
        }
      },
    });

    monaco.languages.registerDefinitionProvider(model.getLanguageId(), {
      provideDefinition: async (
        model,
        position,
        _token,
      ): Promise<monaco.languages.Definition | null> => {
        const uri = model.uri.toString();
        const response = await this.sendRequest("textDocument/definition", {
          textDocument: { uri },
          position: {
            line: position.lineNumber - 1,
            character: position.column - 1,
          },
        } as DefinitionParams);
        const result = response?.result as Definition;

        const transform_location = (def: Location) =>
          ({
            range: transform_range(def.range),
            uri: monaco.Uri.parse(def.uri),
          }) as monaco.languages.Location;

        if (result) {
          if (Array.isArray(result)) {
            return result.map(transform_location);
          } else {
            return transform_location(result);
          }
        } else {
          return null;
        }
      },
    });

    // Signature Help Provider
    monaco.languages.registerSignatureHelpProvider(model.getLanguageId(), {
      signatureHelpTriggerCharacters: ["(", ","],
      provideSignatureHelp: async (
        model,
        position,
        _token,
        context,
      ): Promise<monaco.languages.SignatureHelpResult | null> => {
        const uri = model.uri.toString();
        const response = await this.sendRequest("textDocument/signatureHelp", {
          textDocument: { uri },
          position: {
            line: position.lineNumber - 1,
            character: position.column - 1,
          },
          context,
        } as SignatureHelpParams);
        const result = response?.result as SignatureHelp;
        if (result) {
          return {
            value: result as monaco.languages.SignatureHelp,
            dispose: () => {},
          };
        }
        return null;
      },
    });

    // Hover Provider
    monaco.languages.registerHoverProvider(model.getLanguageId(), {
      provideHover: async (
        model,
        position,
        _token,
      ): Promise<monaco.languages.Hover> => {
        const uri = model.uri.toString();
        const response = await this.sendRequest("textDocument/hover", {
          textDocument: { uri },
          position: {
            line: position.lineNumber - 1,
            character: position.column - 1,
          },
        } as HoverParams);
        const result = response?.result as Hover;
        if (!result) return { contents: [] };
        const contents = result.contents;

        if (MarkupContent.is(contents)) {
          return { contents: [contents] };
        } else if (typeof contents === "string") {
          return { contents: [{ value: contents }] };
        } else if (Array.isArray(contents)) {
          return { contents: contents as monaco.IMarkdownString[] };
        } else {
          return { contents: [contents] };
        }
      },
    });

    // Formatting Provider
    monaco.languages.registerDocumentFormattingEditProvider(
      model.getLanguageId(),
      {
        provideDocumentFormattingEdits: async (
          model,
          options,
          _token,
        ): Promise<monaco.languages.TextEdit[]> => {
          const uri = model.uri.toString();
          const response = await this.sendRequest("textDocument/formatting", {
            textDocument: { uri },
            options,
          } as DocumentFormattingParams);
          const result = response?.result as TextEdit[];
          if (Array.isArray(result)) {
            return result.map((edit) => ({
              range: transform_range(edit.range),
              text: edit.newText,
            }));
          }
          return [];
        },
      },
    );

    // Regularly poll the server for diagnostics
    const pollDiagnostics = async () => {
      const uri = model.uri.toString();
      try {
        const response = await this.sendRequest("textDocument/diagnostic", {
          textDocument: { uri },
        } as DocumentDiagnosticParams);
        const result = response?.result as DocumentDiagnosticReport;
        console.debug(result);
        if (result && "items" in result) {
          console.debug("[LSP Diagnostics]", result);
          const markers = result.items.map((diag) => ({
            severity:
              {
                1: monaco.MarkerSeverity.Error,
                2: monaco.MarkerSeverity.Warning,
                3: monaco.MarkerSeverity.Info,
                4: monaco.MarkerSeverity.Hint,
              }[diag.severity!] || monaco.MarkerSeverity.Info,
            message: diag.message,
            startLineNumber: diag.range.start.line + 1,
            startColumn: diag.range.start.character + 1,
            endLineNumber: diag.range.end.line + 1,
            endColumn: diag.range.end.character + 1,
            code: diag.code as string,
            source: diag.source!,
          }));
          monaco.editor.setModelMarkers(model, "lsp", markers);
        }
      } catch (e) {
        console.warn("Diagnostics polling failed", e);
      }
    };

    // Listen for changes and send didChange notifications
    this.editor.onDidChangeModelContent((_e) => {
      const model = this.editor.getModel();
      if (!model) return;
      const content = model.getValue();
      const uri = model.uri.toString();
      this.intoServer.send({
        jsonrpc: "2.0",
        method: "textDocument/didChange",
        params: {
          textDocument: { uri, version: 1 },
          contentChanges: [{ text: content }],
        } as DidChangeTextDocumentParams,
      } as any);
      pollDiagnostics();
    });
  }

  sendRequest(method: string, params: any) {
    const id = Date.now(); // Simple unique id; replace with better if needed
    const request = {
      jsonrpc: "2.0",
      id,
      method,
      params,
    };
    this.intoServer.send(request);
    return this.fromServer.responses.get(id);
  }
}
