// import * as jsrpc from "json-rpc-2.0";
import { MonacoToProtocolConverter, ProtocolToMonacoConverter } from "monaco-languageclient";
import * as monaco from "monaco-editor-core";
import * as proto from "vscode-languageserver-protocol";
import { standardTokens } from "monaco-languages-extended";

import Client from "./client";

export const monacoToProtocol = new MonacoToProtocolConverter(monaco);
export const protocolToMonaco = new ProtocolToMonacoConverter(monaco);

let language: null | Language;

export default class Language implements monaco.languages.ILanguageExtensionPoint {
  readonly id: string;
  readonly aliases: string[];
  readonly extensions: string[];
  readonly mimetypes: string[];

  private constructor(client: Client) {
    const { id, aliases, extensions, mimetypes } = Language.extensionPoint();
    this.id = id;
    this.aliases = aliases;
    this.extensions = extensions;
    this.mimetypes = mimetypes;
    this.registerLanguage(client);
  }

  static extensionPoint(): monaco.languages.ILanguageExtensionPoint & {
    aliases: string[];
    extensions: string[];
    mimetypes: string[];
  } {
    const id = "fleet";
    const aliases = ["Fleet", "fleet", "fl"];
    const extensions = [".fleet", ".fl"];
    const mimetypes = ["text/fleet"];
    return { id, extensions, aliases, mimetypes };
  }

  private registerLanguage(client: Client): void {
    void client;
    monaco.languages.register(Language.extensionPoint());
    monaco.languages.registerDocumentSymbolProvider(this.id, {
      async provideDocumentSymbols(model, token): Promise<monaco.languages.DocumentSymbol[]> {
        void token;
        const response = await (client.request(proto.DocumentSymbolRequest.type.method, {
          textDocument: monacoToProtocol.asTextDocumentIdentifier(model),
        } as proto.DocumentSymbolParams) as Promise<proto.SymbolInformation[]>);

        const uri = model.uri.toString();

        const result: monaco.languages.DocumentSymbol[] = protocolToMonaco.asSymbolInformations(response, uri);

        return result;
      },
    });
    monaco.languages.registerHoverProvider(this.id, {
      async provideHover(model, position, token) {
        void token;
        const response = await (client.request(proto.HoverRequest.type.method, {
          textDocument: monacoToProtocol.asTextDocumentIdentifier(model),
          position: monacoToProtocol.asPosition(position.lineNumber, position.column),
        } as proto.HoverParams) as Promise<proto.Hover | null>);

        const result: monaco.languages.Hover = response ? protocolToMonaco.asHover(response) : { contents: [] };

        return result;
      },
    });

    monaco.languages.register({ id: "rust" });
    monaco.languages.setMonarchTokensProvider("rust", standardTokens["rust"]);

    monaco.languages.registerSignatureHelpProvider(this.id, {
      signatureHelpTriggerCharacters: ["(", ","],
      async provideSignatureHelp(model, position, token, context) {
        void token;
        const response = await (client.request(proto.SignatureHelpRequest.type.method, {
          textDocument: monacoToProtocol.asTextDocumentIdentifier(model),
          position: monacoToProtocol.asPosition(position.lineNumber, position.column),
          context,
        } as proto.SignatureHelpParams) as Promise<proto.SignatureHelp>);

        const result: monaco.languages.SignatureHelpResult = {
          value: protocolToMonaco.asSignatureHelpResult(response),
          dispose: () => {},
        };

        return result;
      },
    });

    const tokenTypes = [
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
    ];
    const tokenModifiers = [
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
    ];

    monaco.languages.registerDocumentSemanticTokensProvider(this.id, {
      getLegend: () => ({ tokenTypes, tokenModifiers }),

      async provideDocumentSemanticTokens(model, lastResultId, token) {
        void lastResultId;
        void token;

        const response = await (client.request(proto.SemanticTokensRequest.type.method, {
          textDocument: monacoToProtocol.asTextDocumentIdentifier(model),
        } as proto.SemanticTokensParams) as Promise<proto.SemanticTokens | null>);

        if (!response) return { data: new Uint32Array() };

        const result: monaco.languages.SemanticTokens = {
          resultId: undefined,
          data: new Uint32Array(response?.data ?? []),
        };

        return result;
      },

      releaseDocumentSemanticTokens() {},
    });

    monaco.languages.registerDocumentFormattingEditProvider(this.id, {
      async provideDocumentFormattingEdits(model, options, token): Promise<monaco.languages.TextEdit[]> {
        void token;

        console.trace("formatting started");

        const response = await (client.request(proto.DocumentFormattingRequest.type.method, {
          textDocument: monacoToProtocol.asTextDocumentIdentifier(model),
          options,
        } as proto.DocumentFormattingParams) as Promise<proto.TextEdit[]>);
        console.trace("formatting result received");

        const result: monaco.languages.TextEdit[] = Array.isArray(response)
          ? response.map((e) => protocolToMonaco.asTextEdit(e))
          : [];

        return result;
      },
    });
  }

  static initialize(client: Client): Language {
    if (null == language) {
      language = new Language(client);
    } else {
      console.warn("Language already initialized; ignoring");
    }
    return language;
  }
}
