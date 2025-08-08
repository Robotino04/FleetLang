import * as monaco from "monaco-editor";
import Server from "./server";
import { IntoServer, FromServer } from "./codec";
import { MonacoLspTransport } from "./monaco-lsp-transport";
import "./monaco-fleet-lang";
import initWasm, { compile_to_c } from "../assets/wasm/fleetls_wasm.js";

const defaultProperties: monaco.editor.IStandaloneEditorConstructionOptions = {
  theme: "vs-dark",
  automaticLayout: true,
  "semanticHighlighting.enabled": true,
  tabSize: 4,
  autoClosingBrackets: "always",
  parameterHints: {
    enabled: true,
    cycle: true,
  },
  emptySelectionClipboard: true,
  fontFamily: "'Fira Code', monospace",
  fontSize: 15,
  fontLigatures: true,
};

// Create Monaco Editor instances
const editor = monaco.editor.create(document.getElementById("editor")!, {
  value: "let main = () -> i32 {\n    return 42;\n}\n",
  language: "fleet",
  ...defaultProperties,
});

const outputEditor = monaco.editor.create(
  document.getElementById("output-editor")!,
  {
    readOnly: true,
    readOnlyMessage: {
      value:
        "This code is automatically compiled by FleetC and can't be modified. " +
        "You can modify the Fleet code on the left however and see how this code changes!",
    },
    value: "// Compiled C output will appear here",
    language: "c",
    ...defaultProperties,
  },
);

// Setup LSP wiring (WebSocket or WASM-based)
async function setupLanguageServer() {
  // Create the LSP transport queues
  const intoServer = new IntoServer();
  const fromServer = FromServer.create();
  const server = await Server.initialize(intoServer, fromServer);
  new MonacoLspTransport(editor, intoServer, fromServer);
  return server;
}

// WASM and compile-to-c integration
async function setupCompileToC() {
  const updateOutput = () => {
    const src = editor.getValue();
    let c = outputEditor.getValue();
    try {
      c = compile_to_c(src) || c;
    } catch (e) {
      c = `// Compile error: ${e}`;
    }
    outputEditor.setValue(c);
  };
  editor.onDidChangeModelContent(updateOutput);
  updateOutput();
}

const server = await setupLanguageServer();
await setupCompileToC();
await server.start();
