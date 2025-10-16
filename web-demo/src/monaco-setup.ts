import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import "./monaco-imports";

// IDEA: import "monaco-editor/esm/vs/editor/contrib/documentSymbols/browser/documentSymbols";
// IDEA: import "monaco-editor/esm/vs/editor/contrib/documentSymbols/browser/outlineModel";

import Server from "./server";
import { IntoServer, FromServer } from "./codec";
import { MonacoLspTransport } from "./monaco-lsp-transport";
import "./monaco-fleet-lang";
import initWasm, { compile_to_c } from "@assets/wasm/fleetls_wasm.js";
import latteTheme from "@assets/themes/latte.json";
import mochaTheme from "@assets/themes/mocha.json";
import { convertVsCodeThemeToStandalone } from "./theming";

monaco.editor.defineTheme("latte", convertVsCodeThemeToStandalone(latteTheme));
monaco.editor.defineTheme("mocha", convertVsCodeThemeToStandalone(mochaTheme));

const defaultProperties: monaco.editor.IStandaloneEditorConstructionOptions = {
  theme: "mocha",
  automaticLayout: true,
  "semanticHighlighting.enabled": false, // too lazy to figure out the correct scopes and theming stuff
  tabSize: 4,
  autoClosingBrackets: "always",
  parameterHints: {
    enabled: true,
    cycle: true,
  },
  emptySelectionClipboard: true,
  fontFamily:
    "'Maple Mono NF', 'Maple Mono', 'Fira Code', 'Source Code Pro', 'JetBrains Mono', 'Consolas', 'Courier New', 'Monaco', 'Inconsolata', 'Ubuntu Mono', 'Cascadia Code', 'IBM Plex Mono', 'Roboto Mono', 'Hack', 'Anonymous Pro', 'Menlo', 'Operator Mono', monospace",
  fontSize: 15,
  fontLigatures: true,
};

// Create Monaco Editor instances
const editor = monaco.editor.create(document.getElementById("editor")!, {
  value: "let main = () -> i32 {\n    return 42;\n}\n",
  language: "fleet",
  ...defaultProperties,
});

function createTemporaryOutputMessage(
  percentage: number,
  bytes_done: number,
  bytes_total: number,
) {
  return (
    "// Compiled C output will appear here\n" +
    "// FleetLS is currently being loaded:\n" +
    `// ${(percentage * 100).toFixed(2)}% (${(bytes_done / 1024 / 1024).toFixed(2)}MB / ${(bytes_total / 1024 / 1024).toFixed(2)}MB)`
  );
}

const outputEditor = monaco.editor.create(
  document.getElementById("output-editor")!,
  {
    readOnly: true,
    readOnlyMessage: {
      value:
        "This code is automatically compiled by FleetC and can't be modified. " +
        "You can modify the Fleet code on the left however and see how this code changes!",
    },
    value: createTemporaryOutputMessage(0, 0, 0),
    language: "c",
    ...defaultProperties,
  },
);

// Setup LSP wiring (WebSocket or WASM-based)
async function setupLanguageServer() {
  // Create the LSP transport queues
  const intoServer = new IntoServer();
  const fromServer = FromServer.create();
  const server = await Server.initialize(
    intoServer,
    fromServer,
    (percent, bytes_done, bytes_total) => {
      outputEditor.setValue(
        createTemporaryOutputMessage(percent, bytes_done, bytes_total),
      );
    },
  );
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
