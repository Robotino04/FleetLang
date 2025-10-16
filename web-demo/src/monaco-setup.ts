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
import examplePrograms from "@assets/examples.json";

monaco.editor.defineTheme("latte", convertVsCodeThemeToStandalone(latteTheme));
monaco.editor.defineTheme("mocha", convertVsCodeThemeToStandalone(mochaTheme));

function setThemeCssVars(themeName: string) {
  let colors;
  if (themeName === "latte") {
    colors = latteTheme.colors;
    document.body.classList.add("light-theme");
    document.body.classList.remove("dark-theme");
    document.body.style.setProperty(
      "--theme-bg",
      colors["breadcrumb.background"] || "#eff1f5",
    ); // Catppuccin Latte base
  } else {
    colors = mochaTheme.colors;
    document.body.classList.add("dark-theme");
    document.body.classList.remove("light-theme");
    document.body.style.setProperty(
      "--theme-bg",
      colors["breadcrumb.background"] || "#1e1e2e",
    ); // Catppuccin Mocha base
  }
  document.body.style.setProperty(
    "--theme-fg",
    colors["foreground"] || (themeName === "latte" ? "#4c4f69" : "#cdd6f4"),
  );

  document.body.style.setProperty(
    "--theme-border",
    colors["activityBar.foreground"] ||
      (themeName === "latte" ? "#8839ef" : "#cba6f7"),
  );
}

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

setThemeCssVars(defaultProperties.theme || "mocha");

// Create Monaco Editor instances

// Example programs
const editorContainer = document.getElementById("editor");
const exampleSelector = document.getElementById(
  "example-selector",
) as HTMLSelectElement;
for (let i = 0; i < examplePrograms.length; i++) {
  const option = document.createElement("option");
  option.text = examplePrograms[i].title;
  option.value = i.toString();
  exampleSelector.add(option);
}

const editor = monaco.editor.create(editorContainer!, {
  value:
    examplePrograms[exampleSelector ? Number(exampleSelector.value) : 0].code,
  language: "fleet",
  ...defaultProperties,
});

// Listen for theme changes (if you add a theme switcher in future)
// For now, update CSS vars if theme changes programmatically
const originalSetTheme = monaco.editor.setTheme;
monaco.editor.setTheme = function (themeName: string) {
  setThemeCssVars(themeName);
  originalSetTheme.call(monaco.editor, themeName);
};

// Load selected example into editor
if (exampleSelector) {
  exampleSelector.addEventListener("change", (e) => {
    const idx = Number(exampleSelector.value);
    editor.setValue(examplePrograms[idx].code);
  });
}

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

const darkModePreference = window.matchMedia("(prefers-color-scheme: dark)");
darkModePreference.addEventListener("change", (e) => {
  if (e.matches) {
    monaco.editor.setTheme("mocha");
  } else {
    monaco.editor.setTheme("latte");
  }
});

(async () => {
  const server = await setupLanguageServer();
  await setupCompileToC();
  await server.start();
})();
