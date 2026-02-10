import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import "./monaco-imports";
import * as d3 from "d3";

import Server from "./server";
import { IntoServer, FromServer } from "./codec";
import { MonacoLspTransport } from "./monaco-lsp-transport";
import "./monaco-fleet-lang";
import { compile_to_c, extract_ast } from "@assets/wasm/fleetls_wasm.js";
import latteTheme from "@assets/themes/latte.json";
import mochaTheme from "@assets/themes/mocha.json";
import { convertVsCodeThemeToStandalone } from "./theming";
import examplePrograms from "@assets/examples.json";
import { ITheme, Terminal } from "@xterm/xterm";
import "@xterm/xterm/css/xterm.css";
import { FitAddon } from "@xterm/addon-fit";

monaco.editor.defineTheme("latte", convertVsCodeThemeToStandalone(latteTheme));
monaco.editor.defineTheme("mocha", convertVsCodeThemeToStandalone(mochaTheme));

const allThemes = {
  latte: latteTheme,
  mocha: mochaTheme,
};

function setThemeCssVars(themeName: keyof typeof allThemes) {
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
function setTerminalTheme(themeName: keyof typeof allThemes) {
  let colors = allThemes[themeName].colors;
  let terminal_theme: ITheme = {
    foreground: colors["terminal.foreground"],
    background: colors["editor.background"],
    cursor: colors["terminalCursor.foreground"],
    cursorAccent: colors["terminalCursor.background"],
    selectionBackground: colors["terminal.selectionBackground"],
    selectionForeground: undefined,
    selectionInactiveBackground: colors["terminal.inactiveSelectionBackground"],
    scrollbarSliderBackground: colors["scrollbarSlider.background"],
    scrollbarSliderHoverBackground: colors["scrollbarSlider.hoverBackground"],
    scrollbarSliderActiveBackground: colors["scrollbarSlider.activeBackground"],
    overviewRulerBorder: colors["editorOverviewRuler.border"],
    black: colors["terminal.ansiBlack"],
    red: colors["terminal.ansiRed"],
    green: colors["terminal.ansiGreen"],
    yellow: colors["terminal.ansiYellow"],
    blue: colors["terminal.ansiBlue"],
    magenta: colors["terminal.ansiMagenta"],
    cyan: colors["terminal.ansiCyan"],
    white: colors["terminal.ansiWhite"],
    brightBlack: colors["terminal.ansiBrightBlack"],
    brightRed: colors["terminal.ansiBrightRed"],
    brightGreen: colors["terminal.ansiBrightGreen"],
    brightYellow: colors["terminal.ansiBrightYellow"],
    brightBlue: colors["terminal.ansiBrightBlue"],
    brightMagenta: colors["terminal.ansiBrightMagenta"],
    brightCyan: colors["terminal.ansiBrightCyan"],
    brightWhite: colors["terminal.ansiBrightWhite"],
  };

  errorTerminal.options.theme = { ...terminal_theme };
}

const defaultThemeName: keyof typeof allThemes = "mocha";

const defaultProperties: monaco.editor.IStandaloneEditorConstructionOptions = {
  theme: defaultThemeName,
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
  gotoLocation: {
    multiple: "gotoAndPeek",
  },
};

// Create Monaco Editor instances
const editorContainer = document.getElementById("editor");
const editor = monaco.editor.create(editorContainer!, {
  value: examplePrograms[0].code,
  language: "fleet",
  ...defaultProperties,
});

const errorTerminalContainer = document.getElementById("error-terminal")!;
const errorTerminal = new Terminal({
  fontFamily: defaultProperties.fontFamily,
  fontSize: defaultProperties.fontSize,
});
const errorTerminalFit = new FitAddon();

errorTerminal.loadAddon(errorTerminalFit);
errorTerminal.open(errorTerminalContainer);

window.addEventListener("resize", () => {
  errorTerminalFit.fit();
});

// Listen for theme changes (if you add a theme switcher in future)
// For now, update CSS vars if theme changes programmatically
const originalSetTheme = monaco.editor.setTheme;
monaco.editor.setTheme = function (themeName: string) {
  setThemeCssVars(themeName as any);
  setTerminalTheme(themeName as any);
  originalSetTheme.call(monaco.editor, themeName);
};
monaco.editor.setTheme(defaultThemeName);

function setupExampleSelector() {
  const exampleSelector = document.getElementById(
    "example-selector",
  ) as HTMLSelectElement;
  for (let i = 0; i < examplePrograms.length; i++) {
    const option = document.createElement("option");
    option.text = examplePrograms[i].title;
    option.value = i.toString();
    exampleSelector.add(option);
  }

  // Load selected example into editor
  if (exampleSelector) {
    exampleSelector.addEventListener("change", (e) => {
      const idx = Number(exampleSelector.value);
      editor.setValue(examplePrograms[idx].code);
    });
  }
}
const outputSelector = document.getElementById(
  "output-selector",
) as HTMLSelectElement;

function selectOutputPanel(panel: "c" | "ast" | "terminal") {
  outputSelector.value = panel;

  const ast_view = document.getElementById("ast-view")!!;
  const c_view = document.getElementById("output-editor")!!;
  const error_view = document.getElementById("error-terminal")!!;

  switch (panel) {
    case "c":
      ast_view.style.display = "none";
      c_view.style.display = "block";
      error_view.style.display = "none";
      break;
    case "ast":
      c_view.style.display = "none";
      ast_view.style.display = "block";
      error_view.style.display = "none";
      break;
    case "terminal":
      c_view.style.display = "none";
      ast_view.style.display = "none";
      error_view.style.display = "block";

      errorTerminalFit.fit();
      break;
  }
}

function setupOutputSelector() {
  outputSelector.value = "c";

  const ast_view = document.getElementById("ast-view")!!;

  ast_view.setAttribute("width", "100%");
  ast_view.setAttribute("height", "100%");
  ast_view.style.width = "100%";
  ast_view.style.height = "100%";
  ast_view.style.display = "none";

  if (outputSelector) {
    outputSelector.addEventListener("change", (e) => {
      selectOutputPanel(outputSelector.value as any);
    });
  }
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
  let previous_state: "c" | "ast" | "terminal" | undefined = undefined;

  const updateOutput = () => {
    const src = editor.getValue();
    let ccode: string;
    let terminal_out: string;
    let has_error = false;

    try {
      const out = compile_to_c(src);
      ccode = out.ccode;
      terminal_out = out.warnings;
    } catch (e) {
      terminal_out = e as string;
      ccode = "// Compile error";
      has_error = true;
    }
    outputEditor.setValue(ccode);

    if (has_error) {
      if (outputSelector.value != "errors") {
        previous_state = outputSelector.value as any;
        selectOutputPanel("terminal");
      }
    } else {
      if (previous_state !== undefined) {
        selectOutputPanel(previous_state);
        previous_state = undefined;
      }
    }
    errorTerminal.clear();
    for (let line of terminal_out.split("\n")) {
      errorTerminal.writeln(line);
    }
  };
  editor.onDidChangeModelContent(updateOutput);
  updateOutput();
}

interface Tree {
  name: string;
  children: Tree[];
}

type TreeArray = (string | TreeArray)[];

function updateAstView(dataArray: TreeArray) {
  // Convert your array to a tree object suitable for D3
  function arrayToTree(array: TreeArray): any {
    const [name, ...rest] = array;
    const children = rest.map((item) =>
      Array.isArray(item) ? arrayToTree(item) : { name: item },
    );
    return { name, children };
  }

  const treeData = arrayToTree(dataArray);

  const width = 600;
  const height = 400;

  document.querySelectorAll("#ast-view *").forEach((child) => child.remove());

  const svg = d3
    .select("#ast-view")
    .attr("width", width)
    .attr("height", height);

  const g = svg.append("g").attr("transform", "translate(50, 50)");
  const linkGroup = g.append("g").classed("links", true);
  const nodeGroup = g.append("g").classed("nodes", true);

  const root = d3.hierarchy(treeData);
  const treeLayout = d3.tree<Tree>().nodeSize([80, 50]);
  treeLayout(root);

  const offsetX = width / 2;
  const offsetY = height / 2;

  const nodeEnter = nodeGroup
    .selectAll("g.node")
    .data(root.descendants())
    .join("g")
    .classed("node", true)
    .attr("transform", (d) => `translate(${d.x}, ${d.y})`);

  nodeEnter
    .append("text")
    .text((d) => d.data.name)
    .attr("text-anchor", "middle")
    .attr("dy", "0.35em")
    .attr("fill", "var(--theme-fg)")
    .style("font-family", "sans-serif")
    .style("font-size", "12px")
    .each(function (d) {
      const bbox = (this as SVGTextElement).getBBox();
      d3.select(this.parentNode as any)
        .insert("rect", "text")
        .attr("x", bbox.x - 4)
        .attr("y", bbox.y - 2)
        .attr("width", bbox.width + 8)
        .attr("height", bbox.height + 4)
        .attr("rx", 4)
        .attr("fill", "var(--theme-bg)");
    });

  linkGroup
    .selectAll("line.link")
    .data(root.links())
    .join("line")
    .classed("link", true)
    .style("stroke", "var(--theme-fg)")
    .attr("x1", (d) => d.source.x!!)
    .attr("y1", (d) => d.source.y!!)
    .attr("x2", (d) => d.target.x!!)
    .attr("y2", (d) => d.target.y!!);

  const zoom = d3
    .zoom<SVGSVGElement, unknown>()
    .scaleExtent([0.1, 2])
    .on("zoom", (event) => {
      g.attr("transform", event.transform);
    });

  const initialTransform = d3.zoomIdentity.translate(offsetX, offsetY);
  g.attr("transform", initialTransform.toString());

  svg.call(zoom as any);
  svg.call(zoom.transform as any, initialTransform);
}

async function setupAstView() {
  const updateOutput = () => {
    const src = editor.getValue();
    let c = "[]";
    try {
      c = extract_ast(src) || c;
    } catch (e) {}
    console.log(c);
    updateAstView(JSON.parse(c));
  };
  editor.onDidChangeModelContent(updateOutput);
  updateOutput();
}

function setupDarkmode() {
  const darkModePreference = window.matchMedia("(prefers-color-scheme: dark)");
  darkModePreference.addEventListener("change", (e) => {
    if (e.matches) {
      monaco.editor.setTheme("mocha");
    } else {
      monaco.editor.setTheme("latte");
    }
  });
}

(async () => {
  setupDarkmode();
  setupExampleSelector();
  setupOutputSelector();
  const server = await setupLanguageServer();
  await setupCompileToC();
  await setupAstView();
  await server.start();
})();
