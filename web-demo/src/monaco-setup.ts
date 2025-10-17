import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import "./monaco-imports";
import * as d3 from "d3";

import Server from "./server";
import { IntoServer, FromServer } from "./codec";
import { MonacoLspTransport } from "./monaco-lsp-transport";
import "./monaco-fleet-lang";
import initWasm, {
  compile_to_c,
  extract_ast,
} from "@assets/wasm/fleetls_wasm.js";
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
const editorContainer = document.getElementById("editor");
const editor = monaco.editor.create(editorContainer!, {
  value: examplePrograms[0].code,
  language: "fleet",
  ...defaultProperties,
});

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
}
function setupOutputSelector() {
  const outputSelector = document.getElementById(
    "output-selector",
  ) as HTMLSelectElement;

  const ast_view = document.getElementById("ast-view")!!;
  const c_view = document.getElementById("output-editor")!!;

  ast_view.setAttribute("width", "100%");
  ast_view.setAttribute("height", "100%");
  ast_view.style.width = "100%";
  ast_view.style.height = "100%";
  ast_view.style.display = "none";

  if (outputSelector) {
    outputSelector.addEventListener("change", (e) => {
      switch (outputSelector.value) {
        case "c":
          ast_view.style.display = "none";
          c_view.style.display = "block";
          break;
        case "ast":
          c_view.style.display = "none";
          ast_view.style.display = "block";
          break;
      }
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
