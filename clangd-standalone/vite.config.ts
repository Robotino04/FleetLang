/* --------------------------------------------------------------------------------------------
 * Copyright (c) 2024 TypeFox and others.
 * Licensed under the MIT License. See LICENSE in the package root for license information.
 * ------------------------------------------------------------------------------------------ */

import { defineConfig } from "vite";
import fs from "node:fs";
import * as path from "node:path";
import vsixPlugin from "@codingame/monaco-vscode-rollup-vsix-plugin";
import importMetaUrlPlugin from "@codingame/esbuild-import-meta-url-plugin";

export default defineConfig({
  build: {
    rolldownOptions: {
      input: path.resolve(__dirname, "index.html"),
    },
  },
  server: {
    port: 3000,
    cors: {
      origin: "*",
    },
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp",
    },
  },
  optimizeDeps: {
    include: [
      "vscode/localExtensionHost",
      "vscode-jsonrpc",
      "vscode-languageclient",
      "vscode-languageserver",
      "vscode-languageserver/browser.js",
      "vscode-languageserver-protocol",
    ],
    rolldownOptions: {
      plugins: [importMetaUrlPlugin],
    }
  },
  plugins: [
    {
      // For the *-language-features extensions which use SharedArrayBuffer
      name: "configure-response-headers",
      apply: "serve",
      configureServer: (server) => {
        server.middlewares.use((_req, res, next) => {
          res.setHeader("Cross-Origin-Embedder-Policy", "credentialless");
          res.setHeader("Cross-Origin-Opener-Policy", "same-origin");
          res.setHeader("Cross-Origin-Resource-Policy", "cross-origin");

          // Fix MIME type for JS worker files
          if (_req.url?.endsWith(".js")) {
            res.setHeader("Content-Type", "application/javascript");
          }

          next();
        });
      },
    },
    vsixPlugin(),
  ],
  resolve: {
    alias: {
      "@assets": "/assets",
    },
  },
  worker: {
    format: "es",
  },
});