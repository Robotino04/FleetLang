import { defineConfig } from "vite";
import monacoEditorPlugin from "vite-plugin-monaco-editor";
import { EditorLanguageWorks } from "vite-plugin-monaco-editor/dist/lnaguageWork";

export default defineConfig({
  plugins: [
    monacoEditorPlugin({
      // You can add languages or features here if needed
      languageWorkers: ["editorWorkerService"],
    }),
  ],
  resolve: {
    alias: {
      "@wasm": "/assets/wasm",
    },
  },
  build: {
    outDir: "dist",
    rollupOptions: {
      input: "src/server.ts",
    },
  },
  server: {
    port: 3000,
    open: true,
  },
});
