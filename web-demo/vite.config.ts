import { defineConfig } from "vite";
import monacoEditorPlugin from "vite-plugin-monaco-editor";
import { visualizer } from "rollup-plugin-visualizer";

export default defineConfig({
  plugins: [
    monacoEditorPlugin({
      languageWorkers: [
        /*"editorWorkerService"*/
      ],
      globalAPI: false,
    }),
  ],
  resolve: {
    alias: {
      "@assets": "/assets",
    },
  },
  build: {
    outDir: "dist",
    rollupOptions: {
      external: [],
      plugins: [
        visualizer({
          filename: "dist/stats.html",
          open: false,
          gzipSize: true,
          brotliSize: true,
        }),
      ],
    },
  },
  server: {
    port: 3000,
    open: true,
  },
});
