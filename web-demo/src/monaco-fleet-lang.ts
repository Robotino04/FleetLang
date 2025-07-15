import * as monaco from "monaco-editor";

monaco.languages.register({
  id: "fleet",
  extensions: [".fleet", ".fl"],
  aliases: ["FleetLang", "fleet"],
});

monaco.languages.setMonarchTokensProvider("fleet", {
  tokenizer: {
    root: [
      // Keywordsregister
      [
        /\b(on|self|let|as|return|if|elif|else|while|for|break|skip|extern)\b/,
        "keyword",
      ],

      // Types
      [/\b(i8|i16|i32|i64|bool|idk)\b/, "type"],

      // Boolean constants
      [/\b(true|false)\b/, "constant"],

      // String literals
      [/".*?"/, "string"],

      // Numbers
      [/\b\d+\b/, "number"],

      // Brackets
      [/[{}()\[\]]/, "@brackets"],

      // Operators
      [/==|!=|<=|>=|&&|\|\|/, "operator"],
      [/->/, "operator"],
      [/=|!|~|-|\+|\*|\/|%|>|</, "operator"],

      // Punctuation
      [/[:;,.@]/, "delimiter"],

      // Identifiers (fallback)
      [/\b[a-zA-Z_]\w*\b/, "identifier"],

      // Unknown characters
      [/[^ \t\r\n]+/, "invalid"],
    ],
  },
});
