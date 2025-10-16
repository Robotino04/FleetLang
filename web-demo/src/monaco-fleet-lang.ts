import * as monaco from "monaco-editor/esm/vs/editor/editor.api";

monaco.languages.register({
  id: "fleet",
  extensions: [".fleet", ".fl"],
  aliases: ["FleetLang", "fleet"],
});

function makeNegativeLookahead(words: string[]) {
  return `(?!\\b(?:${words.join("|")})\\b)`;
}

let reserved_words = {
  keywords: [
    "on",
    "let",
    "as",
    "return",
    "if",
    "elif",
    "else",
    "while",
    "for",
    "break",
    "skip",
    "extern",
    "struct",
  ],

  builtins: ["self"],

  booleanConstants: ["true", "false"],

  types: [
    "i8",
    "i16",
    "i32",
    "i64",
    "u8",
    "u16",
    "u32",
    "u64",
    "bool",
    "idk",
    "f32",
    "f64",
    "type",
  ],
};

const excludedWords = [
  ...reserved_words.keywords,
  ...reserved_words.builtins,
  ...reserved_words.types,
  ...reserved_words.booleanConstants,
];
const negativeLookahead = makeNegativeLookahead(excludedWords);
const functionCallRegex = new RegExp(
  `\\b${negativeLookahead}([a-zA-Z_]\\w*)(?=\\s*\\()`,
);

const tokenizer: monaco.languages.IMonarchLanguage = {
  defaultToken: "",
  tokenPostfix: "",

  keywords: reserved_words.keywords,
  builtins: reserved_words.builtins,
  booleanConstants: reserved_words.booleanConstants,
  types: reserved_words.types,

  operators: [
    "==",
    "!=",
    "<=",
    ">=",
    "&&",
    "||",
    "->",
    "=",
    "!",
    "~",
    "-",
    "+",
    "*",
    "/",
    "%",
    ">",
    "<",
    "@",
  ],

  // Regexes
  symbols: /[=><!~?:&|+\-*\/%]+/,
  escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{2}|u[0-9A-Fa-f]{4})/,
  digits: /\b\d+(_+\d+)*\b/,
  float: /\b\d+\.\d*\b/,

  tokenizer: {
    root: [
      // Function definitions
      [
        /\b(let)(\s+)([a-zA-Z_]\w*)(?=\s*=\s*\([^)]*\)\s*->)/,
        ["keyword.control.fleet", "", "entity.name.function.fleet"],
      ],

      // Function calls
      [functionCallRegex, "variable.function.fleet"],

      // Keywords, types, builtins, boolean constants
      [
        /\b[a-zA-Z_]\w*\b/,
        {
          cases: {
            "@keywords": "keyword.control.fleet",
            "@types": "entity.name.type.fleet",
            "@builtins": "variable.language.fleet",
            "@booleanConstants": "constant.language.boolean.fleet",
            "@default": "variable.other.readwrite.fleet",
          },
        },
      ],

      // Whitespace
      { include: "@whitespace" },

      // Operators (multi-character first)
      [/(==|!=|<=|>=|&&|\|\||->)/, "keyword.operator.fleet"],
      [/[=!~\-+*/%><@]/, "keyword.operator.fleet"],

      // Punctuation
      [/[{}()\[\]]/, "punctuation.section.brackets.fleet"],
      [/[:;,\.@]/, "punctuation.separator.fleet"],

      // Numbers
      [/@float/, "constant.numeric.float.fleet"],
      [/@digits/, "constant.numeric.integer.fleet"],

      // Strings
      [/"/, "punctuation.definition.string.begin.fleet", "@string_double"],
      [/'/, "punctuation.definition.string.begin.fleet", "@string_single"],
    ],

    whitespace: [
      [/[ \t\r\n]+/, ""],
      [/\/\/.*$/, "comment.line.fleet"],
      [/\/\*/, "punctuation.definition.comment.fleet", "@comment"],
    ],

    comment: [
      [/[^/*]+/, "comment.block.fleet"],
      [/\/\*/, "comment.block.fleet", "@push"],
      [/\*\//, "punctuation.definition.comment.fleet", "@pop"],
      [/[\/*]/, "comment.block.fleet"],
    ],

    string_double: [
      [/[^\\"\n]+/, "string.quoted.double.fleet"],
      [/@escapes/, "constant.character.escape.fleet"],
      [/\\./, "invalid.illegal.fleet"],
      [/"/, "punctuation.definition.string.end.fleet", "@pop"],
    ],

    string_single: [
      [/[^\\'\n]+/, "string.quoted.single.fleet"],
      [/@escapes/, "constant.character.escape.fleet"],
      [/\\./, "invalid.illegal.fleet"],
      [/'/, "punctuation.definition.string.end.fleet", "@pop"],
    ],
  },
};

monaco.languages.setMonarchTokensProvider("fleet", tokenizer);
