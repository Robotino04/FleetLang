# Fleet

## Features (FleetC)
- Compile all valid Fleet programs to an object file using LLVM as a backend
- Give meaningful errors for invalid Fleet programs

## Features (FleetLS)
- Syntax highlighting with `textDocument/semanticTokens`
- Partial syntax highlighting for invalid Fleet programs
- Error messages for invalid Fleet programs
- Formatting for valid Fleet programs with `textDocument/formatting`
    - Automatically removes unnecessary parentheses in expressions: `((1 * 2) + 3)` → `1 * 2 + 3`

## TODOs
- warnings for unreachable code
- use `&mut errors` everywhere, mostly parser and tokenizer for now
- Add AstPass trait that consumes self for visit_program and returns the output of the pass
- replace explicit `AstNode` with `impl Into<AstNode>`
- format if statements without block to ones with block
- if expressions
- allow single manual newlines. Maybe add newline eaters after {\n and before \n} and print trivia newlines as long as they are inside a statement
- make FleetLS not terminate after the first connection drops
- type system (at least the other iX and f32)


## Building
### LLVM
> [!TIP]
> There is a Nix shell available which includes the Rust tooling, Clang, libllvm and some other libraries and automatically sets the correct environment variables for Cargo to find libllvm. This lets you skip straight to [FleetC and FleetLS](#fleetc-and-fleetls).


FleetC uses LLVM as a backend and it is therefore required to have a static build of libllvm 18. If this isn't found, Cargo should detect it and give instructions on how to acquire libllvm.
Some other libraries are also linked by `inkwell` (the LLVM crate) and may need to be installed as well.

### FleetC and FleetLS
After setting up libllvm and the other libraries, the project can be built using a simple 

```sh
cargo build
```

This should compile FleetC (the compiler) to `target/debug/fleetc` and FleetLS (the language server) to `target/debug/fleetls`.


## Setup (FleetLS)
For Neovim users, there is a `load.vim` file that sets up the Fleet filetype and configures nvim-lspconfig to expect FleetLS on port `1234`.

For VSCode users, there is no Fleet-specific extension (yet). However, there is [this extension](https://marketplace.visualstudio.com/items/?itemName=zsol.vscode-glspc), which allows a connection to any language server including FleetLS. The following settings should work to enable FleetLS for `plaintext` files:
```jsonc
{
  "glspc.server.command": "<Path-to-FleetLang-repository>/target/debug/fleetls",
  "glspc.server.commandArguments": ["--stdio"], // FleetLS serves on 0.0.0.0:1234 by default. This makes it use stdout
  "glspc.server.languageId": ["plaintext"], // can also be changed to any other filetype
}
```
**Note:** using FleetLS through VSCode isn't tested well and may not work as intended. There may be an official extension in the future.
