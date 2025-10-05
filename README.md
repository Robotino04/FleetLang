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
- signature help
- hover information


## TODOs

### Priority
- add way to get length of array
- track definitions and references of variables
- tied to ^this^: warnings for unused functions/variables
- fix nested structs in glsl (mostly copy-paste from c)
- use byval attributes for arrays and structs

#### Meta
- make sure web demo compiles and works
- Make sure README is up-to-date and actually good.

### easy
- if expressions
- consistent error language style
- test array-to-array assignments in glsl (maybe c as well)
- test bigger-than-float float literals with c and glsl backend
- disallow returning arrays
- add different errors for missing Pass dependencies and for failed check_empty calls
- move runtime_type_to_byte_size somewhere common
- allow on-statements without iterators again
- add lint to make variables and functions snake_case and types CamelCase
- allow struct initializers to have any order

### medium
- real mutability/constant system
- allow variable redefinition
- add quick fix system for e.g. non-block as if body
- maybe make non-block-statement fixes a code action instead of formatting. Replace `assert_compile_error_no_formatting` after
- strings
- remove lvalues maybe?
- special-case small 2D and 3D GPU dispatches that fit in the size limits
- parse statements even without semicolons (for lsp)

### hard
- tuple types
- struct alignment and padding
- sum types
- comptime evaluation
- comptime type eval for generics (similar to zig/hblang)
- heap allocations
- after consteval, make math intrinsics a single compiler function


## Building
### LLVM
> [!TIP]
> There is a Nix shell available which includes the Rust tooling, Clang, libllvm and some other libraries and automatically sets the correct environment variables for Cargo to find libllvm. This lets you skip straight to [Compiling the Runtime](#compiling-the-runtime).


FleetC uses LLVM as a backend, and it is therefore required to have a static build of libllvm 18. If this isn't found, Cargo should detect it and give instructions on how to acquire libllvm.
Some other libraries are also linked by `inkwell` (the LLVM crate) and may need to be installed as well.

### Compiling the Runtime
Compiling parts of the fl runtime is required for building fleet. Assuming you are using the nix shell, this can be done using
```sh
cd fl_runtime/ && make -j
```

Without nix, you need to have the Vulkan headers (or the whole SDK) installed. You can then either try if the Makefile works or adjust it to the point where you can compile and run the example. FleetC also needs the `fl_runtime_declarations.bc` file so make sure to also build that.


### FleetC and FleetLS
After setting up libllvm and the other libraries, the project can be built using a simple 

```sh
cargo build
```

This should compile FleetC (the compiler) to `target/debug/fleetc` and FleetLS (the language server) to `target/debug/fleetls`.

### Tests
Tests can only be run on Linux for now as they rely on loading `libvulkan` and `libstdc++` at runtime.
If you aren't using nix, you need to ensure those libraries are installed and can be found by dlopen.
Make sure to also compile `fl_runtime.so` and `testhook.so` in case you aren't using the Makefile.


## Setup (FleetLS)
For Neovim users, there is a `load.vim` file that sets up the Fleet filetype and configures nvim-lspconfig to expect FleetLS on port `1234`.

For VSCode users, there is no Fleet-specific extension (yet). However, there is [this extension](https://marketplace.visualstudio.com/items/?itemName=zsol.vscode-glspc), which allows a connection to any language server including FleetLS. The following settings should work to enable FleetLS for `plaintext` files:
```jsonc
{
  "glspc.server.command": "<Path-to-FleetLang-repository>/target/debug/fleetls",
  "glspc.server.commandArguments": ["stdio"], // FleetLS serves on 0.0.0.0:1234 by default. This makes it use stdout
  "glspc.server.languageId": ["plaintext"], // can also be changed to any other filetype
}
```

> [!NOTE]
> Using FleetLS through VSCode isn't tested well and may not work as intended. There may be an official extension in the future.
