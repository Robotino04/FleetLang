[![Main](https://github.com/Robotino04/FleetLang/actions/workflows/main.yml/badge.svg)](https://github.com/Robotino04/FleetLang/actions/workflows/main.yml)

# Fleet

## FleetC
The reference implementation of Fleet.

Features:
- Compile all valid Fleet programs to an object file using LLVM as a backend
- Give meaningful errors for invalid Fleet programs

## FleetLS
A language server for improving the experience of writing Fleet code. 

Features:
- Syntax highlighting with `textDocument/semanticTokens`
- Partial syntax highlighting for invalid Fleet programs
- Error messages for invalid Fleet programs
- Formatting for parsable Fleet programs with `textDocument/formatting`
    - Automatically removes unnecessary parentheses in expressions: `((1 * 2) + 3)` → `1 * 2 + 3`
- signature help
- hover information
- lots of crashes
There is a [Web-Demo](https://robotino04.github.io/FleetLang/) available which runs a VSCode-style editor in your browser, giving you access to all the features of FleetLS. You can also observe how the generated C-Code corresponds to the Fleet one and even take a look at the generated AST. Unfortunately, you can't execute the programs yet.


## Building
### LLVM
> [!TIP]
> If you have the [Nix package manager](https://wiki.nixos.org/wiki/Nix_(package_manager)) installed, you can use the included flake to build FleetC and FleetLS in a single command:
> ```sh
> nix build .
> ```
> This will perform all steps listed below and write the binaries into `./result/bin/{fleetc,fleetls}`.
>
> Note that the resulting binaries use Vulkan and therefore need to be run with [NixGL](https://github.com/nix-community/nixGL) if you aren't using a NixOS host.

FleetC uses LLVM as a backend, and it is therefore required to have a static build of libllvm 18. If this isn't found, Cargo should detect it and give instructions on how to acquire libllvm.
Some other libraries are also linked by `inkwell` (the LLVM crate) and may need to be installed as well. Even if you aren't using the Nix flake, it can still serve as a reference for which libraries are required.

### Compiling the Runtime
Compiling the Fleet runtime is required for building the Fleet tooling. Assuming you have the Vulkan SDK and shaderc installed, this can be done using

```sh
cd fl_runtime/ && make -j
```

Note that, because Fleet uses LLVM as its backend, this Makefile is hardcoded to use Clang. Using other compilers like GCC or MSVC **will not work** as we rely on generating LLVM bitcode to be consumed by FleetC. 


### FleetC and FleetLS
After setting up libllvm and the other libraries, the project can be built using a simple 

```sh
cargo build
```

This should compile the project to `target/debug/{fleetc,fleetls}`.

### Tests
Tests can only be run on Linux for now as they rely on loading `libvulkan` and `libstdc++` at runtime.
If you aren't using nix, you need to ensure those libraries are installed and can be found by dlopen.

### Windows
If you want to run Fleet on Windows, it'll be a lot of setup.
Instead, consider using WSL and following the normal [development instructions](#development-setup):
If you plan to use VS Code for writing Fleet, make sure you run VS Code inside WSL and install the extensions on WSL as well.

### Development Setup
1. Install the [Nix package manager](https://wiki.nixos.org/wiki/Nix_(package_manager)).
2. Enable flakes by writing `experimental-features = nix-command flakes` into `~/.config/nix/nix.conf`.
3. Clone this repository:
   ```sh
   git clone https://github.com/Robotino04/FleetLang.git
   ```
4. Enter the development shell and compile everything:
   ```sh
   nix develop . # enter dev shell with all dependencies
   nix run --impure github:nix-community/nixGL#nixVulkanIntel -- bash # make vulkan work on non-NixOS hosts
   cargo build --all
   ```

At this point, you are done.


## Setup (FleetLS)
For Neovim users, there is a `load.vim` file that sets up the Fleet filetype and configures nvim-lspconfig to automatically launch FleetLS. It also has commented-out options for connecting to FleetLS over TCP which can be useful for easily reading debug output. 

For VSCode users, there is no Fleet-specific extension (yet). However, there is [this extension](https://marketplace.visualstudio.com/items/?itemName=zsol.vscode-glspc), which allows a connection to any language server including FleetLS. The following settings should work to enable FleetLS for `plaintext` files:
```jsonc
{
  "glspc.server.command": "<Path-to-FleetLang-repository>/target/debug/fleetls",
  "glspc.server.commandArguments": ["stdio"], // FleetLS can also serve over TCP. This makes it use stdout
  "glspc.server.languageId": ["plaintext"], // can also be changed to any other filetype you want to hijack for Fleet files.
}
```

> [!NOTE]
> Using FleetLS through VSCode isn't tested well and may not work as intended. There may be an official extension in the future.

## Known Bugs
Some things are known to be broken.
The most significant one is that arrays and structs are currently passed to functions by value.
This breaks LLVM for large parameters because it treats each field as its own SSA register.
You can still use the C backend for programs that rely on this.
FleetC always writes the C code before starting up LLVM.
So you can just cancel the compilation after it stalls.

Also, as noted in the examples, everything is stored on the stack so you may need to make that bigger:

```sh
ulimit -s 650000 
```


## Contributing
If you wish to contribute to Fleet, that's awesome.
In order to make sure noone else is working on something already, you are encouraged to create an issue on GitHub for discussion related to your task.
After you have finished something, you can submit a Pull Request.
You can of course already open the PR before you are done to keep track of partial work. In this case, please mark your PR as a draft.

### TODOs
These are some things that need doing or fixing.
If something doesn't make sense or isn't accurate anymore, feel free to reach out or create an issue.

#### Priority
- track definitions and references of variables
- tied to ^this^: warnings for unused functions/variables
- use byval attributes for arrays and structs
- import of some sort
- llvm crashes if skip isn't last statement in block

#### Easy
- if expressions
- test array-to-array assignments in GLSL (maybe C as well)
- test bigger-than-float float literals with C and GLSL backend
- add different errors for missing Pass dependencies and for failed check_empty calls
- move runtime_type_to_byte_size somewhere common
- allow on-statements without iterators again
- add lint to make variables and functions snake_case and types CamelCase
- allow struct initializers to have any order
- Invalid IR:
  ```rust
  let test = (x: u64) -> i32 {
      return x as i32;
  }
  ```
- Allow inference here:
  ```rust
  idk {
      b: 2,
  };
  ```

#### Medium
- real mutability/constant system
- allow variable redefinition
- add quick fix system for e.g. non-block as if body
- maybe make non-block-statement fixes a code action instead of formatting. Replace `assert_compile_error_no_formatting` after
- strings
- remove lvalues maybe?
- special-case small 2D and 3D GPU dispatches that fit in the size limits
- parse statements even without semicolons (for FleetLS)
- array index bounds checking
- Unstable formatting:
  ```rust
  if (@sqrt(seed) < 1){

  }
  ```

#### Hard
- tuple types
- struct alignment and padding
- sum types
- comptime evaluation
- comptime type eval for generics (similar to zig/hblang)
- heap allocations
- after consteval, make math intrinsics a single compiler function
