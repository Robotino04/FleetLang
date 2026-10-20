{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    flake-utils,
    naersk,
    nixpkgs,
    rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = (import nixpkgs) {
          inherit system;

          overlays = [
            (import rust-overlay)
          ];
        };
        lib = pkgs.lib;

        toolchain =
          pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

        naersk' = pkgs.callPackage naersk {
          cargo = toolchain;
          rustc = toolchain;
        };

        llvmPackages = pkgs.llvmPackages_18;
        patchedClang = pkgs.writeShellScriptBin "clang++" ''
          exec ${llvmPackages.clang}/bin/clang++ \
            -stdlib=libc++ "$@"
        '';

        fl_runtime = llvmPackages.libcxxStdenv.mkDerivation {
          name = "fl_runtime";
          src = ./fl_runtime;
          installPhase = ''
            mkdir -p $out/share/
            cp fl_runtime_declarations.bc fl_runtime.o fl_runtime.so testhook.so $out/share/
          '';
          buildInputs = [
            pkgs.shaderc.bin #pkgs.glslang

            pkgs.vulkan-headers
            pkgs.vulkan-loader
            pkgs.vulkan-validation-layers
          ];
        };

        shared_rust_src = lib.fileset.unions [
          ./Cargo.lock
          ./rust-toolchain.toml
          ./Cargo.toml
          ./fleet
          ./fleetc
          ./fleetls
          ./fleetls-lib
          ./fleetls-wasm
          ./fl_runtime/fl_runtime.h
        ];

        shared_attrs = rec {
          src = lib.fileset.toSource {
            root = ./.;
            fileset = shared_rust_src;
          };
          preBuild = ''
            mkdir -p fl_runtime/
            cp "${fl_runtime}/share/fl_runtime_declarations.bc" fl_runtime/
          '';

          doCheck = true;
          checkInputs = [
            pkgs.mesa
            llvmPackages.libcxx
            patchedClang
          ];
          preCheck = ''
            mkdir -p fl_runtime/
            cp ${fl_runtime}/share/{fl_runtime.o,fl_runtime.so,testhook.so} fl_runtime/

            export VK_ICD_FILENAMES=${pkgs.mesa}/share/vulkan/icd.d/lvp_icd.x86_64.json
            export MESA_SHADER_CACHE_DIR=/tmp/mesa_shader_cache
          '';

          buildInputs = [
            pkgs.zlib
            pkgs.libbfd
            pkgs.libxml2
            pkgs.libffi
            pkgs.ncurses # pkgs.libtinfo is broken atm (see https://github.com/NixOS/nixpkgs/issues/387912)

            pkgs.shaderc.bin #pkgs.glslang
            pkgs.vulkan-headers
            pkgs.vulkan-loader
            pkgs.vulkan-validation-layers
          ];

          env = {
            LLVM_SYS_180_PREFIX = "${llvmPackages.libllvm.dev}";
            VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
            SHADERC_LIB_DIR = lib.makeLibraryPath [pkgs.shaderc.lib];

            LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";

            # nvidia drivers don't clean up on shutdown and thus ASan/LSan will detect the leaks and exit the program early
            # needs to be here for tests
            LSAN_OPTIONS = "suppressions=${./lsan.supp}";

            # rustc segfaults inside lsp-types otherwise
            RUST_MIN_STACK = 16777216;
          };
        };
      in rec {
        defaultPackage = packages.fleet;
        packages = {
          all = pkgs.writeTextFile {
            name = "all.txt";
            text = lib.pipe packages [
              (pkgs.lib.filterAttrs (name: value: name != "all"))
              pkgs.lib.attrValues
              (pkgs.lib.map (package: "${package}"))
              (pkgs.lib.concatStringsSep "\n")
            ];
          };
          fleet = naersk'.buildPackage shared_attrs;

          fleetls-wasm = naersk'.buildPackage (
            shared_attrs
            // {
              pname = "fleetls-wasm";

              release = false;
              copyLibs = true;
              copyBins = false;
              overrideMain = old: {
                preConfigure = ''
                  cargo_build_options="$cargo_build_options --profile wasm-release --target wasm32-unknown-unknown --package fleetls-wasm"
                '';
                nativeBuildInputs =
                  old.nativeBuildInputs
                  ++ [
                    llvmPackages.bintools # lld for wasm-streams
                  ];
              };
            }
          );
          fleetls-wasm-bindgened = llvmPackages.libcxxStdenv.mkDerivation {
            pname = "fleetls-wasm-bindgened";
            version = "0.1";
            src = ./fleetls-wasm;

            nativeBuildInputs = [
              pkgs.wasm-bindgen-cli
              pkgs.binaryen
            ];

            buildPhase = ''
              mkdir -p $out/lib
              make RELEASE_PATH=${packages.fleetls-wasm}/lib/ OUT_PATH=$out/lib/ -o FORCE release # override always compiling with cargo
            '';
            installPhase = ''
              # do nothing because buildPhase already installed things
            '';
          };

          web-demo = pkgs.buildNpmPackage {
            name = "fleetls-web-demo";

            src = ./web-demo;
            npmDepsHash = "sha256-YKtF1opf0uMdzHH1ZgD8437gtxfe89zVez0BM1dxXpI=";

            npmBuildScript = "build";

            preBuild = ''
              mkdir -p assets/wasm/
              cp -r ${packages.fleetls-wasm-bindgened}/lib/* assets/wasm/
            '';

            installPhase = ''
              mkdir -p $out/
              mv dist/* $out/
              #cp -r package.json node_modules $out/
            '';
          };
        };
        devShells.default =
          (pkgs.mkShell.override {
            stdenv = llvmPackages.libcxxStdenv;
          })
          {
            nativeBuildInputs = [
              pkgs.rustup
              pkgs.cargo-watch

              llvmPackages.bintools
              llvmPackages.clang-tools
              llvmPackages.lldb

              pkgs.raylib
              pkgs.python3

              pkgs.vulkan-tools
              pkgs.vim.xxd

              pkgs.viu

              # TODO: read from the web-demo package
              pkgs.wasm-bindgen-cli
              pkgs.binaryen
              pkgs.nodejs
            ];

            buildInputs = shared_attrs.buildInputs;

            env =
              shared_attrs.env
              // {
                LD_LIBRARY_PATH = "/run/opengl-driver/lib:/run/opengl-driver-32/lib:" + shared_attrs.env.LD_LIBRARY_PATH;
                #LD_PRELOAD = "${pkgs.libsegfault}/lib/libsegfault.so";

                # stop the devshell from using the system glibc
                NIX_CFLAGS_LINK = "-L${pkgs.glibc}/lib";
              };
          };
      }
    );
}
