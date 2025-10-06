{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    flake-utils,
    naersk,
    nixpkgs,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = (import nixpkgs) {
          inherit system;
        };

        naersk' = pkgs.callPackage naersk {};
      in rec {
        fl_runtime = pkgs.clang18Stdenv.mkDerivation {
          name = "fl_runtime";
          src = ./fl_runtime;
          installPhase = ''
            mkdir -p $out/share/
            cp fl_runtime_declarations.bc $out/share/
          '';
          buildInputs = [
            pkgs.shaderc.bin #pkgs.glslang

            pkgs.libGL
            pkgs.libGL.dev

            pkgs.vulkan-headers
            pkgs.vulkan-loader
            pkgs.vulkan-validation-layers
          ];
        };

        defaultPackage = naersk'.buildPackage {
          src = ./.;
          preBuild = ''
            mkdir -p fl_runtime/
            cp "${fl_runtime}/share/fl_runtime_declarations.bc" fl_runtime/
          '';
          buildInputs = [
            pkgs.libz
            pkgs.libxml2
            pkgs.libffi
            pkgs.ncurses # pkgs.libtinfo is broken atm
            pkgs.llvmPackages_18.bintools
            pkgs.llvmPackages_18.clang-tools
            pkgs.llvmPackages_18.clang # required for tests

            pkgs.glfw
            pkgs.libGL
            pkgs.libGL.dev

            pkgs.shaderc.bin #pkgs.glslang
            pkgs.vulkan-headers
            pkgs.vulkan-loader
            pkgs.vulkan-validation-layers
          ];

          LLVM_SYS_180_PREFIX = "${pkgs.llvmPackages_18.libllvm.dev}";
          VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
          SHADERC_LIB_DIR = "${pkgs.shaderc.static}/lib/";

          #LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath packages}";

          # nvidia drivers don't clean up on shutdown and thus ASan/LSan will detect the leaks and exit the program early
          # needs to be here for tests
          LSAN_OPTIONS = "suppressions=${./lsan.supp}";
        };

        devShell = defaultPackage.overrideAttrs (prev: {
          nativeBuildInputs =
            prev.nativeBuildInputs
            ++ [
              pkgs.cargo
              pkgs.clippy
              pkgs.rustc
              pkgs.cargo-watch

              pkgs.vulkan-tools
              pkgs.vim.xxd

              pkgs.viu
            ];
        });
      }
    );
}
