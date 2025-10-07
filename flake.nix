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

        llvmPackages = pkgs.llvmPackages_18;

        fl_runtime = llvmPackages.stdenv.mkDerivation {
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

        shared_attrs = rec {
          src = ./.;
          preBuild = ''
            mkdir -p fl_runtime/
            cp "${fl_runtime}/share/fl_runtime_declarations.bc" fl_runtime/
          '';

          doCheck = true;
          checkInputs = [
            pkgs.mesa
            llvmPackages.clang
          ];
          preCheck = ''
            mkdir -p fl_runtime/
            cp ${fl_runtime}/share/{fl_runtime.o,fl_runtime.so,testhook.so} fl_runtime/

            export VK_ICD_FILENAMES=${pkgs.mesa}/share/vulkan/icd.d/lvp_icd.x86_64.json
            export MESA_SHADER_CACHE_DIR=/tmp/mesa_shader_cache
          '';

          buildInputs = [
            pkgs.libz
            pkgs.libxml2
            pkgs.libffi
            pkgs.ncurses # pkgs.libtinfo is broken atm (see https://github.com/NixOS/nixpkgs/issues/387912)

            pkgs.shaderc.bin #pkgs.glslang
            pkgs.vulkan-headers
            pkgs.vulkan-loader
            pkgs.vulkan-validation-layers
          ];

          LLVM_SYS_180_PREFIX = "${llvmPackages.libllvm.dev}";
          VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
          SHADERC_LIB_DIR = "${pkgs.shaderc.static}/lib/";

          LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";

          # nvidia drivers don't clean up on shutdown and thus ASan/LSan will detect the leaks and exit the program early
          # needs to be here for tests
          LSAN_OPTIONS = "suppressions=${./lsan.supp}";
        };
      in {
        defaultPackage = naersk'.buildPackage shared_attrs;
        devShell = naersk'.buildPackage (
          shared_attrs
          // {
            # don't forget to merge arrays manually
            singleStep = true;
            nativeBuildInputs = [
              pkgs.cargo
              pkgs.clippy
              pkgs.rustc
              pkgs.cargo-watch

              llvmPackages.bintools
              llvmPackages.clang-tools
              llvmPackages.clang

              pkgs.raylib

              pkgs.vulkan-tools
              pkgs.vim.xxd

              pkgs.viu
            ];
            LD_LIBRARY_PATH = "/run/opengl-driver/lib:/run/opengl-driver-32/lib:" + shared_attrs.LD_LIBRARY_PATH;
          }
        );
      }
    );
}
