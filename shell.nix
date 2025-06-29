let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    packages = [
      pkgs.cargo
      pkgs.rustc
      pkgs.cargo-watch
      pkgs.libz
      pkgs.libxml2
      pkgs.libffi
      pkgs.ncurses # pkgs.libtinfo is broken atm
      pkgs.llvmPackages_18.clang-tools
      pkgs.llvmPackages_18.clang # required for tests

      pkgs.glfw
      pkgs.libGL
      pkgs.libGL.dev

      pkgs.glslang
      pkgs.vulkan-headers
      pkgs.vulkan-loader
      pkgs.vulkan-validation-layers
    ];

    env = {
      LLVM_SYS_180_PREFIX = "${pkgs.llvmPackages_18.libllvm.dev}";

      VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
      /*
      LLVM_SYS_180_PREFIX = "${(
        pkgs.buildEnv {
          name = "libllvm-all";
          paths = pkgs.llvmPackages_18.libllvm.all;
        }
      )}";
      LIBCLANG_PATH = "${
        ((pkgs.libclang.override (prev: {
            }))
          .overrideAttrs (prev: {
            cmakeFlags = prev.cmakeFlags ++ [(pkgs.lib.cmakeBool "LIBCLANG_BUILD_STATIC" true)];
          }))
        .lib
      }/lib";
      */
    };
  }
