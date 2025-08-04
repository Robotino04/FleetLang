let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell rec {
    packages = [
      pkgs.cargo
      pkgs.rustc
      pkgs.cargo-watch
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

      pkgs.vim.xxd
    ];

    env = {
      LLVM_SYS_180_PREFIX = "${pkgs.llvmPackages_18.libllvm.dev}";

      VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
      SHADERC_LIB_DIR = "${pkgs.shaderc.static}/lib/";

      LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath packages}";

      # nvidia drivers don't clean up on shutdown and thus ASan/LSan will detect the leaks and exit the program early
      LSAN_OPTIONS = "suppressions=${./lsan.supp}";
    };
  }
