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
      pkgs.llvmPackages_18.clang
    ];

    env = {
      LLVM_SYS_180_PREFIX = "${pkgs.llvmPackages_18.libllvm.dev}";
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
