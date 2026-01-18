let
  pkgs = import <nixpkgs> {};

  # Fetch GRIN from fuselang/grin flake with Boehm GC support
  grinFlake = builtins.getFlake "github:fuselang/grin/boehm-gc";
  grin = grinFlake.packages.${builtins.currentSystem}.default;

  # Import LLVM 15 from nixos-23.11 (same as grin flake)
  pkgs-23-11 = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz";
  }) {};
  llvm15 = pkgs-23-11.llvmPackages_15;
in
pkgs.mkShell {
  buildInputs = [
    grin
    pkgs.boehmgc
    llvm15.clang
    llvm15.llvm
  ];

  shellHook = ''
    export GC_INCLUDE="${pkgs.boehmgc}/include"
    export GC_LIB="${pkgs.boehmgc}/lib"
    export GRIN_CC="${llvm15.clang}/bin/clang"
    export GRIN_OPT="${llvm15.llvm}/bin/opt"
    export GRIN_LLC="${llvm15.llvm}/bin/llc"
  '';
}
