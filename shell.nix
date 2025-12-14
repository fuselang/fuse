let
  # Allow overriding GRIN path via environment variable for CI
  grinPathEnv = builtins.getEnv "GRIN_PATH";
  grinPath = if grinPathEnv != "" then grinPathEnv else "/root/grin";

  # Import grin's nixpkgs sources which have LLVM 7
  grinSources = import (grinPath + "/nix/sources.nix") {};
  pkgs = import grinSources.nixpkgs {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.sbt
    pkgs.llvm_7
    pkgs.clang_7
  ];

  # Add pre-built grin to PATH and set LLVM environment variables
  shellHook = ''
    export PATH="${grinPath}/result/bin:$PATH"
    export GRIN_CC="${pkgs.clang_7}/bin/clang"
    export GRIN_OPT="${pkgs.llvm_7}/bin/opt"
    export GRIN_LLC="${pkgs.llvm_7}/bin/llc"
    echo "Fuse development environment"
    echo "grin: $(which grin 2>/dev/null || echo 'not found')"
    echo "clang: $GRIN_CC"
  '';
}
