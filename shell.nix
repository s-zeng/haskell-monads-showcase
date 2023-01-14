{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.11.tar.gz") {} }:
let

  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
        "
    '';
  };

in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ 
    stack-wrapped
    haskell.compiler.ghc8107
  ];
  packages = with pkgs; [
    (haskell-language-server.override { supportedGhcVersions = [ "8107" ]; })
  ];
  NIX_PATH = "nixpkgs=" + pkgs.path;
}
