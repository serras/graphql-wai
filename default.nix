let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/180779b.tar.gz) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:
let
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "c4662e6";
    sha256 = "sha256:1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
  };
  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;
  hnPkgs = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "mu-haskell";
      src = gitignoreSource ./.;
    };
  };
in {
  graphq-wai = hnPkgs.graphql-wai.components.library;
}