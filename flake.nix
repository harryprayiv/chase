{
  description = "chase: structural compression of Haskell source for LLM context";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-src = {
      url = "github:purescript/purescript/v0.15.16";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, purescript-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs  = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskell.packages.ghc910;

        vendor-purescript-cst = import ./vendor.nix {
          inherit pkgs purescript-src;
        };

        cheeblr-dist = hpkgs.callCabal2nix "cheeblr-dist" ./. { };
      in {
        packages.default               = cheeblr-dist;
        packages.cheeblr-dist          = cheeblr-dist;
        packages.vendor-purescript-cst = vendor-purescript-cst;

        apps.default = {
          type    = "app";
          program = "${cheeblr-dist}/bin/cheeblr-dist";
        };

        apps.vendor-purescript-cst = {
          type = "app";
          program = toString (pkgs.writeShellScript "install-vendor-purescript-cst" ''
            set -euo pipefail
            target="$PWD/vendor/purescript-cst"
            rm -rf "$target"
            mkdir -p "$target"
            cp -r ${vendor-purescript-cst}/. "$target/"
            chmod -R u+rw "$target"
            echo "Vendored purescript CST to $target"
          '');
        };

        devShells.default = hpkgs.shellFor {
          packages = _: [ cheeblr-dist ];
          buildInputs = with hpkgs; [
            cabal-install
            haskell-language-server
            ghcid
            fourmolu
          ];
        };
      });
}