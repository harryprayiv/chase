{
  description = "chase: structural compression of Haskell source for LLM context";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs  = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskell.packages.ghc910;

        cheeblr-dist = hpkgs.callCabal2nix "cheeblr-dist" ./. { };
      in {
        packages.default      = cheeblr-dist;
        packages.cheeblr-dist = cheeblr-dist;

        apps.default = {
          type    = "app";
          program = "${cheeblr-dist}/bin/cheeblr-dist";
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




# _---__-


# {
#   description = "chase — fantasy baseball points engine";

#   inputs = {
#     haskellNix.url = "github:input-output-hk/haskell.nix";
#     nixpkgs.follows = "haskellNix/nixpkgs-unstable";

#     iohkNix = {
#       url = "github:input-output-hk/iohk-nix";
#       inputs.nixpkgs.follows = "nixpkgs";
#     };

#     hackage = {
#       url = "github:input-output-hk/hackage.nix";
#       flake = false;
#     };

#     flake-utils.url = "github:numtide/flake-utils";

#     flake-compat = {
#       url = "github:edolstra/flake-compat";
#       flake = false;
#     };

#     sops-nix = {
#       url = "github:Mic92/sops-nix";
#       inputs.nixpkgs.follows = "nixpkgs";
#     };
#   };

#   outputs = inputs@{ self, flake-utils, ... }:
#     let
#       build = import ./nix/build.nix { inherit inputs; };
#     in
#       flake-utils.lib.eachSystem build.systems (system: build.perSystem system);

#   nixConfig = {
#     extra-experimental-features = [ "nix-command flakes" "ca-derivations" ];
#     allow-import-from-derivation = "true";
#     extra-substituters = [
#       "https://cache.iog.io"
#       "https://cache.nixos.org"
#       "https://hercules-ci.cachix.org"
#     ];
#     extra-trusted-public-keys = [
#       "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
#       "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
#       "hercules-ci.cachix.org-1:ZZeDl9Va+xe9j+KqdzoBZMFJHVQ42Uu/c/1/KMC5Lw0="
#     ];
#   };
# }