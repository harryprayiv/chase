{
  description = "chase: structural compression of Haskell source for LLM context";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    flake-utils.url = "github:numtide/flake-utils";

    grace = {
      url = "github:Gabriella439/grace";
    };

    purescript-src = {
      url = "github:purescript/purescript/v0.15.16";
      flake = false;
    };

    unison-nix = {
      url = "github:ceedubs/unison-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, grace, purescript-src, unison-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc96";

        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
          overlays = [
            grace.overlays.${compiler}
            unison-nix.overlay

            (final: prev:
              let hlib = final.haskell.lib;
              in {
                haskell = prev.haskell // {
                  packages = prev.haskell.packages // {
                    ${compiler} = prev.haskell.packages.${compiler}.override (old: {
                      overrides = final.lib.composeExtensions
                        (old.overrides or (_: _: { }))
                        (hself: hsuper: {
                          grace =
                            hlib.dontCheck
                              (hlib.dontHaddock
                                (hsuper.callPackage
                                  "${grace}/dependencies/grace.nix" { }));
                        });
                    });
                  };
                };
              })
          ];
        };

        hpkgs = pkgs.haskell.packages.${compiler};

        vendor-purescript-cst = import ./vendor.nix {
          inherit pkgs purescript-src;
        };

        chase = hpkgs.callCabal2nix "chase" ./. { };
      in {
        packages = {
          default = chase;
          chase   = chase;
          inherit vendor-purescript-cst;
        };

        apps = {
          default = {
            type    = "app";
            program = "${chase}/bin/chase";
          };

          chase-annotate = {
            type    = "app";
            program = "${chase}/bin/chase-annotate";
          };

          chase-unison = {
            type    = "app";
            program = "${chase}/bin/chase-unison";
          };

          vendor-purescript-cst = {
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
        };

        devShells.default = hpkgs.shellFor {
          packages = _: [ chase ];

          nativeBuildInputs = with hpkgs; [
            cabal-install
            haskell-language-server
            ghcid
            fourmolu
            pkgs.unison-ucm
            (pkgs.writeShellApplication {
              name = "ucm-serve";
              runtimeInputs = [ pkgs.unison-ucm ];
              text = ''
                # Starts the codebase server with no interactive REPL.
                # ucm prints the API base URL (scheme://host:port/token) on
                # startup; copy it and run:
                #   chase-unison <thatUrl> <project> <branch> [namespace]
                exec ucm headless "$@"
              '';
            })
          ];

          withHoogle = true;
        };
      });
}