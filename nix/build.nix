{ inputs }:

let
  inherit (inputs) nixpkgs flake-utils haskellNix iohkNix CHaP hackage;

  appConfig = import ./config.nix { };
  name      = appConfig.name;

  mkSystemOutputs = system:
    let
      lib = nixpkgs.lib;

      pkgs = import haskellNix.inputs.nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [
          haskellNix.overlay
          iohkNix.overlays.crypto
        ];
      };

      haskellProject = pkgs.haskell-nix.project' {
        src = ../.;
        compiler-nix-name = "ghc910";

        inputMap = {
          "https://chap.intersectmbo.org/" = CHaP;
          "https://hackage.haskell.org/"   = hackage;
        };

        shell = {
          tools = {
            cabal = { };
            haskell-language-server = { };
            hlint = { };
            fourmolu = { };
          };

          buildInputs = with pkgs; [
            pkg-config
            postgresql.lib
            openssl.dev
            zlib
          ];
        };

        modules = [{
          packages.http-client-tls.postPatch = ''
            substituteInPlace http-client-tls.cabal --replace-warn "memory" "ram"
          '';
        }];
      };

      backendFlake = haskellProject.flake { };

      defaultPackage =
        backendFlake.packages."${name}:exe:chase" or
        backendFlake.packages."${name}:exe:fetch-rosters" or
        (builtins.head (builtins.attrValues backendFlake.packages));

    in {
      legacyPackages = pkgs;

      packages = backendFlake.packages // {
        default = defaultPackage;
      };

      devShells = let
        shell = pkgs.mkShell {
          inherit name;

          inputsFrom = [ backendFlake.devShells.default ];

          buildInputs = with pkgs; [
            pkg-config
            openssl.dev
            zlib
            lsof
            tmux
            gettext
            jq
            gum
          ];

          shellHook = ''
            echo ""
            echo "  Chase Dev Environment"
            echo "  ================================"
            echo ""
            echo "  Build:"
            echo "    cabal build            Build all targets"
            echo "    cabal run chase     Run the CLI entry point"
            echo ""
          '';
        };
      in {
        default = shell;
      };
    };

in {
  perSystem = mkSystemOutputs;
  systems = [
    "x86_64-linux"
    "aarch64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];
}