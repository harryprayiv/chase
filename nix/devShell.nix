{ pkgs, name, lib, system ? builtins.currentSystem }:

let
  appConfig     = import ./config.nix { inherit name; };
  dbConfig      = appConfig.database;
  licenseConfig = appConfig.license;

  hsConfig     = appConfig.haskell;
  backendPath  = ".";

  hsDirs = map (d: builtins.replaceStrings [ "./" ] [ "" ] d) hsConfig.codeDirs;

  hsTestDirs =
    let testPath = hsConfig.tests or null;
    in if testPath != null then [ (builtins.replaceStrings [ "./" ] [ "" ] testPath) ] else [];

  commonBuildInputs = with pkgs; [
    zlib
    pgcli
    pkg-config
    openssl.dev
    libiconv
    openssl
    gum
    gettext
    toilet rsync tmux
    coreutils bash gnused gnugrep jq perl findutils
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config zlib openssl.dev libiconv openssl
    lsof tmux direnv
  ];

  darwinInputs =
    if (system == "aarch64-darwin" || system == "x86_64-darwin") then
      (with pkgs.darwin.apple_sdk.frameworks; [ Cocoa CoreServices ])
    else [];

  devShell = pkgs.mkShell {
    inherit name;
    inherit nativeBuildInputs;
    buildInputs = commonBuildInputs ++ darwinInputs;

    shellHook = ''
      echo ""
      echo "Build / run:"
      echo "  cabal build chase   - Build the CLI"
      echo "  cabal run chase     - Run the CLI"
      echo ""
      toilet ${lib.toSentenceCase name} -t --metal 2>/dev/null || echo "${lib.toSentenceCase name}"
    '';
  };

in {
  inherit devShell;
}