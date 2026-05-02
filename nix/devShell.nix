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

  postgresModule = import ./postgres-utils.nix {
    inherit pkgs name;
    database = dbConfig;
  };

  sopsModule = import ./sops-dev.nix { inherit pkgs lib name; };

  manifestModule = import ./scripts/manifest.nix {
    inherit pkgs lib;
    config = {
      inherit backendPath hsDirs hsTestDirs;
      hsConfig = { cabalFile = hsConfig.cabalFile; };
    };
  };

  fileTools = import ./scripts/file-tools.nix {
    inherit pkgs name lib backendPath hsDirs hsTestDirs;
    hsConfig = hsConfig;
  };

  commonBuildInputs = with pkgs; [
    zlib
    pgcli
    pkg-config
    openssl.dev
    libiconv
    openssl
    gum

    sopsModule.sops-init-key
    sopsModule.sops-pubkey
    sopsModule.sops-bootstrap
    sopsModule.sops-get
    sopsModule.sops-exec
    sopsModule.sops-status
    pkgs.sops
    pkgs.age
    pkgs.ssh-to-age

    postgresModule.pg-start
    postgresModule.pg-connect
    postgresModule.pg-stop
    postgresModule.pg-cleanup
    postgresModule.pg-backup
    postgresModule.pg-restore
    postgresModule.pg-rotate-credentials
    postgresModule.pg-create-schema
    postgresModule.pg-stats
    postgresModule.with-db
    gettext

    manifestModule.generateScript
    fileTools.compile-manifest
    fileTools.compile-archive
    fileTools.llm-context
    fileTools.manifest-tui

    toilet rsync tmux
    coreutils bash gnused gnugrep jq perl findutils
  ];

  nativeBuildInputs = with pkgs; [
    pkg-config postgresql postgresql.lib zlib openssl.dev libiconv openssl
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
      export PGDATA="${dbConfig.dataDir}"
      export PGPORT="${toString dbConfig.port}"
      export PGUSER="${dbConfig.user}"
      export PGDATABASE="${dbConfig.name}"
      export PKG_CONFIG_PATH="${pkgs.postgresql.lib}/lib/pkgconfig:$PKG_CONFIG_PATH"

      mkdir -p "$(pwd)/script/concat_archive/output" \
               "$(pwd)/script/concat_archive/archive" \
               "$(pwd)/script/concat_archive/.hashes"

      echo "Welcome to the ${lib.toSentenceCase name} dev environment!"
      echo "Copyright (C) ${licenseConfig.years} ${licenseConfig.holder}. Licensed under the ${licenseConfig.name}."
      echo ""
      echo "Secrets:"
      ${sopsModule.loadSecretsHook}
      echo ""
      echo "Database (port ${toString dbConfig.port}):"
      echo "  pg-start               - Start PostgreSQL"
      echo "  pg-connect             - Connect with psql"
      echo "  pg-stop                - Stop PostgreSQL"
      echo "  pg-backup / pg-restore - Backup / restore"
      echo "  pg-rotate-credentials  - Rotate password"
      echo "  pg-stats               - Database statistics"
      echo "  with-db <cmd>          - Run command with DB env in scope"
      echo ""
      echo "Build / run:"
      echo "  cabal build chase   - Build the CLI"
      echo "  cabal run chase     - Run the CLI"
      echo ""
      echo "LLM context tools:"
      echo "  generate-manifest      - Scan source tree into script/manifest.json"
      echo "  compile-manifest       - Bundle source files for review"
      echo "  llm-context            - Generate context from git diff"
      echo "  manifest-tui           - Interactive TUI for above"
      echo ""
      toilet ${lib.toSentenceCase name} -t --metal 2>/dev/null || echo "${lib.toSentenceCase name}"
    '';
  };

in {
  inherit devShell;
}