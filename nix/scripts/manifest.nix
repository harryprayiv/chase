{ lib, pkgs, config ? { } }:

let
  defaultConfig = {
    projectRoot = ".";
    backendPath = ".";
    hsDirs      = [ "lib" "src-new" "app" ];
    hsTestDirs  = [ "test" ];
    hsConfig = {
      cabalFile  = null;
      extensions = [ ".hs" ];
    };
    nixConfig = {
      extensions = [ ".nix" ];
      dirs       = [ "." "nix" ];
    };
    excludePatterns = [
      "dist"
      "dist-newstyle"
      ".direnv"
      ".envrc"
      "result"
    ];
  };

  cfg = lib.recursiveUpdate defaultConfig config;

  excludePatternStr = lib.concatMapStringsSep "\\|" (p: p) cfg.excludePatterns;

  emitJsonArray = varName: ''
    if [ ''${#${varName}[@]} -gt 0 ]; then
      for i in "''${!${varName}[@]}"; do
        if [ $i -eq $(( ''${#${varName}[@]} - 1 )) ]; then
          echo "      \"''${${varName}[$i]}\""
        else
          echo "      \"''${${varName}[$i]}\","
        fi
      done
    fi
  '';

  generateManifestScript = pkgs.writeShellScriptBin "generate-manifest" ''
    set -euo pipefail

    PROJECT_ROOT="$(pwd)"
    SCRIPT_DIR="$PROJECT_ROOT/script"
    MANIFEST_FILE="$SCRIPT_DIR/manifest.json"
    mkdir -p "$SCRIPT_DIR"

    echo "Generating manifest..."

    HASKELL_DIRS=()
    for dir in ${lib.concatStringsSep " " cfg.hsDirs}; do
      HASKELL_DIRS+=("$PROJECT_ROOT/$dir")
    done

    HS_TEST_DIRS=()
    ${if cfg.hsTestDirs != [] then ''
    for dir in ${lib.concatStringsSep " " cfg.hsTestDirs}; do
      HS_TEST_DIRS+=("$PROJECT_ROOT/$dir")
    done
    '' else ""}

    echo "Finding Haskell source files..."
    HS_FILES=()
    for dir in "''${HASKELL_DIRS[@]}"; do
      if [ -d "$dir" ]; then
        echo "  Scanning $dir"
        while IFS= read -r file; do
          [ -n "$file" ] && HS_FILES+=("''${file#$PROJECT_ROOT/}")
        done < <(find "$dir" -type f -name "*.hs" 2>/dev/null | grep -v "${excludePatternStr}" | sort)
      fi
    done

    echo "Finding Haskell test files..."
    HS_TEST_FILES=()
    for dir in "''${HS_TEST_DIRS[@]+"''${HS_TEST_DIRS[@]}"}"; do
      if [ -d "$dir" ]; then
        echo "  Scanning $dir"
        while IFS= read -r file; do
          [ -n "$file" ] && HS_TEST_FILES+=("''${file#$PROJECT_ROOT/}")
        done < <(find "$dir" -type f -name "*.hs" 2>/dev/null | grep -v "${excludePatternStr}" | sort)
      fi
    done

    echo "Finding Nix files..."
    NIX_FILES=()
    while IFS= read -r file; do
      [[ "$file" != *"/script/concat_archive/"* ]] && NIX_FILES+=("''${file#$PROJECT_ROOT/}")
    done < <(find "$PROJECT_ROOT" -maxdepth 1 -type f -name "*.nix" 2>/dev/null | sort)
    if [ -d "$PROJECT_ROOT/nix" ]; then
      while IFS= read -r file; do
        NIX_FILES+=("''${file#$PROJECT_ROOT/}")
      done < <(find "$PROJECT_ROOT/nix" -type f -name "*.nix" 2>/dev/null | sort)
    fi

    {
      echo "{"
      echo "  \"meta\": {"
      echo "    \"generated\": \"$(date '+%s')\","
      echo "    \"humanTime\": \"$(date '+%Y-%m-%d %H:%M:%S')\","
      echo "    \"projectRoot\": \"$PROJECT_ROOT\""
      echo "  },"

      echo "  \"haskell\": {"
      echo "    \"include\": ["
      ${emitJsonArray "HS_FILES"}
      echo "    ],"
      echo "    \"exclude\": [],"
      echo "    \"count\": ''${#HS_FILES[@]}"
      echo "  },"

      echo "  \"haskellTests\": {"
      echo "    \"include\": ["
      ${emitJsonArray "HS_TEST_FILES"}
      echo "    ],"
      echo "    \"exclude\": [],"
      echo "    \"count\": ''${#HS_TEST_FILES[@]}"
      echo "  },"

      echo "  \"nix\": {"
      echo "    \"include\": ["
      ${emitJsonArray "NIX_FILES"}
      echo "    ],"
      echo "    \"exclude\": [],"
      echo "    \"count\": ''${#NIX_FILES[@]}"
      echo "  }"
      echo "}"
    } > "$MANIFEST_FILE"

    ${pkgs.jq}/bin/jq . "$MANIFEST_FILE" > "$MANIFEST_FILE.tmp" \
      && mv "$MANIFEST_FILE.tmp" "$MANIFEST_FILE"

    echo ""
    echo "Manifest generated: $MANIFEST_FILE"
    echo ""
    echo "File counts:"
    echo "  Haskell src:   ''${#HS_FILES[@]}"
    echo "  Haskell tests: ''${#HS_TEST_FILES[@]}"
    echo "  Nix:           ''${#NIX_FILES[@]}"
  '';

in {
  generateScript = generateManifestScript;
  debug          = { config = cfg; excludePattern = excludePatternStr; };
}