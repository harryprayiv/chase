{ pkgs, lib, name, backendPath, hsDirs, hsTestDirs ? [] }:

let
  excludePatterns = "dist\\|dist-newstyle\\|\\.direnv\\|result";

  manifest-tui = pkgs.writeShellScriptBin "manifest-tui" ''
    set -euo pipefail
    _G="${pkgs.gum}/bin/gum"

    PROJECT_ROOT="$(pwd)"
    MANIFEST_FILE="$PROJECT_ROOT/script/manifest.json"
    mkdir -p "$PROJECT_ROOT/script"

    header() {
      clear
      "$_G" style \
        --foreground 10 --border-foreground 2 --border double \
        --align center --width 62 --margin "1 2" --padding "1 3" \
        "${lib.toUpper name}" "manifest tools"
      echo ""
    }

    section_header() {
      "$_G" style \
        --foreground 6 --border-foreground 6 --border normal \
        --align center --width 62 --margin "0 2" --padding "0 2" \
        "$1"
      echo ""
    }

    pause() {
      echo ""
      read -r -p "  Press Enter to continue..."
    }

    ok()   { "$_G" style --foreground 2  "  ✓ $*"; }
    err()  { "$_G" style --foreground 1  "  ✗ $*"; }
    info() { "$_G" style --foreground 3  "  · $*"; }
    dim()  { "$_G" style --foreground 8  "  $*"; }

    ensure_manifest() {
      if [ ! -f "$MANIFEST_FILE" ]; then
        info "No manifest found — running generate-manifest..."
        echo ""
        generate-manifest
        echo ""
      fi
    }

    pick_git_ref() {
      local prompt="$1"
      local default="''${2:-HEAD}"

      local REFS
      REFS=$(
        echo "HEAD"
        echo "(working tree)"
        git -C "$PROJECT_ROOT" branch --format='%(refname:short)' 2>/dev/null || true
        git -C "$PROJECT_ROOT" branch -r --format='%(refname:short)' 2>/dev/null | grep -v '/HEAD' || true
        git -C "$PROJECT_ROOT" tag 2>/dev/null || true
      )

      local CHOSEN
      CHOSEN=$(echo "$REFS" | "$_G" filter \
        --prompt="  $prompt > " \
        --placeholder="type to search refs..." \
        --height=20 || echo "$default")

      echo "$CHOSEN"
    }

    run_llm_context() {
      header
      section_header "LLM Context"

      if ! git -C "$PROJECT_ROOT" rev-parse --git-dir > /dev/null 2>&1; then
        err "Not a git repository."
        pause
        return
      fi

      echo ""
      dim "Step 1 of 3 — pick base ref (diff FROM this)"
      echo ""
      local BASE_REF
      BASE_REF=$(pick_git_ref "Base ref")
      ok "Base ref:    $BASE_REF"
      echo ""

      dim "Step 2 of 3 — pick compare ref (diff TO this)"
      echo ""
      local COMPARE_REF
      COMPARE_REF=$(pick_git_ref "Compare ref" "(working tree)")
      ok "Compare ref: $COMPARE_REF"
      echo ""

      dim "Step 3 of 3 — options"
      echo ""
      local FLAGS_RAW
      FLAGS_RAW=$("$_G" choose --no-limit \
        --header="Select flags (space to toggle, enter to confirm):" \
        --height=8 \
        "--diff-only     skip full file contents, show diff only" \
        "--files-only    skip diff, show full file contents only" \
        "--no-compile    do not run cabal" \
        "--no-strip      keep comments in output" || true)

      local CMD_FLAGS=()
      echo "$FLAGS_RAW" | grep -q -- "--diff-only"  && CMD_FLAGS+=(--diff-only)
      echo "$FLAGS_RAW" | grep -q -- "--files-only" && CMD_FLAGS+=(--files-only)
      echo "$FLAGS_RAW" | grep -q -- "--no-compile" && CMD_FLAGS+=(--no-compile)
      echo "$FLAGS_RAW" | grep -q -- "--no-strip"   && CMD_FLAGS+=(--no-strip)

      [ "$BASE_REF"    != "(working tree)" ] && CMD_FLAGS+=(--base-ref="$BASE_REF")
      [ "$COMPARE_REF" != "(working tree)" ] && CMD_FLAGS+=(--compare-ref="$COMPARE_REF")

      echo ""
      "$_G" style --foreground 6 "  llm-context ''${CMD_FLAGS[*]+"''${CMD_FLAGS[*]}"}"
      echo ""
      echo "────────────────────────────────────────────────────────────"
      echo ""

      if llm-context "''${CMD_FLAGS[@]+"''${CMD_FLAGS[@]}"}"; then
        echo ""
        ok "LLM context generated."
      else
        echo ""
        err "llm-context exited with an error."
      fi
      pause
    }

    while true; do
      header

      if [ -f "$MANIFEST_FILE" ]; then
        HT=$(${pkgs.jq}/bin/jq -r '.meta.humanTime // "unknown"' "$MANIFEST_FILE" 2>/dev/null || echo "unknown")
        HS_C=$(${pkgs.jq}/bin/jq -r '.haskell.count // 0' "$MANIFEST_FILE" 2>/dev/null || echo 0)
        NIX_C=$(${pkgs.jq}/bin/jq -r '.nix.count // 0' "$MANIFEST_FILE" 2>/dev/null || echo 0)
        dim "manifest: $HT  ·  HS $HS_C  Nix $NIX_C"
      else
        "$_G" style --foreground 1 --margin "0 4" "No manifest found — run Generate Manifest first"
      fi
      echo ""

      ACTION=$("$_G" choose \
        --cursor "> " \
        --header "  Select action:" \
        --height 10 \
        "Generate Manifest" \
        "Compile Manifest" \
        "Compile Archive" \
        "LLM Context" \
        "Quit") || break

      case "$ACTION" in
        "Generate Manifest")
          header; section_header "Generate Manifest"; echo ""
          generate-manifest && ok "Done." || err "Failed."
          pause ;;
        "Compile Manifest")
          header; section_header "Compile Manifest"; echo ""
          compile-manifest && ok "Done." || err "Failed."
          pause ;;
        "Compile Archive")
          header; section_header "Compile Archive"; echo ""
          compile-archive && ok "Done." || err "Failed."
          pause ;;
        "LLM Context") run_llm_context ;;
        "Quit"|*) break ;;
      esac
    done

    clear
    "$_G" style --foreground 2 --align center --width 62 --margin "1 2" \
      "Goodbye from ${name} manifest tools."
  '';

in {
  inherit manifest-tui;
}