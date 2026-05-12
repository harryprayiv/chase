# vendor.nix
{ pkgs, purescript-src }:

pkgs.runCommand "vendor-purescript-cst" {} ''
  set -euo pipefail

  ROOT=$out/Chase/Vendor
  mkdir -p $ROOT/PureScript/CST/Traversals
  mkdir -p $ROOT/PureScript/AST
  mkdir -p $ROOT/Control/Monad/Supply
  mkdir -p $ROOT/Data/Text

  SRC=${purescript-src}/src

  # CST core
  for f in Types Lexer Layout Errors Monad Positions Utils Flatten Print; do
    cp "$SRC/Language/PureScript/CST/$f.hs" "$ROOT/PureScript/CST/$f.hs"
  done
  cp "$SRC/Language/PureScript/CST/Parser.y"           "$ROOT/PureScript/CST/Parser.y"
  cp "$SRC/Language/PureScript/CST/Traversals/Type.hs" "$ROOT/PureScript/CST/Traversals/Type.hs"
  cp "$SRC/Language/PureScript/CST/Traversals.hs"      "$ROOT/PureScript/CST/Traversals.hs"

  # Support modules under Language.PureScript
  cp "$SRC/Language/PureScript/Names.hs"          "$ROOT/PureScript/Names.hs"
  cp "$SRC/Language/PureScript/PSString.hs"       "$ROOT/PureScript/PSString.hs"
  cp "$SRC/Language/PureScript/Roles.hs"          "$ROOT/PureScript/Roles.hs"
  cp "$SRC/Language/PureScript/Comments.hs"       "$ROOT/PureScript/Comments.hs"
  cp "$SRC/Language/PureScript/AST/SourcePos.hs"  "$ROOT/PureScript/AST/SourcePos.hs"

  # Supply monad transformer (used by Names.hs)
  cp "$SRC/Control/Monad/Supply.hs"        "$ROOT/Control/Monad/Supply.hs"
  cp "$SRC/Control/Monad/Supply/Class.hs"  "$ROOT/Control/Monad/Supply/Class.hs"

  # Custom Text utility module (used by Lexer.hs)
  cp "$SRC/Data/Text/PureScript.hs" "$ROOT/Data/Text/PureScript.hs"

  # Rewrite module paths
  chmod -R u+w "$out"
  find "$out" -type f \( -name "*.hs" -o -name "*.y" \) -exec sed -i \
    -e 's|Language\.PureScript\.CST|Chase.Vendor.PureScript.CST|g' \
    -e 's|Language\.PureScript\.Names|Chase.Vendor.PureScript.Names|g' \
    -e 's|Language\.PureScript\.PSString|Chase.Vendor.PureScript.PSString|g' \
    -e 's|Language\.PureScript\.Roles|Chase.Vendor.PureScript.Roles|g' \
    -e 's|Language\.PureScript\.Comments|Chase.Vendor.PureScript.Comments|g' \
    -e 's|Language\.PureScript\.AST|Chase.Vendor.PureScript.AST|g' \
    -e 's|Control\.Monad\.Supply|Chase.Vendor.Control.Monad.Supply|g' \
    -e 's|Data\.Text\.PureScript|Chase.Vendor.Data.Text.PureScript|g' \
    '{}' \;
''