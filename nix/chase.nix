# chase's derivation, written in cabal2nix's form and COMMITTED, so that a
# flake that uses chase (open-slop's llmq-index, through neoblade-config)
# evaluates without building anything first. callCabal2nix runs cabal2nix
# during evaluation, which made every evaluation of the machine that uses
# chase wait on a build (import from derivation).
#
# Keep it in step with chase.cabal: a dependency added there is added here.
# `nix run nixpkgs#cabal2nix -- .` prints the current form to compare.
#
# src is the bare tree; the flake replaces it with the tree plus the
# vendored PureScript CST, which is gitignored.
{ mkDerivation, aeson, array, async, base, bytestring, containers
, deepseq, directory, dlist, filepath, ghc-lib-parser, grace, happy
, hashable, http-client, http-types, lib, mtl, optparse-applicative
, parallel, protolude, scientific, serialise, syb, text, time
, transformers, utf8-string, vector
}:
mkDerivation {
  pname = "chase";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async base bytestring containers deepseq directory dlist
    filepath ghc-lib-parser grace hashable http-client http-types mtl
    parallel protolude scientific serialise syb text time transformers
    utf8-string vector
  ];
  libraryToolDepends = [ happy ];
  executableHaskellDepends = [
    aeson base bytestring containers directory filepath grace
    optparse-applicative text
  ];
  description = "Compress Haskell + PureScript source into LLM context skeletons";
  license = lib.meta.getLicenseFromSpdxId "AGPL-3.0-or-later";
}