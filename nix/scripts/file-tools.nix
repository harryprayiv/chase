{ pkgs, name, lib, backendPath, hsDirs, hsTestDirs ? [], hsConfig }:

let
  manifestModule = import ./manifest.nix {
    inherit pkgs lib;
    config = {
      inherit backendPath hsDirs hsTestDirs;
      hsConfig = { cabalFile = hsConfig.cabalFile or null; };
    };
  };

  devScriptsModule = import ./devScripts.nix {
    inherit pkgs name lib backendPath hsDirs hsTestDirs hsConfig;
  };

  tuiModule = import ./manifest-tui.nix {
    inherit pkgs lib name backendPath hsDirs hsTestDirs;
  };

in {
  inherit (devScriptsModule) compile-manifest compile-archive llm-context;
  inherit (tuiModule)        manifest-tui;
  generate-manifest = manifestModule.generateScript;
}