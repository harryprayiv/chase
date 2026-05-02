{ name ? "chase", ... }:
{
  inherit name;

  database = {
    name     = name;
    user     = "$(whoami)";
    password = "BOOTSTRAP_FALLBACK_ONLY_USE_SOPS";
    port     = 5433;
    dataDir  = "$HOME/.local/share/${name}/postgres";
    settings = {
      max_connections            = 100;
      shared_buffers             = "128MB";
      dynamic_shared_memory_type = "posix";
      log_destination            = "stderr";
      logging_collector          = true;
      log_directory              = "log";
      log_filename               = "postgresql-%Y-%m-%d_%H%M%S.log";
      log_min_messages           = "info";
      log_connections            = true;
      listen_addresses           = "localhost";
    };
  };

  haskell = {
    cabalFile = "./chase.cabal";
    codeDirs  = [ "./lib" "./app" ];
    tests     = [ "./test" "./integration-test" ];
  };

  license = {
    holder = "Harry Pray IV";
    years  = "2024-2026";
    spdx   = "AGPL-3.0-or-later";
    name   = "GNU AGPLv3 or later";
  };

  dataDir = "$HOME/.local/share/${name}";
  logDir  = "$HOME/.local/share/${name}/logs";
}