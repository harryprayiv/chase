{ pkgs, lib ? pkgs.lib, name }:

let
  config   = import ./config.nix { inherit name; };
  dbPort   = toString config.database.port;
  dataDir  = config.dataDir;

  db-start = pkgs.writeShellScriptBin "db-start" ''
    set -euo pipefail

    echo "Starting database service on port ${dbPort}..."

    BACKUP_DIR="${dataDir}/backups"
    mkdir -p "$BACKUP_DIR"
    LATEST_BACKUP="$(find "$BACKUP_DIR" -type f -name '*.sql' -printf '%T@ %p\n' 2>/dev/null \
      | sort -nr | head -n1 | cut -d' ' -f2- || true)"

    if [ -z "$LATEST_BACKUP" ]; then
      echo "No backup found, starting fresh database..."
      pg-start
    else
      echo "Found backup at $LATEST_BACKUP"
      pg-start
      echo "Restoring from backup..."
      pg-restore "$LATEST_BACKUP"
    fi

    echo "Database started on localhost:${dbPort}"
    watch -n 5 pg-stats
  '';

  db-stop = pkgs.writeShellScriptBin "db-stop" ''
    set -euo pipefail
    echo "Creating database backup..."
    pg-backup
    echo "Stopping database..."
    pg-stop || { echo "Failed to stop PostgreSQL"; exit 1; }
    echo "Database stopped."
  '';

  fetch-rosters = pkgs.writeShellScriptBin "fetch-rosters" ''
    set -euo pipefail
    SEASON="''${1:-2025}"
    echo "Fetching rosters for season $SEASON..."
    cabal run fetch-rosters -- "$SEASON"
  '';

  dev = pkgs.writeShellScriptBin "pe-dev" ''
    set -euo pipefail

    echo "Starting chase dev environment..."

    if ! ${pkgs.postgresql}/bin/pg_isready -h "$PGHOST" -p "$PGPORT" -q 2>/dev/null; then
      echo "Starting PostgreSQL..."
      pg-start

      BACKUP_DIR="${dataDir}/backups"
      LATEST_BACKUP="$(find "$BACKUP_DIR" -type f -name '*.sql' -printf '%T@ %p\n' 2>/dev/null \
        | sort -nr | head -n1 | cut -d' ' -f2- || true)"
      if [ -n "$LATEST_BACKUP" ]; then
        echo "Restoring from backup: $LATEST_BACKUP"
        pg-restore "$LATEST_BACKUP"
      fi
    else
      echo "PostgreSQL already running."
    fi

    echo ""
    echo "Database ready at: postgresql://$(whoami)@localhost:$PGPORT/${config.database.name}"
    echo ""
    echo "Available commands:"
    echo "  cabal build                     Build everything"
    echo "  cabal run fetch-rosters -- 2025 Fetch MLB rosters"
    echo "  pg-connect                      psql into ${config.database.name}"
    echo "  pg-stats                        Database statistics"
    echo "  pg-backup                       Backup database"
    echo "  pg-stop                         Stop PostgreSQL"
    echo ""
  '';

  deploy = pkgs.writeShellScriptBin "pe-deploy" ''
    set -euo pipefail

    echo "TMux Commands:"
    echo "  Ctrl-b d    Detach"
    echo "  Ctrl-b o    Switch panes"
    echo ""
    echo "Starting services..."
    echo "  Postgres: localhost:${dbPort}"
    echo ""

    tmux kill-session -t ${name} 2>/dev/null || true
    tmux new-session -d -s ${name} -n "Services" -x 120 -y 42

    tmux split-window -v -b -l 12

    tmux send-keys -t ${name}:Services.0 'watch -n 5 pg-stats' C-m
    tmux send-keys -t ${name}:Services.1 'pe-dev' C-m

    tmux select-pane -t ${name}:Services.1
    tmux attach-session -t ${name}
  '';

  stop = pkgs.writeShellScriptBin "pe-stop" ''
    set -euo pipefail

    echo "Creating database backup..."
    pg-backup || true

    echo "Stopping database..."
    pg-stop || true

    echo "Stopping tmux session..."
    tmux kill-session -t ${name} 2>/dev/null || true

    echo "All services stopped."
  '';

in {
  inherit db-start db-stop fetch-rosters dev deploy stop;
}