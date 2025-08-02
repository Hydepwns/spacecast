{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    elixir_1_15
    erlang_26
    postgresql_15
    nodejs_20
    git
    inotify-tools
  ];

  shellHook = ''
    echo "Elixir development environment loaded!"
    echo "Elixir version: $(elixir --version)"
    echo "PostgreSQL version: $(psql --version)"
    echo "Node.js version: $(node --version)"
    
    # Start PostgreSQL if not running
    if ! pg_isready -h localhost -p 5432 > /dev/null 2>&1; then
      echo "Starting PostgreSQL..."
      initdb -D /tmp/postgres-data 2>/dev/null || true
      pg_ctl -D /tmp/postgres-data -l /tmp/postgres.log start
      createdb spacecast_dev 2>/dev/null || true
      createdb spacecast_test 2>/dev/null || true
    fi
    
    echo "PostgreSQL is ready!"
  '';
} 