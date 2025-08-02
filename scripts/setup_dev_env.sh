#!/bin/bash

echo "Setting up Spacecast development environment..."

# Check if we're on NixOS
if [ -f /etc/os-release ] && grep -q "NixOS" /etc/os-release; then
    echo "Detected NixOS system"
    
    # Try to use nix-shell if available
    if command -v nix-shell >/dev/null 2>&1; then
        echo "Using nix-shell to set up environment..."
        exec nix-shell --run "$0"
    else
        echo "nix-shell not available, trying alternative approaches..."
        
        # Try to add Nix to PATH
        export PATH="/nix/var/nix/profiles/default/bin:$PATH"
        
        # Try to use system packages
        if [ -d "/run/current-system/sw/bin" ]; then
            export PATH="/run/current-system/sw/bin:$PATH"
        fi
    fi
fi

# Check if Elixir is available
if command -v elixir >/dev/null 2>&1; then
    echo "✅ Elixir found: $(elixir --version | head -1)"
else
    echo "❌ Elixir not found. Please install Elixir first."
    echo "On NixOS, you can add it to your system configuration:"
    echo "environment.systemPackages = with pkgs; [ elixir_1_15 erlang_26 ];"
    exit 1
fi

# Check if Mix is available
if command -v mix >/dev/null 2>&1; then
    echo "✅ Mix found: $(mix --version | head -1)"
else
    echo "❌ Mix not found. Please install Mix (comes with Elixir)."
    exit 1
fi

# Check if PostgreSQL is available
if command -v psql >/dev/null 2>&1; then
    echo "✅ PostgreSQL found: $(psql --version)"
    
    # Try to start PostgreSQL if not running
    if ! pg_isready -h localhost -p 5432 >/dev/null 2>&1; then
        echo "Starting PostgreSQL..."
        
        # Try to start PostgreSQL using systemctl
        if command -v systemctl >/dev/null 2>&1; then
            sudo systemctl start postgresql 2>/dev/null || true
        fi
        
        # Wait a moment for PostgreSQL to start
        sleep 2
        
        # Check if it's running now
        if pg_isready -h localhost -p 5432 >/dev/null 2>&1; then
            echo "✅ PostgreSQL is now running"
        else
            echo "⚠️  PostgreSQL is not running. Tests may fail."
        fi
    else
        echo "✅ PostgreSQL is already running"
    fi
else
    echo "⚠️  PostgreSQL not found. Tests may fail."
fi

# Install dependencies
echo "Installing Elixir dependencies..."
mix deps.get

# Compile the project
echo "Compiling the project..."
mix compile

# Run tests
echo "Running tests..."
mix test

echo "Development environment setup complete!" 