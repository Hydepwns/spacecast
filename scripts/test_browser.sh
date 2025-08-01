#!/usr/bin/env bash

# Browser Test Helper Script
# This script sets up a virtual display and runs Wallaby browser tests

set -e

echo "🌐 Setting up browser test environment..."

# Check if we're in nix-shell
if [ -z "$IN_NIX_SHELL" ]; then
    echo "❌ Error: This script must be run inside nix-shell"
    echo "   Run: nix-shell"
    exit 1
fi

# Check if chromedriver is available
if ! command -v chromedriver &> /dev/null; then
    echo "❌ Error: chromedriver not found"
    echo "   Expected path: $CHROMEDRIVER_PATH"
    exit 1
fi

echo "✅ Chromedriver found at: $(which chromedriver)"

# Check if Chrome is available
if ! command -v google-chrome &> /dev/null; then
    echo "❌ Error: google-chrome not found"
    echo "   Expected path: $CHROME_BIN"
    exit 1
fi

echo "✅ Chrome found at: $(which google-chrome)"

# Install xvfb if not available
if ! command -v Xvfb &> /dev/null; then
    echo "📦 Installing xvfb..."
    nix-env -iA nixpkgs.xorg.xvfb
fi

# Start virtual display
echo "🖥️  Starting virtual display..."
Xvfb :99 -screen 0 1024x768x24 > /dev/null 2>&1 &
XVFB_PID=$!

# Wait for display to be ready
sleep 2

# Set display environment variable
export DISPLAY=:99

echo "🧪 Running browser tests..."
echo "   - Display: $DISPLAY"
echo "   - Chrome: $CHROME_BIN"
echo "   - Chromedriver: $CHROMEDRIVER_PATH"

# Run the tests
if [ $# -eq 0 ]; then
    # Run all Wallaby tests
    mix test --only wallaby
else
    # Run specific test file
    mix test --only wallaby "$@"
fi

# Clean up virtual display
echo "🧹 Cleaning up virtual display..."
kill $XVFB_PID 2>/dev/null || true

echo "✅ Browser tests completed!" 