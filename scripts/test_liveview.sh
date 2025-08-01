#!/usr/bin/env bash

# LiveView Test Helper Script
# This script ensures proper setup for LiveView testing

set -e

echo "🧪 Setting up LiveView test environment..."

# Ensure PostgreSQL is running
if ! pg_ctl status -D .postgres > /dev/null 2>&1; then
    echo "📦 Starting PostgreSQL..."
    pg_ctl start -D .postgres -o "-k /tmp"
    sleep 2
fi

# Set environment variables for testing
export MIX_ENV=test
export PHX_SERVER=true
export SQL_SANDBOX=true
export CHROME_HEADLESS=true
export CHROME_NO_SANDBOX=true
export CHROME_DISABLE_DEV_SHM=true

# Parse command line arguments
MAX_FAILURES=${1:-10}
TEST_PATH=${2:-""}

echo "🔧 Test configuration:"
echo "   - Max failures: $MAX_FAILURES"
echo "   - Test path: ${TEST_PATH:-'all tests'}"
echo ""

# Run the tests
if [ -n "$TEST_PATH" ]; then
    echo "🚀 Running tests: $TEST_PATH"
    nix-shell --run "mix test $TEST_PATH --max-failures=$MAX_FAILURES"
else
    echo "🚀 Running all tests"
    nix-shell --run "mix test --max-failures=$MAX_FAILURES"
fi

echo ""
echo "✅ Test run completed!"
echo "📊 Check tmp/test_error_summary.txt for detailed results" 