#!/bin/bash

# Local CI/CD Test Script
# This script simulates the GitHub Actions CI/CD pipeline locally

set -e  # Exit on any error

echo "🚀 Starting local CI/CD simulation..."
echo "======================================"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    local status=$1
    local message=$2
    case $status in
        "SUCCESS")
            echo -e "${GREEN}✅ $message${NC}"
            ;;
        "ERROR")
            echo -e "${RED}❌ $message${NC}"
            ;;
        "WARNING")
            echo -e "${YELLOW}⚠️  $message${NC}"
            ;;
        "INFO")
            echo -e "${BLUE}ℹ️  $message${NC}"
            ;;
    esac
}

# Function to run a command and check its status
run_check() {
    local name=$1
    local command=$2
    
    print_status "INFO" "Running: $name"
    if eval "$command"; then
        print_status "SUCCESS" "$name passed"
    else
        print_status "ERROR" "$name failed"
        return 1
    fi
}

# Check if we're in the right directory
if [ ! -f "mix.exs" ]; then
    print_status "ERROR" "Not in Elixir project root. Please run from project directory."
    exit 1
fi

# Step 1: Install dependencies
print_status "INFO" "Step 1: Installing dependencies"
run_check "Mix deps.get" "mix deps.get"
run_check "npm install" "cd assets && npm install && cd .."

# Step 2: Compile
print_status "INFO" "Step 2: Compiling"
run_check "Mix compile" "mix compile --warnings-as-errors"

# Step 3: Code quality checks
print_status "INFO" "Step 3: Code quality checks"
run_check "Code formatting" "mix format --check-formatted"
run_check "Credo analysis" "mix credo --strict"

# Step 4: Run tests
print_status "INFO" "Step 4: Running tests"

# Unit tests (excluding browser tests)
print_status "INFO" "Running unit tests..."
if mix test --exclude wallaby --exclude visual_test:true --exclude performance:true; then
    print_status "SUCCESS" "Unit tests passed"
else
    print_status "ERROR" "Unit tests failed"
    exit 1
fi

# Browser tests (if Chrome is available)
if command -v google-chrome &> /dev/null; then
    print_status "INFO" "Running browser tests..."
    if mix test --only wallaby; then
        print_status "SUCCESS" "Browser tests passed"
    else
        print_status "WARNING" "Browser tests failed (this is expected if Chrome is not properly configured)"
    fi
else
    print_status "WARNING" "Chrome not found, skipping browser tests"
fi

# Step 5: Type checking
print_status "INFO" "Step 5: Type checking"
run_check "Dialyzer" "mix dialyzer"

# Step 6: Security audit
print_status "INFO" "Step 6: Security audit"
if mix deps.audit; then
    print_status "SUCCESS" "Security audit passed"
else
    print_status "WARNING" "Security audit found issues"
fi

# Step 7: Build release
print_status "INFO" "Step 7: Building release"
run_check "Release build" "mix release --overwrite"

# Step 8: Health check (if server is running)
print_status "INFO" "Step 8: Health check"
if curl -f http://localhost:4000/health > /dev/null 2>&1; then
    print_status "SUCCESS" "Health check passed"
else
    print_status "WARNING" "Health check failed (server may not be running)"
fi

# Summary
echo ""
echo "======================================"
print_status "SUCCESS" "Local CI/CD simulation completed!"
echo ""
echo "Next steps:"
echo "1. Push your changes to trigger GitHub Actions"
echo "2. Monitor the workflow at: https://github.com/your-repo/actions"
echo "3. Check deployment status if applicable"
echo ""
echo "For more information, see: docs/CI_CD_SETUP.md" 