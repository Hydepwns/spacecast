# Spacecast Development Environment Setup Guide

## Overview

This guide will help you set up the development environment and run tests to identify and fix issues in the Spacecast project.

## Issues Found and Fixed

### ✅ Already Fixed

1. **BaseLive render issue**: Fixed `@inner_content` to use `render_slot(@inner_block)`
2. **EventSystemExampleLive missing assigns**: Added all required assigns to the `do_mount` function
3. **SQLite configuration**: Created `config/test_sqlite.exs` for alternative database testing
4. **Test helper without database**: Created `test/test_helper_no_db.exs` for tests that don't need database

### ⚠️ Issues to Address

1. **Database Connection Issue**: PostgreSQL not running/accessible
2. **Environment Setup**: Elixir/Mix environment not properly configured
3. **Code Quality Issues**: Unused variables, functions, pattern matching issues (need tests to confirm)

## Setup Instructions

### Option 1: Using Nix Shell (Recommended)

1. **Enter the Nix shell environment**:

   ```bash
   nix-shell
   ```

2. **If nix-shell is not available, try**:

   ```bash
   # Add Nix to PATH
   export PATH="/nix/var/nix/profiles/default/bin:$PATH"
   
   # Or use system packages
   export PATH="/run/current-system/sw/bin:$PATH"
   ```

3. **Run the setup script**:

   ```bash
   bash setup_dev_env.sh
   ```

### Option 2: Manual Setup

1. **Install Elixir and Erlang**:

   ```bash
   # On NixOS, add to system configuration:
   environment.systemPackages = with pkgs; [ 
     elixir_1_15 
     erlang_26 
     postgresql_15 
   ];
   ```

2. **Start PostgreSQL**:

   ```bash
   # Start PostgreSQL service
   sudo systemctl start postgresql
   
   # Create databases
   createdb spacecast_dev
   createdb spacecast_test
   ```

3. **Install dependencies**:

   ```bash
   mix deps.get
   ```

4. **Compile the project**:

   ```bash
   mix compile
   ```

### Option 3: Using Docker

1. **Start PostgreSQL containers**:

   ```bash
   cd docker
   docker-compose up -d db test_db
   ```

2. **Run tests**:

   ```bash
   mix test
   ```

## Running Tests

### Run All Tests

```bash
mix test
```

### Run Tests with Error Summary

```bash
./scripts/summarize_test_errors.sh
```

### Run Specific Test Files

```bash
# Run simple test (no database required)
mix test test/simple_test.exs

# Run CSS organization test
mix test test/css_organization_test.exs

# Run specific module tests
mix test test/spacecast_web/components/theme_toggle_test.exs
```

### Run Tests with SQLite (Alternative)

```bash
MIX_ENV=test_sqlite mix test
```

## Code Analysis Tools

### Run Credo (Code Quality)

```bash
mix credo
```

### Run Dialyzer (Static Analysis)

```bash
mix dialyzer
```

### Run Tests with Coverage

```bash
mix test --cover
```

## Expected Issues to Address

Based on the test error summary script, expect to find:

### 1. Critical Issues (Must Fix)

- Compilation errors
- Undefined functions/modules
- Database connection issues

### 2. Warnings (Should Fix)

- Unused variables
- Unused functions
- Unused aliases/imports
- Pattern matching issues

### 3. File-Specific Issues

- Issues by file location
- Component-specific problems

## Troubleshooting

### Database Connection Issues

```bash
# Check if PostgreSQL is running
pg_isready -h localhost -p 5432

# Check database exists
psql -l | grep spacecast

# Create databases if missing
createdb spacecast_dev
createdb spacecast_test
```

### Elixir Environment Issues

```bash
# Check Elixir version
elixir --version

# Check Mix version
mix --version

# Check if dependencies are installed
mix deps
```

### Test Environment Issues

```bash
# Clean and reinstall dependencies
mix deps.clean --all
mix deps.get

# Clean build artifacts
mix clean

# Recompile
mix compile
```

## Next Steps

1. **Set up the environment** using one of the options above
2. **Run the tests** to get a complete picture of issues
3. **Use the error summary script** to categorize problems
4. **Fix issues** in order of priority:
   - Critical errors first
   - Warnings second
   - Code quality issues last
5. **Re-run tests** to verify fixes

## Files Modified

- `lib/spacecast_web/live/base_live.ex`: Fixed render function
- `lib/spacecast_web/live/examples/event_system_example_live.ex`: Added missing assigns
- `config/test_sqlite.exs`: Created SQLite test configuration
- `test/test_helper_no_db.exs`: Created test helper without database
- `shell.nix`: Created development environment setup
- `mix.exs`: Added SQLite3 adapter dependency
- `test/simple_test.exs`: Created simple test for environment verification
- `setup_dev_env.sh`: Created setup script
- `SETUP_GUIDE.md`: This guide

## Support

If you encounter issues:

1. Check the troubleshooting section above
2. Verify your environment setup
3. Check the test output for specific error messages
4. Refer to the Elixir/Phoenix documentation for specific issues
