#!/bin/bash

# Script to rename all hydepwns references to spacecast
# Run this from the /tmp/spacecast_migration directory

set -e

echo "Starting systematic renaming of hydepwns to spacecast..."

# Function to rename directories
rename_directories() {
    echo "Renaming directories..."
    
    # Rename lib directories
    if [ -d "lib/hydepwns_liveview" ]; then
        mv lib/hydepwns_liveview lib/spacecast
        echo "Renamed lib/hydepwns_liveview to lib/spacecast"
    fi
    
    if [ -d "lib/hydepwns_liveview_web" ]; then
        mv lib/hydepwns_liveview_web lib/spacecast_web
        echo "Renamed lib/hydepwns_liveview_web to lib/spacecast_web"
    fi
    
    if [ -d "lib/hydepwns" ]; then
        mv lib/hydepwns lib/spacecast_core
        echo "Renamed lib/hydepwns to lib/spacecast_core"
    fi
    
    # Rename test directories
    if [ -d "test/hydepwns_liveview" ]; then
        mv test/hydepwns_liveview test/spacecast
        echo "Renamed test/hydepwns_liveview to test/spacecast"
    fi
    
    if [ -d "test/hydepwns_liveview_web" ]; then
        mv test/hydepwns_liveview_web test/spacecast_web
        echo "Renamed test/hydepwns_liveview_web to test/spacecast_web"
    fi
}

# Function to rename files
rename_files() {
    echo "Renaming files..."
    
    # Rename lib files
    if [ -f "lib/hydepwns_liveview.ex" ]; then
        mv lib/hydepwns_liveview.ex lib/spacecast.ex
        echo "Renamed lib/hydepwns_liveview.ex to lib/spacecast.ex"
    fi
    
    if [ -f "lib/hydepwns_liveview_web.ex" ]; then
        mv lib/hydepwns_liveview_web.ex lib/spacecast_web.ex
        echo "Renamed lib/hydepwns_liveview_web.ex to lib/spacecast_web.ex"
    fi
}

# Function to update file contents
update_file_contents() {
    echo "Updating file contents..."
    
    # Find all .ex, .exs, .heex, .md, .yml, .yaml, .json files and update their contents
    find . -type f \( -name "*.ex" -o -name "*.exs" -o -name "*.heex" -o -name "*.md" -o -name "*.yml" -o -name "*.yaml" -o -name "*.json" -o -name "*.js" -o -name "*.css" -o -name "*.html" \) -exec sed -i 's/HydepwnsLiveview/Spacecast/g' {} \;
    find . -type f \( -name "*.ex" -o -name "*.exs" -o -name "*.heex" -o -name "*.md" -o -name "*.yml" -o -name "*.yaml" -o -name "*.json" -o -name "*.js" -o -name "*.css" -o -name "*.html" \) -exec sed -i 's/HydepwnsLiveviewWeb/SpacecastWeb/g' {} \;
    find . -type f \( -name "*.ex" -o -name "*.exs" -o -name "*.heex" -o -name "*.md" -o -name "*.yml" -o -name "*.yaml" -o -name "*.json" -o -name "*.js" -o -name "*.css" -o -name "*.html" \) -exec sed -i 's/:hydepwns_liveview/:spacecast/g' {} \;
    find . -type f \( -name "*.ex" -o -name "*.exs" -o -name "*.heex" -o -name "*.md" -o -name "*.yml" -o -name "*.yaml" -o -name "*.json" -o -name "*.js" -o -name "*.css" -o -name "*.html" \) -exec sed -i 's/hydepwns_liveview/spacecast/g' {} \;
    
    echo "Updated file contents"
}

# Function to clean up build artifacts
cleanup_build() {
    echo "Cleaning up build artifacts..."
    
    # Remove _build and .elixir_ls directories as they will be regenerated
    rm -rf _build
    rm -rf .elixir_ls
    
    echo "Cleaned up build artifacts"
}

# Main execution
echo "Starting migration..."

# Step 1: Rename directories
rename_directories

# Step 2: Rename files
rename_files

# Step 3: Update file contents
update_file_contents

# Step 4: Clean up build artifacts
cleanup_build

echo "Migration completed successfully!"
echo "Next steps:"
echo "1. Run 'mix deps.get' to get dependencies"
echo "2. Run 'mix compile' to compile the project"
echo "3. Run 'mix test' to verify everything works" 