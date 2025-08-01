#!/usr/bin/env bash

# CSS build script for Spacecast project
#
# Purpose:
#   Concatenates all CSS files into a single file for the browser
#
# Usage:
#   ./scripts/build/css_builder.sh [output_path]
#
# Arguments:
#   output_path - Optional. Path to output file. Default: priv/static/assets/app.css
#
# Example:
#   ./scripts/build/css_builder.sh 
#   ./scripts/build/css_builder.sh priv/static/assets/custom.css
#
# Options:
#   --help      Show this help message and exit

if [[ "$1" == "--help" ]]; then
  echo "Usage: $0 [output_path]"
  echo "  output_path: Optional. Path to output file. Default: priv/static/assets/app.css"
  echo "  Set ENV=production for minification."
  exit 0
fi

# Exit on any error
set -e

# Get the project root directory (works regardless of where script is called from)
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$PROJECT_ROOT"

# Default output path
OUTPUT_PATH="priv/static/assets/app.css"

# Use custom output path if provided
if [ "$1" != "" ]; then
  OUTPUT_PATH="$1"
fi

# Create output directory if it doesn't exist
OUTPUT_DIR=$(dirname "$OUTPUT_PATH")
mkdir -p "$OUTPUT_DIR"

CSS_DIR="assets/css"
CSS_FILES=()
while IFS= read -r file; do
  CSS_FILES+=("$file")
done < <(find "$CSS_DIR" -type f -name '*.css')

start=$(date +%s)
echo "Building CSS at $(date)..."

# Clean up old build artifact
rm -f "$OUTPUT_PATH"

# Concatenate all non-empty CSS files
for file in "${CSS_FILES[@]}"; do
  if [ ! -s "$file" ]; then
    echo "Warning: $file is missing or empty"
    continue
  fi
  cat "$file" >> "$OUTPUT_PATH"
done

echo "CSS files combined successfully into $OUTPUT_PATH"

# Minify for production if ENV=production and cleancss is available
if [ "$ENV" = "production" ]; then
  if command -v cleancss >/dev/null 2>&1; then
    cleancss -o "$OUTPUT_PATH" "$OUTPUT_PATH"
    echo "Minified CSS for production."
  else
    echo "Warning: cleancss not found. Skipping minification."
  fi
fi

end=$(date +%s)
echo "Build completed in $((end - start)) seconds."
ls -lh "$OUTPUT_PATH" 