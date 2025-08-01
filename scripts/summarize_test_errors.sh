#!/bin/bash
# Summarize real test failures from a test run.
# Usage: ./scripts/summarize_test_errors.sh

# Usage instructions
if [ "$1" == "--help" ] || [ "$1" == "-h" ]; then
  echo "Usage: $0"
  echo "Runs mix test and summarizes warnings and errors in tmp/test_error_summary.txt"
  exit 0
fi

# Directory for storing test output
TMP_DIR="tmp"
# File to store raw test output
INPUT_FILE="$TMP_DIR/test_output.txt"
# File to store the summarized errors
OUTPUT_FILE="$TMP_DIR/test_error_summary.txt"

# Ensure tmp directory exists
mkdir -p "$TMP_DIR"

# Function to count occurrences of a pattern
count_occurrences() {
  local pattern="$1"
  local file="$2"
  grep -E "$pattern" "$file" | wc -l | tr -d ' '
}

# Function to categorize and summarize issues
categorize_issues() {
  local input_file="$1"
  local output_file="$2"
  
  # Clear the output file
  echo "=== Test Results Summary ===" > "$output_file"
  echo "Generated: $(date)" >> "$output_file"
  echo "" >> "$output_file"

  # Count total issues
  local total_warnings
  local total_errors
  total_warnings=$(grep -c "warning:" "$input_file")
  total_errors=$(grep -c "error:" "$input_file")
  
  echo "=== Summary ===" >> "$output_file"
  echo "Total Warnings: $total_warnings" >> "$output_file"
  echo "Total Errors: $total_errors" >> "$output_file"
  echo "" >> "$output_file"

  # Critical Issues (Errors)
  echo "=== Critical Issues (Errors) ===" >> "$output_file"
  echo "These issues must be fixed:" >> "$output_file"
  echo "" >> "$output_file"
  
  # Compilation Errors
  echo "Compilation Errors:" >> "$output_file"
  grep -A 2 "CompileError" "$input_file" | grep -v "^--$" >> "$output_file"
  echo "" >> "$output_file"
  
  # Undefined Functions/Modules
  echo "Undefined Functions/Modules:" >> "$output_file"
  grep -E "is undefined|is undefined or private" "$input_file" >> "$output_file"
  echo "" >> "$output_file"

  # Warnings by Category
  echo "=== Warnings by Category ===" >> "$output_file"
  
  # Unused Variables
  local unused_vars
  unused_vars=$(count_occurrences "variable \".*\" is unused" "$input_file")
  echo "Unused Variables ($unused_vars):" >> "$output_file"
  grep -E "variable \".*\" is unused" "$input_file" | sort -u >> "$output_file"
  echo "" >> "$output_file"
  
  # Unused Functions
  local unused_funcs
  unused_funcs=$(count_occurrences "function .* is unused" "$input_file")
  echo "Unused Functions ($unused_funcs):" >> "$output_file"
  grep -E "function .* is unused" "$input_file" | sort -u >> "$output_file"
  echo "" >> "$output_file"
  
  # Unused Aliases/Imports
  local unused_imports
  unused_imports=$(count_occurrences "unused (alias|import)" "$input_file")
  echo "Unused Aliases/Imports ($unused_imports):" >> "$output_file"
  grep -E "unused (alias|import)" "$input_file" | sort -u >> "$output_file"
  echo "" >> "$output_file"
  
  # Pattern Matching Issues
  local pattern_issues
  pattern_issues=$(count_occurrences "this clause .* cannot match|the underscored variable .* is used after being set" "$input_file")
  echo "Pattern Matching Issues ($pattern_issues):" >> "$output_file"
  grep -E "this clause .* cannot match|the underscored variable .* is used after being set" "$input_file" | sort -u >> "$output_file"
  echo "" >> "$output_file"
  
  # Other Warnings
  local other_warnings
  other_warnings=$(grep -E "warning:" "$input_file" | grep -v -E "variable .* is unused|function .* is unused|unused (alias|import)|this clause .* cannot match|the underscored variable .* is used after being set|is undefined|is undefined or private" | wc -l | tr -d ' ')
  echo "Other Warnings ($other_warnings):" >> "$output_file"
  grep -E "warning:" "$input_file" | grep -v -E "variable .* is unused|function .* is unused|unused (alias|import)|this clause .* cannot match|the underscored variable .* is used after being set|is undefined|is undefined or private" | sort -u >> "$output_file"
  echo "" >> "$output_file"

  # File-specific Issues
  echo "=== Issues by File ===" >> "$output_file"
  grep -E "warning:|error:" "$input_file" | grep -o "lib/.*\.ex:" | sort | uniq -c | sort -nr >> "$output_file"
  echo "" >> "$output_file"
}

# Main execution
echo "Running mix test..."
if mix test > "$INPUT_FILE" 2>&1; then
  echo "All tests passed!" > "$OUTPUT_FILE"
  echo "Full summary in $OUTPUT_FILE"
  exit 0
fi

# If mix test failed
echo "mix test failed. Analyzing output..." > "$OUTPUT_FILE"

# Process the output
categorize_issues "$INPUT_FILE" "$OUTPUT_FILE"

echo "Full summary in $OUTPUT_FILE"
