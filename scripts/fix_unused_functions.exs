#!/usr/bin/env elixir

# Script to analyze and fix unused function warnings
# Usage: mix run scripts/fix_unused_functions.exs

defmodule FixUnusedFunctions do
  @moduledoc """
  Analyzes unused function warnings and provides guidance on how to fix them.
  """

  def run do
    IO.puts("🔍 Analyzing unused function warnings...")
    
    # Run mix test to get warnings
    {output, _exit_code} = System.cmd("mix", ["test"], stderr_to_stdout: true)
    
    # Extract unused function warnings
    unused_functions = extract_unused_functions(output)
    
    if unused_functions == [] do
      IO.puts("✅ No unused function warnings found!")
      :ok
    else
      IO.puts("📝 Found #{length(unused_functions)} unused function warnings")
      
      # Group by file
      grouped_functions = Enum.group_by(unused_functions, & &1.file)
      
      # Analyze each file
      Enum.each(grouped_functions, fn {file, functions} ->
        analyze_file_functions(file, functions)
      end)
      
      IO.puts("\n💡 Recommendations:")
      IO.puts("1. Review each function to determine if it's actually needed")
      IO.puts("2. If needed but not used, add @doc false to suppress the warning")
      IO.puts("3. If truly unused, consider removing the function")
      IO.puts("4. For test helper functions, ensure they're properly imported")
    end
  end

  defp extract_unused_functions(output) do
    output
    |> String.split("\n")
    |> Enum.filter(&String.contains?(&1, "warning: function "))
    |> Enum.filter(&String.contains?(&1, "is unused"))
    |> Enum.map(&parse_function_warning/1)
    |> Enum.reject(&is_nil/1)
  end

  defp parse_function_warning(line) do
    # Extract function name and file info
    case Regex.run(~r/warning: function ([^\/]+)\/(\d+) is unused/, line) do
      [_, func_name, arity] ->
        # Look for file info in the output
        file_info = extract_file_info(line)
        %{
          function: func_name,
          arity: String.to_integer(arity),
          file: file_info
        }
      _ ->
        nil
    end
  end

  defp extract_file_info(line) do
    # The file info is usually on a separate line, so we'll need to look at the context
    # For now, return a placeholder
    "unknown_file.ex"
  end

  defp analyze_file_functions(file, functions) do
    IO.puts("\n📁 #{file} (#{length(functions)} unused functions):")
    
    Enum.each(functions, fn func ->
      IO.puts("  - #{func.function}/#{func.arity}")
      
      # Check if function exists in the file
      if File.exists?(file) do
        content = File.read!(file)
        if String.contains?(content, "def #{func.function}") do
          IO.puts("    ✓ Function exists in file")
        else
          IO.puts("    ⚠️  Function not found in file (may be in different module)")
        end
      end
    end)
  end
end

# Run the analysis
FixUnusedFunctions.run() 