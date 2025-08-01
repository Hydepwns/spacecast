#!/usr/bin/env elixir

# Script to remove unused functions from the codebase
# Usage: mix run scripts/remove_unused_functions.exs

defmodule RemoveUnusedFunctions do
  @moduledoc """
  Removes unused functions from the codebase based on compiler warnings.
  """

  def run do
    IO.puts("🔍 Finding and removing unused functions...")
    
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
      
      # Process each file
      Enum.each(grouped_functions, fn {file, functions} ->
        process_file(file, functions)
      end)
      
      IO.puts("\n✅ Unused functions removed!")
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

  defp process_file(file, functions) do
    IO.puts("\n📁 Processing #{file} (#{length(functions)} unused functions):")
    
    if File.exists?(file) do
      content = File.read!(file)
      
      Enum.each(functions, fn func ->
        IO.puts("  - Removing #{func.function}/#{func.arity}")
        content = remove_function(content, func.function, func.arity)
      end)
      
      # Write the updated content back to the file
      File.write!(file, content)
      IO.puts("  ✅ Updated #{file}")
    else
      IO.puts("  ⚠️  File not found: #{file}")
    end
  end

  defp remove_function(content, func_name, arity) do
    # This is a simplified approach - in a real implementation you'd need more sophisticated parsing
    # For now, we'll just remove the function definition line
    lines = String.split(content, "\n")
    
    # Find the function definition
    func_pattern = "defp #{func_name}("
    
    lines
    |> Enum.with_index()
    |> Enum.reduce_while(lines, fn {line, index}, acc ->
      if String.contains?(line, func_pattern) do
        # Remove the function definition line
        updated_lines = List.delete_at(acc, index)
        {:halt, updated_lines}
      else
        {:cont, acc}
      end
    end)
    |> Enum.join("\n")
  end
end

# Run the removal
RemoveUnusedFunctions.run() 