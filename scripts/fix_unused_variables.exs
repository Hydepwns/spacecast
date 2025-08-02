#!/usr/bin/env elixir

# Script to fix unused variable warnings by prefixing them with underscores
# Usage: mix run scripts/fix_unused_variables.exs

defmodule FixUnusedVariables do
  @moduledoc """
  Automatically fixes unused variable warnings by prefixing them with underscores.
  """

  def run do
    IO.puts("🔍 Scanning for unused variable warnings...")

    # Run mix test to get warnings
    {output, _exit_code} = System.cmd("mix", ["test"], stderr_to_stdout: true)

    # Extract unused variable warnings
    unused_vars = extract_unused_variables(output)

    if unused_vars == [] do
      IO.puts("✅ No unused variable warnings found!")
      :ok
    else
      IO.puts("📝 Found #{length(unused_vars)} unused variable warnings")

      # Group by file
      grouped_vars = Enum.group_by(unused_vars, & &1.file)

      # Process each file
      Enum.each(grouped_vars, fn {file, vars} ->
        fix_file_variables(file, vars)
      end)

      IO.puts("✅ Fixed #{length(unused_vars)} unused variable warnings")
    end
  end

  defp extract_unused_variables(output) do
    lines = String.split(output, "\n")

    unused_vars =
      lines
      |> Enum.with_index()
      |> Enum.filter(fn {line, _i} ->
        String.contains?(line, "warning: variable ") and String.contains?(line, "is unused")
      end)
      |> Enum.map(fn {line, idx} ->
        # Extract variable name
        [_, var_name] = Regex.run(~r/warning: variable "([^"]+)" is unused/, line)
        # Look ahead for the next line starting with '└─ '
        file_info =
          Enum.find_value(idx..(idx + 5), fn j ->
            if j < length(lines) do
              l = Enum.at(lines, j)

              case Regex.run(~r/└─ ([^:]+):(\d+):/, l) do
                [_, file, line_num] -> {file, String.to_integer(line_num)}
                _ -> nil
              end
            end
          end)

        case file_info do
          {file, line_num} -> %{file: file, line: line_num, variable: var_name}
          _ -> nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    IO.puts("Found #{length(unused_vars)} unused variable warnings with file info")

    Enum.take(unused_vars, 3)
    |> Enum.each(fn warning ->
      IO.puts("  #{warning.file}:#{warning.line} - #{warning.variable}")
    end)

    unused_vars
  end

  defp fix_file_variables(file, vars) do
    if not File.exists?(file) do
      IO.puts("[skip] File does not exist: #{file}")
      vars
    else
      IO.puts("🔧 Fixing #{file} (#{length(vars)} variables)")
      # Read file content
      content = File.read!(file)
      lines = String.split(content, "\n")
      # Sort vars by line number in descending order to avoid line number shifts
      sorted_vars = Enum.sort_by(vars, & &1.line, :desc)
      # Apply fixes
      updated_lines =
        Enum.reduce(sorted_vars, lines, fn var, acc ->
          fix_variable_in_lines(acc, var)
        end)

      # Write back to file
      updated_content = Enum.join(updated_lines, "\n")
      File.write!(file, updated_content)
    end
  end

  defp fix_variable_in_lines(lines, %{line: line_num, variable: var_name}) do
    # Get the line to fix (1-indexed)
    line_index = line_num - 1

    if line_index < length(lines) do
      line = Enum.at(lines, line_index)

      # Find and replace the variable name
      # This is a simple replacement - we'll be more careful about context
      updated_line = fix_variable_in_line(line, var_name)

      List.replace_at(lines, line_index, updated_line)
    else
      lines
    end
  end

  defp fix_variable_in_line(line, var_name) do
    # More sophisticated replacement that considers context
    # We want to avoid replacing variables that are already prefixed with _
    # or are part of string literals, etc.

    # Pattern to match the variable name as a whole word, but not if it's already prefixed with _
    pattern = ~r/(?<!\w|_)#{Regex.escape(var_name)}(?!\w)/

    case Regex.replace(pattern, line, "_#{var_name}") do
      ^line ->
        # If no replacement was made, try a more aggressive approach
        # but only for simple cases
        if String.contains?(line, var_name) and not String.contains?(line, "_#{var_name}") do
          String.replace(line, var_name, "_#{var_name}")
        else
          line
        end

      updated_line ->
        updated_line
    end
  end
end

# Run the script
FixUnusedVariables.run()
