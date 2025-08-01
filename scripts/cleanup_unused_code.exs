defmodule CleanupUnusedCode do
  @moduledoc """
  Script to automatically clean up unused code in the codebase.
  """

  def run do
    # Get all .ex and .exs files in the lib directory
    files = Path.wildcard("lib/**/*.{ex,exs}")

    # Process each file
    Enum.each(files, &process_file/1)
  end

  defp process_file(file) do
    content = File.read!(file)
    ast = Code.string_to_quoted!(content)

    # Find unused code
    {functions, _} = find_functions(ast)
    {variables, _} = find_variables(ast)
    {calls, _} = find_calls(ast)

    # Find unused functions and variables
    unused_functions = Enum.filter(functions, fn {name, _line} ->
      not Enum.any?(calls, fn {call_name, _line} -> call_name == name end)
    end)

    unused_variables = Enum.filter(variables, fn {name, _line} ->
      not Enum.any?(calls, fn {call_name, _line} -> call_name == name end)
    end)

    # Clean up the file
    if unused_functions != [] or unused_variables != [] do
      IO.puts("Cleaning up #{file}...")
      cleaned_content = clean_file(content, unused_functions, unused_variables)
      File.write!(file, cleaned_content)
      IO.puts("Done!")
    end
  end

  defp find_functions(ast) do
    Macro.prewalk(ast, [], fn
      {:def, _, [{:when, _, [{name, _, args}, _]}, _]} = node, acc ->
        if args == [] or args == nil do
          {node, [{name, node |> elem(2) |> hd |> elem(2)} | acc]}
        else
          {node, acc}
        end
      {:def, _, [{name, _, args}, _]} = node, acc ->
        if args == [] or args == nil do
          {node, [{name, node |> elem(2) |> hd |> elem(2)} | acc]}
        else
          {node, acc}
        end
      node, acc -> {node, acc}
    end)
  end

  defp find_variables(ast) do
    Macro.prewalk(ast, [], fn
      {:=, _, [{name, _, nil}, _]} = node, acc ->
        {node, [{name, node |> elem(2) |> hd |> elem(2)} | acc]}
      node, acc -> {node, acc}
    end)
  end

  defp find_calls(ast) do
    Macro.prewalk(ast, [], fn
      {name, _, args} = node, acc when is_list(args) ->
        {node, [{name, node |> elem(2)} | acc]}
      node, acc -> {node, acc}
    end)
  end

  defp clean_file(content, unused_functions, unused_variables) do
    lines = String.split(content, "\n")

    # Remove lines containing unused functions
    lines = Enum.reject(lines, fn line ->
      Enum.any?(unused_functions, fn {name, _line} ->
        String.contains?(line, "def #{name}")
      end)
    end)

    # Remove lines containing unused variables
    lines = Enum.reject(lines, fn line ->
      Enum.any?(unused_variables, fn {name, _line} ->
        String.contains?(line, "#{name} =")
      end)
    end)

    # Join lines back together
    Enum.join(lines, "\n")
  end
end

# Run the script
CleanupUnusedCode.run() 