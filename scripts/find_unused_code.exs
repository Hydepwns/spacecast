defmodule FindUnusedCode do
  @moduledoc """
  Script to identify unused code in the codebase.
  """

  def run do
    # Get all .ex and .exs files in the lib directory
    files = Path.wildcard("lib/**/*.{ex,exs}")

    # Parse each file and find unused code
    results = Enum.map(files, &analyze_file/1)

    # Print results
    IO.puts("\nUnused Code Report:")
    IO.puts("==================\n")

    Enum.each(results, fn {file, unused} ->
      if unused != [] do
        IO.puts("File: #{file}")
        IO.puts("-------------------")
        Enum.each(unused, fn {type, name, line} ->
          IO.puts("  #{type}: #{name} (line #{line})")
        end)
        IO.puts("")
      end
    end)
  end

  defp analyze_file(file) do
    content = File.read!(file)
    ast = Code.string_to_quoted!(content)

    # Find all function definitions
    functions = find_functions(ast)
    variables = find_variables(ast)

    # Find all function calls
    calls = find_calls(ast)

    # Find unused functions
    unused_functions = Enum.filter(functions, fn {name, _line} ->
      not Enum.any?(calls, fn {call_name, _line} -> call_name == name end)
    end)

    # Find unused variables
    unused_variables = Enum.filter(variables, fn {name, _line} ->
      not Enum.any?(calls, fn {call_name, _line} -> call_name == name end)
    end)

    # Format results
    unused = Enum.map(unused_functions, fn {name, line} -> {:function, name, line} end) ++
             Enum.map(unused_variables, fn {name, line} -> {:variable, name, line} end)

    {file, unused}
  end

  defp find_functions(ast) do
    {functions, _} = Macro.prewalk(ast, [], fn
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
    functions
  end

  defp find_variables(ast) do
    {variables, _} = Macro.prewalk(ast, [], fn
      {:=, _, [{name, _, nil}, _]} = node, acc ->
        {node, [{name, node |> elem(2) |> hd |> elem(2)} | acc]}
      node, acc -> {node, acc}
    end)
    variables
  end

  defp find_calls(ast) do
    {calls, _} = Macro.prewalk(ast, [], fn
      {name, _, args} = node, acc when is_list(args) ->
        {node, [{name, node |> elem(2)} | acc]}
      node, acc -> {node, acc}
    end)
    calls
  end
end

# Run the script
FindUnusedCode.run() 