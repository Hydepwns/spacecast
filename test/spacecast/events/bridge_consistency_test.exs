defmodule Spacecast.Events.BridgeConsistencyTest do
  use Spacecast.DataCase

  # Define bridge modules and their core implementations
  @bridge_mappings [
    {Spacecast.Events.EventStore, Spacecast.Events.Core.EventStore},
    {Spacecast.Events.HandlerSupervisor,
     Spacecast.Events.Handlers.HandlerSupervisor},
    {Spacecast.Events.HandlerProcess, Spacecast.Events.Handlers.HandlerProcess},
    {Spacecast.Events.ProjectionSupervisor,
     Spacecast.Events.Projections.ProjectionSupervisor},
    {Spacecast.Events.EventSourcedResource,
     Spacecast.Events.ResourceIntegration.EventSourcedResource}
  ]

  describe "bridge module consistency" do
    test "bridge modules delegate to corresponding core functions" do
      for {bridge_module, core_module} <- @bridge_mappings do
        # Get all public functions from the bridge module
        bridge_functions = get_module_functions(bridge_module)
        # Get all public functions from the core module
        _core_functions = get_module_functions(core_module)

        # For each bridge function (except direct implementations), check if a matching core function exists
        for {name, arity} <- bridge_functions,
            delegate?(bridge_module, name, arity) do
          assert function_exists?(core_module, name, arity) ||
                   aliased_function_exists?(bridge_module, core_module, name, arity),
                 "Bridge function #{inspect(bridge_module)}.#{name}/#{arity} does not have a matching core implementation"
        end
      end
    end
  end

  # Helper functions

  defp get_module_functions(module) do
    module.__info__(:functions)
  rescue
    _ -> []
  end

  defp delegate?(module, name, arity) do
    # Determine if a function is delegated or directly implemented
    # This is a simplified approach and might need refinement
    # based on how delegates are actually implemented in your codebase
    module_ast =
      module.module_info(:compile)[:source]
      |> File.read!()
      |> Code.string_to_quoted!()

    {result, _} =
      Macro.prewalk(module_ast, false, fn
        {:defdelegate, _, [{^name, _, args} | _]}, _acc when length(args) == arity -> {nil, true}
        other, acc -> {other, acc}
      end)

    result
  rescue
    _ -> false
  end

  defp function_exists?(module, name, arity) do
    Kernel.function_exported?(module, name, arity)
  end

  defp aliased_function_exists?(bridge_module, core_module, name, arity) do
    # Handle cases where functions are aliased via "as:"
    module_ast =
      bridge_module.module_info(:compile)[:source]
      |> File.read!()
      |> Code.string_to_quoted!()

    {aliases, _} =
      Macro.prewalk(module_ast, [], fn
        {:defdelegate, _, [{^name, _, args} | rest]}, aliases when length(args) == arity ->
          alias_opts = Keyword.get(rest, :to, [])
          alias_name = Keyword.get(alias_opts, :as, name)
          {nil, [alias_name | aliases]}

        other, acc ->
          {other, acc}
      end)

    Enum.any?(aliases, fn alias_name ->
      function_exists?(core_module, alias_name, arity)
    end)
  rescue
    _ -> false
  end
end
