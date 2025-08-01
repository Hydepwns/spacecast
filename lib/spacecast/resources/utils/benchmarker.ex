defmodule Spacecast.Resources.Benchmarker do
  @moduledoc """
  Handles performance benchmarking for resource operations.

  This module provides:
  - Strategy benchmarking
  - Performance metrics collection
  - Iteration-based testing
  """

  alias Spacecast.Resources.PerformanceMeasurer

  @spec benchmark((keyword() -> any()), list({atom(), keyword()}), integer()) ::
          {:ok, map()} | {:error, any()}
  def benchmark(operation_fn, strategies, iterations \\ 10) do
    # Run each strategy and collect metrics
    results =
      Enum.map(strategies, fn {strategy_name, strategy_opts} ->
        {strategy_name, benchmark_strategy(strategy_name, strategy_opts, operation_fn, iterations)}
      end)
      |> Enum.into(%{})

    {:ok, results}
  end

  defp benchmark_strategy(strategy_name, strategy_opts, operation_fn, iterations) do
    # Run multiple iterations
    iteration_results =
      Enum.map(1..iterations, fn i ->
        # Apply strategy to operation
        {result, metrics} =
          PerformanceMeasurer.measure(
            fn -> operation_fn.(strategy_opts) end,
            [],
            name: "benchmark_#{strategy_name}_iteration_#{i}"
          )

        {result, metrics}
      end)

    # Calculate aggregate metrics
    successful = Enum.count(iteration_results, fn {result, _} -> match?({:ok, _}, result) end)

    # Extract time metrics
    times = Enum.map(iteration_results, fn {_, metrics} -> metrics.execution_time_ms end)
    avg_time = Enum.sum(times) / length(times)

    # Extract memory metrics
    memory_deltas =
      Enum.map(iteration_results, fn {_, metrics} -> metrics.memory_delta_bytes end)

    avg_memory = Enum.sum(memory_deltas) / length(memory_deltas)

    # Return strategy results
    %{
      success_rate: successful / iterations,
      avg_execution_time_ms: avg_time,
      min_execution_time_ms: Enum.min(times),
      max_execution_time_ms: Enum.max(times),
      avg_memory_delta_bytes: avg_memory,
      iterations: iterations
    }
  end
end
