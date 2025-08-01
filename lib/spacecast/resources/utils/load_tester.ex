defmodule Spacecast.Resources.LoadTester do
  @moduledoc """
  Load testing utilities for resource systems.

  This module provides functionality for running load tests on resource systems
  to measure performance and identify bottlenecks.
  """

  require Logger

  @type resource_module :: module()
  @type operation_fn :: (resource_module(), any() -> any())
  @type test_id :: String.t()
  @type load_test_result :: %{
    test_id: test_id(),
    resource_type: String.t(),
    total_operations: integer(),
    successful_operations: integer(),
    failed_operations: integer(),
    total_time_ms: integer(),
    operations_per_second: float(),
    avg_operation_time_ms: float(),
    min_operation_time_ms: integer(),
    max_operation_time_ms: integer(),
    concurrency: integer(),
    worker_results: list(map())
  }
  @type stress_test_result :: %{
    test_id: test_id(),
    resource_type: String.t(),
    max_concurrency: integer(),
    concurrency_step: integer(),
    operations_per_step: integer(),
    results: list(load_test_result()),
    breaking_point: integer(),
    total_time_ms: integer()
  }

  @spec run_load_test(resource_module(), operation_fn(), keyword()) ::
          {:ok, load_test_result()} | {:error, any()}
  @doc """
  Runs a load test on the resource system.

  ## Parameters
  * `resource_module` - The resource module
  * `operation_fn` - Function that performs an operation on a resource
  * `opts` - Load test options

  ## Options
  * `:concurrency` - Number of concurrent workers (default: 10)
  * `:operations` - Total number of operations to perform (default: 100)

  ## Returns
  * `{:ok, results}` - Load test results
  * `{:error, reason}` - Failed to run load test
  """
  def run_load_test(resource_module, operation_fn, opts \\ []) do
    # Default options
    concurrency = Keyword.get(opts, :concurrency, 10)
    operations = Keyword.get(opts, :operations, 100)

    resource_type = resource_module.resource_type()
    test_id = Ecto.UUID.generate()

    Logger.info("Starting load test #{test_id} for #{resource_type}")

    # Track metrics
    start_time = System.monotonic_time(:millisecond)

    # Create a task for each concurrent operation
    task_results =
      1..concurrency
      |> Enum.map(fn worker_id ->
        Task.async(fn ->
          run_worker_operations(worker_id, operations, concurrency, test_id, resource_module, operation_fn)
        end)
      end)
      |> Enum.map(&Task.await(&1, 60_000))

    # Calculate final metrics
    end_time = System.monotonic_time(:millisecond)
    total_time = end_time - start_time

    # Flatten all operation results
    all_operations = Enum.flat_map(task_results, fn worker -> worker.results end)

    # Calculate statistics
    successful = Enum.count(all_operations, fn op -> match?({:ok, _}, op.result) end)
    failed = length(all_operations) - successful

    times = Enum.map(all_operations, & &1.time_ms)
    avg_time = Enum.sum(times) / length(times)
    max_time = Enum.max(times)
    min_time = Enum.min(times)

    # Format results
    results = %{
      test_id: test_id,
      resource_type: resource_type,
      total_operations: length(all_operations),
      successful_operations: successful,
      failed_operations: failed,
      total_time_ms: total_time,
      operations_per_second: length(all_operations) / (total_time / 1000),
      avg_operation_time_ms: avg_time,
      min_operation_time_ms: min_time,
      max_operation_time_ms: max_time,
      concurrency: concurrency,
      worker_results: task_results
    }

    Logger.info(
      "Load test #{test_id} completed: #{successful}/#{length(all_operations)} operations successful"
    )

    {:ok, results}
  end

  @spec run_stress_test(resource_module(), operation_fn(), keyword()) ::
          {:ok, stress_test_result()} | {:error, any()}
  @doc """
  Runs a stress test on the resource system.

  This is similar to a load test but gradually increases load to find breaking points.

  ## Parameters
  * `resource_module` - The resource module
  * `operation_fn` - Function that performs an operation on a resource
  * `opts` - Stress test options (default: [])

  ## Options
  * `:max_concurrency` - Maximum concurrency to test (default: 100)
  * `:concurrency_step` - Concurrency increment per step (default: 10)
  * `:operations_per_step` - Operations per concurrency level (default: 50)

  ## Returns
  * `{:ok, results}` - Stress test results
  * `{:error, reason}` - Failed to run stress test
  """
  def run_stress_test(resource_module, operation_fn, opts \\ []) do
    max_concurrency = Keyword.get(opts, :max_concurrency, 100)
    concurrency_step = Keyword.get(opts, :concurrency_step, 10)
    operations_per_step = Keyword.get(opts, :operations_per_step, 50)

    resource_type = resource_module.resource_type()
    test_id = Ecto.UUID.generate()

    Logger.info("Starting stress test #{test_id} for #{resource_type}")

    # Run tests at increasing concurrency levels
    results =
      for concurrency <- 1..max_concurrency//concurrency_step do
        Logger.info("Testing concurrency level: #{concurrency}")

        {:ok, result} = run_load_test(resource_module, operation_fn, [
          concurrency: concurrency,
          operations: operations_per_step
        ])

        # Add concurrency level to result
        Map.put(result, :concurrency_level, concurrency)
      end

    # Analyze results to find breaking point
    breaking_point = find_breaking_point(results)

    stress_test_result = %{
      test_id: test_id,
      resource_type: resource_type,
      max_concurrency: max_concurrency,
      concurrency_step: concurrency_step,
      operations_per_step: operations_per_step,
      results: results,
      breaking_point: breaking_point,
      total_time_ms: Enum.sum(Enum.map(results, & &1.total_time_ms))
    }

    Logger.info("Stress test #{test_id} completed. Breaking point at concurrency: #{breaking_point}")

    {:ok, stress_test_result}
  end

  @spec run_endurance_test(resource_module(), operation_fn(), keyword()) ::
          {:ok, load_test_result()} | {:error, any()}
  @doc """
  Runs an endurance test on the resource system.

  This runs a sustained load test over a longer period to check for memory leaks
  and performance degradation over time.

  ## Parameters
  * `resource_module` - The resource module
  * `operation_fn` - Function that performs an operation on a resource
  * `opts` - Endurance test options

  ## Options
  * `:duration_minutes` - Test duration in minutes (default: 30)
  * `:concurrency` - Number of concurrent workers (default: 10)
  * `:operations_per_batch` - Operations per batch (default: 100)

  ## Returns
  * `{:ok, results}` - Endurance test results
  * `{:error, reason}` - Failed to run endurance test
  """
  def run_endurance_test(resource_module, operation_fn, opts \\ []) do
    duration_minutes = Keyword.get(opts, :duration_minutes, 30)
    concurrency = Keyword.get(opts, :concurrency, 10)
    operations_per_batch = Keyword.get(opts, :operations_per_batch, 100)

    resource_type = resource_module.resource_type()
    test_id = Ecto.UUID.generate()
    duration_ms = duration_minutes * 60 * 1000

    Logger.info("Starting endurance test #{test_id} for #{resource_type} (duration: #{duration_minutes} minutes)")

    start_time = System.monotonic_time(:millisecond)
    end_time = start_time + duration_ms

    # Run batches until time is up
    batch_number = 1

    {final_batch_results, _} =
      Stream.unfold({[], batch_number, start_time}, fn {results, batch, current_time} ->
        if current_time >= end_time do
          nil
        else
          # Run a batch
          {:ok, batch_result} = run_load_test(resource_module, operation_fn, [
            concurrency: concurrency,
            operations: operations_per_batch
          ])

          batch_result = Map.put(batch_result, :batch_number, batch)
          batch_result = Map.put(batch_result, :timestamp, DateTime.utc_now())

          new_results = [batch_result | results]
          new_time = System.monotonic_time(:millisecond)

          {batch_result, {new_results, batch + 1, new_time}}
        end
      end)
      |> Enum.to_list()

    # Analyze endurance results
    total_operations = Enum.sum(Enum.map(final_batch_results, & &1.total_operations))
    total_successful = Enum.sum(Enum.map(final_batch_results, & &1.successful_operations))
    total_failed = total_operations - total_successful

    # Check for performance degradation
    performance_trend = analyze_performance_trend(final_batch_results)

    endurance_result = %{
      test_id: test_id,
      resource_type: resource_type,
      duration_minutes: duration_minutes,
      concurrency: concurrency,
      total_operations: total_operations,
      total_successful_operations: total_successful,
      total_failed_operations: total_failed,
      batch_results: Enum.reverse(final_batch_results),
      performance_trend: performance_trend,
      total_time_ms: System.monotonic_time(:millisecond) - start_time
    }

    Logger.info("Endurance test #{test_id} completed: #{total_successful}/#{total_operations} operations successful")

    {:ok, endurance_result}
  end

  # Private helper functions

  defp run_worker_operations(worker_id, operations, concurrency, test_id, resource_module, operation_fn) do
    # Calculate operations for this worker
    worker_operations = div(operations, concurrency)

    # Run operations
    results =
      Enum.map(1..worker_operations, fn i ->
        op_start_time = System.monotonic_time(:millisecond)

        # Generate a unique ID for this operation
        resource_id = "test-#{test_id}-#{worker_id}-#{i}"

        # Run the operation and capture result
        {result, time_taken} =
          try do
            result = operation_fn.(resource_module, resource_id)
            end_time = System.monotonic_time(:millisecond)
            {result, end_time - op_start_time}
          catch
            kind, error ->
              {{:error, {kind, error}}, System.monotonic_time(:millisecond) - op_start_time}
          end

        # Return operation result with metrics
        %{
          worker_id: worker_id,
          operation: i,
          resource_id: resource_id,
          result: result,
          time_ms: time_taken
        }
      end)

    # Return all results from this worker
    %{
      worker_id: worker_id,
      operations: worker_operations,
      results: results
    }
  end

  defp find_breaking_point(results) do
    # Find the first concurrency level where failure rate exceeds threshold
    threshold = 0.05 # 5% failure rate

    breaking_point =
      Enum.find(results, fn result ->
        failure_rate = result.failed_operations / result.total_operations
        failure_rate > threshold
      end)

    case breaking_point do
      nil -> :no_breaking_point_found
      result -> result.concurrency_level
    end
  end

  defp analyze_performance_trend(batch_results) do
    # Analyze if performance degrades over time
    if length(batch_results) < 2 do
      :insufficient_data
    else
      # Calculate average response time trend
      avg_times = Enum.map(batch_results, & &1.avg_operation_time_ms)

      # Simple linear regression to check if times are increasing
      trend = calculate_trend(avg_times)

      cond do
        trend > 0.1 -> :degrading
        trend < -0.1 -> :improving
        true -> :stable
      end
    end
  end

  defp calculate_trend(values) do
    # Simple linear trend calculation
    n = length(values)

    if n < 2 do
      0.0
    else
      indices = 0..(n-1)
      sum_x = Enum.sum(indices)
      sum_y = Enum.sum(values)
      sum_xy = Enum.sum(Enum.zip(indices, values) |> Enum.map(fn {x, y} -> x * y end))
      sum_x2 = Enum.sum(Enum.map(indices, & &1 * &1))

      slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      slope
    end
  end
end
