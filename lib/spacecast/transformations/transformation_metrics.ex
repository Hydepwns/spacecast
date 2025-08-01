defmodule Spacecast.Transformations.TransformationMetrics do
  @moduledoc """
  Metrics collection for resource transformations.

  The TransformationMetrics module collects, stores, and reports metrics about
  transformation execution, including timing information, success rates, and usage patterns.

  ## Features

  - Collection of execution time for individual transformations
  - Tracking of success/failure rates
  - Resource type and operation metrics
  - Metric aggregation for analysis
  - Export and reporting capabilities
  """

  use GenServer

  # Metric record structure
  @type transformation_metric :: %{
          transformation_name: String.t(),
          resource_type: atom() | module(),
          operation: atom(),
          execution_time_ms: float(),
          success: boolean(),
          error: String.t() | nil,
          timestamp: DateTime.t()
        }

  # Initialize the metrics server
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  # Initialize the metrics state
  def init(_opts) do
    {:ok,
     %{
       metrics: [],
       aggregate_data: %{
         execution_count: 0,
         total_execution_time_ms: 0,
         success_count: 0,
         error_count: 0,
         transformation_stats: %{},
         resource_type_stats: %{},
         operation_stats: %{}
       }
     }}
  end

  @doc """
  Records a metric about a transformation execution.

  ## Parameters

  - `transformation_name` - Name of the transformation
  - `resource_type` - Type of resource being transformed
  - `operation` - Operation being performed (:create, :update, :delete)
  - `execution_time_ms` - Time taken to execute the transformation in milliseconds
  - `success` - Whether the transformation was successful
  - `error` - Error message if the transformation failed

  ## Returns

  `:ok`

  ## Example

  ```elixir
  TransformationMetrics.record_metric(
    "format_email", 
    User,
    :update,
    12.5,
    true,
    nil
  )
  ```
  """
  def record_metric(
        transformation_name,
        resource_type,
        operation,
        execution_time_ms,
        success,
        error \\ nil
      ) do
    GenServer.cast(__MODULE__, {
      :record_metric,
      %{
        transformation_name: transformation_name,
        resource_type: resource_type,
        operation: operation,
        execution_time_ms: execution_time_ms,
        success: success,
        error: error,
        timestamp: DateTime.utc_now()
      }
    })
  end

  @doc """
  Measures the execution time of a transformation function.

  ## Parameters

  - `transformation_name` - Name of the transformation
  - `resource_type` - Type of resource being transformed
  - `operation` - Operation being performed (:create, :update, :delete)
  - `function` - Function to execute and measure

  ## Returns

  The result of the function execution

  ## Example

  ```elixir
  result = TransformationMetrics.measure(
    "format_email", 
    User,
    :update,
    fn -> MyTransformer.transform(resource, context) end
  )
  ```
  """
  def measure(transformation_name, resource_type, operation, function) do
    start_time = System.monotonic_time(:millisecond)

    result =
      try do
        function.()
      rescue
        e -> {:error, Exception.message(e)}
      end

    end_time = System.monotonic_time(:millisecond)
    execution_time_ms = end_time - start_time

    # Determine if the transformation was successful
    {success, error} =
      case result do
        {:ok, _} -> {true, nil}
        {:ok, _, _} -> {true, nil}
        {:error, message} when is_binary(message) -> {false, message}
        {:error, _, _} -> {false, "Transformation error"}
        _ -> {false, "Unexpected return value"}
      end

    # Record the metric
    record_metric(
      transformation_name,
      resource_type,
      operation,
      execution_time_ms,
      success,
      error
    )

    # Return the original result
    result
  end

  @doc """
  Gets aggregate metrics for all transformations.

  ## Returns

  Map of aggregate metrics

  ## Example

  ```elixir
  metrics = TransformationMetrics.get_aggregate_metrics()
  average_execution_time = metrics.average_execution_time_ms
  ```
  """
  def get_aggregate_metrics do
    GenServer.call(__MODULE__, :get_aggregate_metrics)
  end

  @doc """
  Gets metrics for a specific transformation.

  ## Parameters

  - `transformation_name` - Name of the transformation

  ## Returns

  Map of metrics for the specified transformation

  ## Example

  ```elixir
  metrics = TransformationMetrics.get_transformation_metrics("format_email")
  success_rate = metrics.success_rate
  ```
  """
  def get_transformation_metrics(transformation_name) do
    GenServer.call(__MODULE__, {:get_transformation_metrics, transformation_name})
  end

  @doc """
  Gets the raw metrics data for detailed analysis.

  ## Parameters

  - `limit` - Maximum number of records to return (default: 100)
  - `filter` - Function to filter metrics (default: all metrics)

  ## Returns

  List of metric records

  ## Example

  ```elixir
  # Get the last 50 failed transformation metrics
  metrics = TransformationMetrics.get_raw_metrics(50, fn m -> m.success == false end)
  ```
  """
  def get_raw_metrics(limit \\ 100, filter \\ fn _ -> true end) do
    GenServer.call(__MODULE__, {:get_raw_metrics, limit, filter})
  end

  @doc """
  Clears all collected metrics.

  ## Returns

  `:ok`

  ## Example

  ```elixir
  TransformationMetrics.clear_metrics()
  ```
  """
  def clear_metrics do
    GenServer.call(__MODULE__, :clear_metrics)
  end

  # Server callbacks

  def handle_cast({:record_metric, metric}, state) do
    # Add the metric to the list
    updated_metrics = [metric | state.metrics]

    # Update the aggregate data
    updated_aggregate = update_aggregate_data(state.aggregate_data, metric)

    {:noreply, %{state | metrics: updated_metrics, aggregate_data: updated_aggregate}}
  end

  def handle_call(:get_aggregate_metrics, _from, state) do
    # Calculate derived metrics
    derived_metrics = calculate_derived_metrics(state.aggregate_data)

    # Combine with the aggregate data
    metrics = Map.merge(state.aggregate_data, derived_metrics)

    {:reply, metrics, state}
  end

  def handle_call({:get_transformation_metrics, transformation_name}, _from, state) do
    # Get the stats for this transformation
    transformation_stats =
      Map.get(
        state.aggregate_data.transformation_stats,
        transformation_name,
        %{
          execution_count: 0,
          total_execution_time_ms: 0,
          success_count: 0,
          error_count: 0
        }
      )

    # Calculate derived metrics for this transformation
    derived_metrics =
      if transformation_stats.execution_count > 0 do
        %{
          average_execution_time_ms:
            transformation_stats.total_execution_time_ms / transformation_stats.execution_count,
          success_rate: transformation_stats.success_count / transformation_stats.execution_count,
          error_rate: transformation_stats.error_count / transformation_stats.execution_count
        }
      else
        %{
          average_execution_time_ms: 0,
          success_rate: 0,
          error_rate: 0
        }
      end

    # Combine with the transformation stats
    metrics = Map.merge(transformation_stats, derived_metrics)

    {:reply, metrics, state}
  end

  def handle_call({:get_raw_metrics, limit, filter}, _from, state) do
    filtered_metrics =
      state.metrics
      |> Enum.filter(filter)
      |> Enum.take(limit)

    {:reply, filtered_metrics, state}
  end

  def handle_call(:clear_metrics, _from, _state) do
    new_state = %{
      metrics: [],
      aggregate_data: %{
        execution_count: 0,
        total_execution_time_ms: 0,
        success_count: 0,
        error_count: 0,
        transformation_stats: %{},
        resource_type_stats: %{},
        operation_stats: %{}
      }
    }

    {:reply, :ok, new_state}
  end

  # Helper functions

  # Update aggregate data with a new metric
  defp update_aggregate_data(aggregate_data, metric) do
    # Update top-level counters
    updated_aggregate = %{
      aggregate_data
      | execution_count: aggregate_data.execution_count + 1,
        total_execution_time_ms: aggregate_data.total_execution_time_ms + metric.execution_time_ms
    }

    # Update success/error counts
    updated_aggregate =
      if metric.success do
        %{updated_aggregate | success_count: updated_aggregate.success_count + 1}
      else
        %{updated_aggregate | error_count: updated_aggregate.error_count + 1}
      end

    # Update transformation-specific stats
    transformation_stats =
      Map.get(
        updated_aggregate.transformation_stats,
        metric.transformation_name,
        %{
          execution_count: 0,
          total_execution_time_ms: 0,
          success_count: 0,
          error_count: 0
        }
      )

    updated_transformation_stats = %{
      transformation_stats
      | execution_count: transformation_stats.execution_count + 1,
        total_execution_time_ms:
          transformation_stats.total_execution_time_ms + metric.execution_time_ms
    }

    updated_transformation_stats =
      if metric.success do
        %{
          updated_transformation_stats
          | success_count: updated_transformation_stats.success_count + 1
        }
      else
        %{
          updated_transformation_stats
          | error_count: updated_transformation_stats.error_count + 1
        }
      end

    updated_aggregate = %{
      updated_aggregate
      | transformation_stats:
          Map.put(
            updated_aggregate.transformation_stats,
            metric.transformation_name,
            updated_transformation_stats
          )
    }

    # Update resource type stats
    resource_type_stats =
      Map.get(
        updated_aggregate.resource_type_stats,
        metric.resource_type,
        %{
          execution_count: 0,
          total_execution_time_ms: 0,
          success_count: 0,
          error_count: 0
        }
      )

    updated_resource_type_stats = %{
      resource_type_stats
      | execution_count: resource_type_stats.execution_count + 1,
        total_execution_time_ms:
          resource_type_stats.total_execution_time_ms + metric.execution_time_ms
    }

    updated_resource_type_stats =
      if metric.success do
        %{
          updated_resource_type_stats
          | success_count: updated_resource_type_stats.success_count + 1
        }
      else
        %{updated_resource_type_stats | error_count: updated_resource_type_stats.error_count + 1}
      end

    updated_aggregate = %{
      updated_aggregate
      | resource_type_stats:
          Map.put(
            updated_aggregate.resource_type_stats,
            metric.resource_type,
            updated_resource_type_stats
          )
    }

    # Update operation stats
    operation_stats =
      Map.get(
        updated_aggregate.operation_stats,
        metric.operation,
        %{
          execution_count: 0,
          total_execution_time_ms: 0,
          success_count: 0,
          error_count: 0
        }
      )

    updated_operation_stats = %{
      operation_stats
      | execution_count: operation_stats.execution_count + 1,
        total_execution_time_ms:
          operation_stats.total_execution_time_ms + metric.execution_time_ms
    }

    updated_operation_stats =
      if metric.success do
        %{updated_operation_stats | success_count: updated_operation_stats.success_count + 1}
      else
        %{updated_operation_stats | error_count: updated_operation_stats.error_count + 1}
      end

    %{
      updated_aggregate
      | operation_stats:
          Map.put(
            updated_aggregate.operation_stats,
            metric.operation,
            updated_operation_stats
          )
    }
  end

  # Calculate derived metrics from aggregate data
  defp calculate_derived_metrics(aggregate_data) do
    if aggregate_data.execution_count > 0 do
      %{
        average_execution_time_ms:
          aggregate_data.total_execution_time_ms / aggregate_data.execution_count,
        success_rate: aggregate_data.success_count / aggregate_data.execution_count,
        error_rate: aggregate_data.error_count / aggregate_data.execution_count
      }
    else
      %{
        average_execution_time_ms: 0,
        success_rate: 0,
        error_rate: 0
      }
    end
  end
end
