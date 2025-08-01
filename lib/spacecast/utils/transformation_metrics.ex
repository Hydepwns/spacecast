defmodule Spacecast.Utils.TransformationMetrics do
  @moduledoc """
  Provides tools for collecting, storing, and analyzing metrics related to
  transformation execution in the Resource Transformation Pipeline.

  This module helps track:
  - Execution time of transformations
  - Success/failure rates
  - Resource impact metrics (size before/after)
  - Frequency of transformation application
  """

  defmodule MetricsRecord do
    @moduledoc """
    Structured data for storing transformation metrics.
    """

    defstruct [
      :transformation_name,
      :transformation_module,
      :execution_time_ms,
      :result_status,
      :resource_size_before,
      :resource_size_after,
      :timestamp,
      :metadata
    ]

    @type t :: %__MODULE__{
            transformation_name: String.t(),
            transformation_module: module(),
            execution_time_ms: float(),
            result_status: :ok | :error,
            resource_size_before: non_neg_integer(),
            resource_size_after: non_neg_integer(),
            timestamp: DateTime.t(),
            metadata: map()
          }
  end

  @doc """
  Tracks a transformation execution, recording execution time and result metrics.

  ## Example

      metrics = TransformationMetrics.track(MyApp.Transformations.NormalizeEmail, fn ->
        MyApp.Transformations.NormalizeEmail.transform(resource, context)
      end, resource)
  """
  @spec track(module(), (-> {atom(), any()}), map(), keyword()) :: {any(), MetricsRecord.t()}
  def track(transformation_module, execution_fun, resource, opts \\ []) do
    start_time = System.monotonic_time(:millisecond)
    resource_size_before = calculate_resource_size(resource)

    result = execution_fun.()

    end_time = System.monotonic_time(:millisecond)
    execution_time_ms = end_time - start_time

    {result_status, transformed_resource} =
      case result do
        {:ok, resource} -> {:ok, resource}
        {:error, _} -> {:error, resource}
        _ -> {:error, resource}
      end

    resource_size_after = calculate_resource_size(transformed_resource)

    transformation_name = get_transformation_name(transformation_module)

    metrics = %MetricsRecord{
      transformation_name: transformation_name,
      transformation_module: transformation_module,
      execution_time_ms: execution_time_ms,
      result_status: result_status,
      resource_size_before: resource_size_before,
      resource_size_after: resource_size_after,
      timestamp: DateTime.utc_now(),
      metadata: Keyword.get(opts, :metadata, %{})
    }

    store_metrics(metrics, opts)

    {result, metrics}
  end

  @doc """
  Formats metrics for easy visualization in logs or UI.

  ## Example

      metrics = TransformationMetrics.track(...)
      TransformationMetrics.format_metrics(metrics)
  """
  @spec format_metrics(MetricsRecord.t()) :: String.t()
  def format_metrics(%MetricsRecord{} = metrics) do
    size_change = metrics.resource_size_after - metrics.resource_size_before
    size_change_str = if size_change >= 0, do: "+#{size_change}", else: "#{size_change}"

    """
    Transformation: #{metrics.transformation_name}
    Status: #{format_status(metrics.result_status)}
    Execution Time: #{metrics.execution_time_ms}ms
    Resource Size: #{metrics.resource_size_before} → #{metrics.resource_size_after} (#{size_change_str})
    Timestamp: #{Calendar.strftime(metrics.timestamp, "%Y-%m-%d %H:%M:%S")}
    """
  end

  @doc """
  Returns metrics in a format suitable for visualization tools like charts.

  ## Example

      metrics_data = TransformationMetrics.get_metrics_for_visualization(module, opts)
  """
  @spec get_metrics_for_visualization(module(), keyword()) :: [map()]
  def get_metrics_for_visualization(transformation_module, opts \\ []) do
    metrics = get_metrics(transformation_module, opts)

    Enum.map(metrics, fn m ->
      %{
        name: m.transformation_name,
        execution_time: m.execution_time_ms,
        status: m.result_status,
        timestamp: m.timestamp,
        size_change: m.resource_size_after - m.resource_size_before
      }
    end)
  end

  @doc """
  Retrieves stored metrics for a specific transformation or all transformations.
  """
  @spec get_metrics(module() | nil, keyword()) :: [MetricsRecord.t()]
  def get_metrics(transformation_module \\ nil, _opts \\ []) do
    metrics = get_stored_metrics()

    if transformation_module do
      Enum.filter(metrics, &(&1.transformation_module == transformation_module))
    else
      metrics
    end
  end

  @doc """
  Generates a report of transformation performance across multiple executions.

  ## Example

      TransformationMetrics.generate_performance_report()
  """
  @spec generate_performance_report(keyword()) :: map()
  def generate_performance_report(opts \\ []) do
    metrics = get_metrics(Keyword.get(opts, :transformation_module), opts)

    metrics_by_module = Enum.group_by(metrics, & &1.transformation_module)

    Enum.map(metrics_by_module, fn {module, module_metrics} ->
      avg_time =
        Enum.reduce(module_metrics, 0, fn m, acc -> acc + m.execution_time_ms end) /
          length(module_metrics)

      success_count = Enum.count(module_metrics, &(&1.result_status == :ok))
      error_count = length(module_metrics) - success_count
      success_rate = success_count / length(module_metrics) * 100

      avg_size_change =
        Enum.reduce(module_metrics, 0, fn m, acc ->
          acc + (m.resource_size_after - m.resource_size_before)
        end) / length(module_metrics)

      {module,
       %{
         transformation_name:
           case module_metrics do
             [%{transformation_name: name} | _] ->
               name

             [first | _] ->
               if is_map(first) and Map.has_key?(first, :transformation_name) do
                 first.transformation_name
               else
                 "unknown"
               end

             _ ->
               "unknown"
           end,
         total_executions: length(module_metrics),
         avg_execution_time_ms: avg_time,
         success_rate: success_rate,
         success_count: success_count,
         error_count: error_count,
         avg_size_change: avg_size_change
       }}
    end)
    |> Enum.into(%{})
  end

  # Private functions

  defp calculate_resource_size(resource) do
    # Calculate an approximation of the resource size
    # This is a simple implementation that could be enhanced
    :erlang.term_to_binary(resource) |> byte_size()
  end

  @doc false
  defp get_transformation_name(transformation_module) do
    try do
      if function_exported?(transformation_module, :info, 0) do
        info = apply(transformation_module, :info, [])
        info[:name] || transformation_module |> to_string() |> String.split(".") |> List.last()
      else
        transformation_module |> to_string() |> String.split(".") |> List.last()
      end
    rescue
      _ -> transformation_module |> to_string() |> String.split(".") |> List.last()
    end
  end

  defp format_status(:ok), do: "✅ Success"
  defp format_status(:error), do: "❌ Error"

  # Store metrics in ETS or another storage mechanism
  # This is a simple implementation using process dictionary 
  # In a production environment, consider using ETS, persistent storage,
  # or telemetry for better performance and durability
  defp store_metrics(metrics, opts) do
    metrics_key = :transformation_metrics
    current_metrics = Process.get(metrics_key, [])
    max_metrics = Keyword.get(opts, :max_stored_metrics, 1000)

    updated_metrics =
      [metrics | current_metrics]
      |> Enum.take(max_metrics)

    Process.put(metrics_key, updated_metrics)

    # You could add telemetry events here
    # :telemetry.execute([:hydepwns, :transformation, :executed], %{
    #   execution_time: metrics.execution_time_ms
    # }, %{transformation: metrics.transformation_module})

    metrics
  end

  defp get_stored_metrics do
    Process.get(:transformation_metrics, [])
  end
end
