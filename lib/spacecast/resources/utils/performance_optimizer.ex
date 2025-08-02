defmodule Spacecast.Resources.PerformanceOptimizer do
  @moduledoc """
  Main performance optimization orchestrator.

  This module coordinates the various performance optimization components:
  - PerformanceMeasurer: Telemetry and measurement
  - ResourceCache: Multi-level caching
  - LoadingStrategy: Client-aware loading strategies
  - PerformanceAnalyzer: Analysis and suggestions
  - PerformanceMonitor: Monitoring and alerting
  - Benchmarker: Benchmarking functionality
  """

  alias Spacecast.Resources.{
    PerformanceMeasurer,
    ResourceCache,
    LoadingStrategy,
    PerformanceAnalyzer,
    PerformanceMonitor,
    Benchmarker
  }

  alias Spacecast.Resources.CacheServer

  @spec init() :: :ok
  def init do
    # Start the cache server
    {:ok, _pid} = CacheServer.start_link()

    # Register telemetry handlers
    PerformanceMeasurer.register_telemetry_handlers()

    :ok
  end

  # Performance Measurement Delegations
  @spec measure((... -> any()), list(), keyword()) :: {any(), map()}
  def measure(operation, args, opts \\ []) when is_function(operation, length(args)) do
    PerformanceMeasurer.measure(operation, args, opts)
  end

  # Caching Delegations
  @spec cached_resource(atom(), any(), (-> any()), keyword()) :: {:ok, any()} | {:error, any()}
  def cached_resource(resource_type, resource_id, fetch_fn, opts \\ []) do
    ResourceCache.cached_resource(resource_type, resource_id, fetch_fn, opts)
  end

  @spec get_cached_resource(module(), any()) :: {:ok, any()} | {:error, any()}
  def get_cached_resource(resource_type, resource_id) do
    ResourceCache.get_cached_resource(resource_type, resource_id)
  end

  @spec invalidate_cache(atom(), any()) :: :ok
  def invalidate_cache(resource_type, resource_id) do
    ResourceCache.invalidate_cache(resource_type, resource_id)
  end

  @spec bulk_load_resources(atom(), list(), (list() -> {:ok, map()} | {:error, any()}), keyword()) ::
          {:ok, map()} | {:error, any()}
  def bulk_load_resources(resource_type, resource_ids, loader_fn, opts \\ []) do
    ResourceCache.bulk_load_resources(resource_type, resource_ids, loader_fn, opts)
  end

  # Loading Strategy Delegations
  @spec optimized_loading_strategy(map(), atom(), keyword()) :: map()
  def optimized_loading_strategy(client_info, resource_type, opts \\ []) do
    LoadingStrategy.optimized_loading_strategy(client_info, resource_type, opts)
  end

  # Benchmarking Delegations
  @spec benchmark((keyword() -> any()), list({atom(), keyword()}), integer()) ::
          {:ok, map()} | {:error, any()}
  def benchmark(operation_fn, strategies, iterations \\ 10) do
    Benchmarker.benchmark(operation_fn, strategies, iterations)
  end

  # Performance Analysis Delegations
  @spec analyze_performance_patterns(atom(), integer()) :: {:ok, list(map())} | {:error, any()}
  def analyze_performance_patterns(resource_type, period \\ 3600) do
    PerformanceAnalyzer.analyze_performance_patterns(resource_type, period)
  end

  # Performance Monitoring Delegations
  @spec setup_performance_alerts(atom(), map()) :: :ok | {:error, any()}
  def setup_performance_alerts(resource_type, thresholds \\ %{}) do
    PerformanceMonitor.setup_performance_alerts(resource_type, thresholds)
  end

  @spec alert_performance_issue(atom(), integer(), integer(), map()) :: :ok | {:error, any()}
  def alert_performance_issue(operation, duration, threshold, opts \\ %{}) do
    PerformanceMonitor.alert_performance_issue(operation, duration, threshold, opts)
  end

  @spec send_performance_alert(atom(), integer(), integer(), map()) :: :ok | {:error, any()}
  def send_performance_alert(operation, duration, threshold, context) do
    PerformanceMonitor.send_performance_alert(operation, duration, threshold, context)
  end
end
