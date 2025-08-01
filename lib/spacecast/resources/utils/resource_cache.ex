defmodule Spacecast.Resources.ResourceCache do
  @moduledoc """
  Handles multi-level caching for resources.

  This module provides:
  - Process dictionary caching (fastest)
  - Cache server integration
  - Cache invalidation
  - Bulk cache operations
  """

  require Logger
  alias Spacecast.Resources.CacheServer

  # ETS table name for resource cache
  @resource_cache_table :resource_cache

  # Default cache options
  @default_cache_opts %{
    # 5 minutes
    ttl_seconds: 300,
    # Maximum items in cache
    max_size: 1000,
    # Events that should invalidate cache
    invalidation_events: []
  }

  @spec cached_resource(atom(), any(), (-> any()), keyword()) :: {:ok, any()} | {:error, any()}
  def cached_resource(resource_type, resource_id, fetch_fn, opts \\ []) do
    # Try process dictionary first (fastest)
    case Process.get({:resource_cache, resource_type, resource_id}) do
      {resource, expiry} ->
        if DateTime.compare(expiry, DateTime.utc_now()) == :gt do
          # Process cache is valid
          {:ok, resource}
        else
          # Process cache expired, try cache server
          try_cache_server(resource_type, resource_id, fetch_fn, opts)
        end

      nil ->
        # Not in process dictionary, try cache server
        try_cache_server(resource_type, resource_id, fetch_fn, opts)
    end
  end

  @spec get_cached_resource(module(), any()) :: {:ok, any()} | {:error, any()}
  def get_cached_resource(resource_type, resource_id) do
    # Try process dictionary first (fastest)
    case Process.get({:resource_cache, resource_type, resource_id}) do
      {resource, expiry} -> handle_cached_entry(resource, expiry, resource_type, resource_id)
      nil -> try_cache_server(resource_type, resource_id, fn -> nil end, [])
    end
  end

  @spec invalidate_cache(atom(), any()) :: :ok
  def invalidate_cache(resource_type, resource_id) do
    # Remove from process dictionary
    Process.delete({:resource_cache, resource_type, resource_id})

    # Invalidate in cache server
    CacheServer.invalidate(resource_type, resource_id)

    :ok
  end

  @spec bulk_load_resources(atom(), list(), (list() -> {:ok, map()} | {:error, any()}), keyword()) ::
          {:ok, map()} | {:error, any()}
  def bulk_load_resources(resource_type, resource_ids, loader_fn, opts \\ []) do
    # Filter out IDs that are already in cache
    {cached, uncached} = split_cached_uncached(resource_type, resource_ids)

    # Load uncached resources
    uncached_resources =
      if uncached != [] do
        alias Spacecast.Resources.PerformanceMeasurer
        {result, _metrics} = PerformanceMeasurer.measure(loader_fn, [uncached], name: "bulk_load_#{resource_type}")

        case result do
          {:ok, resources} ->
            # Cache the loaded resources
            cache_resources(resource_type, resources, opts)
            resources

          {:error, reason} ->
            Logger.error("Failed to bulk load #{resource_type} resources: #{inspect(reason)}")
            %{}
        end
      else
        %{}
      end

    # Combine cached and newly loaded resources
    resources = Map.merge(cached, uncached_resources)

    {:ok, resources}
  end

  defp handle_cached_entry(resource, expiry, resource_type, resource_id) do
    if DateTime.compare(expiry, DateTime.utc_now()) == :gt do
      # Process cache is valid
      {:ok, resource}
    else
      # Process cache expired, try cache server
      try_cache_server(resource_type, resource_id, fn -> nil end, [])
    end
  end

  defp try_cache_server(resource_type, resource_id, fetch_fn, _opts) do
    # Try to get from cache server
    case CacheServer.get(resource_type, resource_id, fetch_fn) do
      {:ok, resource} ->
        # Update process cache with default expiry
        expiry = DateTime.add(DateTime.utc_now(), 300, :second)
        Process.put({:resource_cache, resource_type, resource_id}, {resource, expiry})

        {:ok, resource}

      {:error, _} = error ->
        error
    end
  end

  defp split_cached_uncached(resource_type, resource_ids) do
    # Prepare result accumulators
    Enum.reduce(resource_ids, {%{}, []}, fn id, {cached_acc, uncached_acc} ->
      check_cache_status(resource_type, id, cached_acc, uncached_acc)
    end)
  end

  defp check_cache_status(resource_type, id, cached_acc, uncached_acc) do
    # Check if it's in the cache
    cache_key = "#{resource_type}:#{id}"
    case :ets.lookup(@resource_cache_table, cache_key) do
      [{^cache_key, resource, expiry}] ->
        if DateTime.compare(expiry, DateTime.utc_now()) == :gt do
          # It's cached, add to cached accumulator
          {Map.put(cached_acc, id, resource), uncached_acc}
        else
          # It's expired, add to uncached accumulator
          {cached_acc, [id | uncached_acc]}
        end

      [] ->
        # Not cached, add to uncached accumulator
        {cached_acc, [id | uncached_acc]}
    end
  end

  defp cache_resources(resource_type, resources, opts \\ []) do
    # Get cache options
    cache_opts =
      Map.merge(
        @default_cache_opts,
        Map.new(Keyword.take(opts, [:ttl_seconds, :max_size, :invalidation_events]))
      )

    # Cache each resource
    Enum.each(resources, fn {id, resource} ->
      # Cache in process dictionary
      expiry = DateTime.add(DateTime.utc_now(), cache_opts.ttl_seconds, :second)
      Process.put({:resource_cache, resource_type, id}, {resource, expiry})

      # Cache in cache server
      CacheServer.put(resource_type, id, resource, opts)
    end)
  end
end
