defmodule Spacecast.Resources.CacheServer do
  @moduledoc """
  GenServer for managing resource caching with advanced strategies.

  This module provides:
  - LRU (Least Recently Used) cache with size limits
  - Cache warming for frequently accessed resources
  - Adaptive TTL based on access patterns
  - Cache statistics and monitoring
  - Automatic cache cleanup
  """

  use GenServer
  require Logger

  # Client API

  @spec start_link(keyword()) :: {:ok, pid()} | {:error, any()}
  @doc """
  Starts the cache server.

  ## Options
  * `:name` - The name to register the server under
  * `:max_size` - Maximum number of items in cache (default: 1000)
  * `:default_ttl` - Default time-to-live in seconds (default: 300)
  * `:cleanup_interval` - Interval between cache cleanups in seconds (default: 300)
  * `:warmup_interval` - Interval between cache warmups in seconds (default: 3600)

  ## Returns
  * `{:ok, pid}` - Server started successfully
  * `{:error, reason}` - Failed to start server
  """
  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @spec get(atom(), any(), (-> any())) :: {:ok, any()} | {:error, any()}
  @doc """
  Gets a resource from cache or loads it if not cached.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The resource ID
  * `fetch_fn` - Function to fetch the resource if not in cache

  ## Returns
  * `{:ok, resource}` - The resource
  * `{:error, reason}` - Failed to get resource
  """
  def get(resource_type, resource_id, fetch_fn) do
    GenServer.call(__MODULE__, {:get, resource_type, resource_id, fetch_fn})
  end

  @spec put(atom(), any(), any(), keyword()) :: :ok | {:error, any()}
  @doc """
  Puts a resource in the cache.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The resource ID
  * `resource` - The resource to cache
  * `opts` - Cache options

  ## Returns
  * `:ok` - Resource cached successfully
  * `{:error, reason}` - Failed to cache resource
  """
  def put(resource_type, resource_id, resource, opts \\ []) do
    GenServer.call(__MODULE__, {:put, resource_type, resource_id, resource, opts})
  end

  @spec invalidate(atom(), any()) :: :ok | {:error, any()}
  @doc """
  Invalidates cache entries for a resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The resource ID (or :all for all resources)

  ## Returns
  * `:ok` - Cache invalidated successfully
  * `{:error, reason}` - Failed to invalidate cache
  """
  def invalidate(resource_type, resource_id \\ :all) do
    GenServer.call(__MODULE__, {:invalidate, resource_type, resource_id})
  end

  @spec stats() :: {:ok, map()} | {:error, any()}
  @doc """
  Gets cache statistics.

  ## Returns
  * `{:ok, stats}` - Cache statistics
  * `{:error, reason}` - Failed to get statistics
  """
  def stats do
    GenServer.call(__MODULE__, :stats)
  end

  @spec clear() :: :ok
  @doc """
  Clears all cache entries.

  ## Returns
  * `:ok` - Cache cleared successfully
  """
  def clear do
    GenServer.call(__MODULE__, :clear)
  end

  @spec warm_up(atom(), any()) :: :ok | {:error, any()}
  @doc """
  Manually warm up a specific resource.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The resource ID

  ## Returns
  * `:ok` - Resource warmed up successfully
  * `{:error, reason}` - Failed to warm up resource
  """
  def warm_up(resource_type, resource_id) do
    GenServer.call(__MODULE__, {:warm_up, resource_type, resource_id})
  end

  # Server Callbacks

  @impl true
  def init(opts) do
    # Get options
    max_size = Keyword.get(opts, :max_size, 1000)
    default_ttl = Keyword.get(opts, :default_ttl, 300)
    cleanup_interval = Keyword.get(opts, :cleanup_interval, 300)
    warmup_interval = Keyword.get(opts, :warmup_interval, 3600)

    # Create ETS tables
    cache_table = :ets.new(:resource_cache, [:set, :protected])
    stats_table = :ets.new(:cache_stats, [:set, :protected])
    access_table = :ets.new(:resource_access, [:set, :protected])

    # Initialize stats
    :ets.insert(stats_table, {:hits, 0})
    :ets.insert(stats_table, {:misses, 0})
    :ets.insert(stats_table, {:evictions, 0})
    :ets.insert(stats_table, {:warmups, 0})

    # Initialize state
    state = %{
      cache_table: cache_table,
      stats_table: stats_table,
      access_table: access_table,
      max_size: max_size,
      default_ttl: default_ttl,
      cleanup_interval: cleanup_interval,
      warmup_interval: warmup_interval,
      cleanup_timer: nil,
      warmup_timer: nil
    }

    # Schedule cleanup and warmup
    state = schedule_cleanup(state)
    state = schedule_warmup(state)

    Logger.info("Cache server started with max_size: #{max_size}, default_ttl: #{default_ttl}s")

    {:ok, state}
  end

  @impl true
  def handle_call({:get, resource_type, resource_id, fetch_fn}, _from, state) do
    cache_key = {resource_type, resource_id}
    handle_get_request(state, cache_key, resource_type, resource_id, fetch_fn)
  end

  @impl true
  def handle_call({:put, resource_type, resource_id, resource, opts}, _from, state) do
    cache_key = {resource_type, resource_id}
    ttl = Keyword.get(opts, :ttl, state.default_ttl)
    expiry = DateTime.add(DateTime.utc_now(), ttl, :second)

    # Check if we need to evict entries
    current_size = :ets.info(state.cache_table, :size)

    if current_size >= state.max_size do
      evict_entries(state)
    end

    # Store in cache
    :ets.insert(state.cache_table, {cache_key, resource, expiry})
    update_access_stats(state, resource_type, resource_id)

    Logger.debug("Cached #{resource_type}:#{resource_id} with TTL #{ttl}s")
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:invalidate, resource_type, resource_id}, _from, state) do
    handle_invalidate_request(state, resource_type, resource_id)
  end

  @impl true
  def handle_call(:stats, _from, state) do
    hits = get_stat(state, :hits)
    misses = get_stat(state, :misses)
    evictions = get_stat(state, :evictions)
    warmups = get_stat(state, :warmups)
    size = :ets.info(state.cache_table, :size)

    stats = %{
      hits: hits,
      misses: misses,
      evictions: evictions,
      warmups: warmups,
      size: size,
      max_size: state.max_size,
      hit_rate: calculate_hit_rate(hits, misses),
      memory_usage: :ets.info(state.cache_table, :memory) * :erlang.system_info(:wordsize)
    }

    {:reply, {:ok, stats}, state}
  end

  @impl true
  def handle_call(:clear, _from, state) do
    :ets.delete_all_objects(state.cache_table)
    :ets.delete_all_objects(state.access_table)

    # Reset stats
    :ets.insert(state.stats_table, {:hits, 0})
    :ets.insert(state.stats_table, {:misses, 0})
    :ets.insert(state.stats_table, {:evictions, 0})
    :ets.insert(state.stats_table, {:warmups, 0})

    Logger.info("Cache cleared")
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:warm_up, resource_type, resource_id}, _from, state) do
    handle_warmup_request(state, resource_type, resource_id)
  end

  @impl true
  def handle_info(:cleanup, state) do
    cleanup_expired_entries(state)
    state = schedule_cleanup(state)
    {:noreply, state}
  end

  @impl true
  def handle_info(:warmup, state) do
    warmup_frequent_resources(state)
    state = schedule_warmup(state)
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    # Cancel timers
    if state.cleanup_timer, do: Process.cancel_timer(state.cleanup_timer)
    if state.warmup_timer, do: Process.cancel_timer(state.warmup_timer)

    # Clean up ETS tables
    :ets.delete(state.cache_table)
    :ets.delete(state.stats_table)
    :ets.delete(state.access_table)

    Logger.info("Cache server terminated")
    :ok
  end

  # Private functions

  defp handle_get_request(state, cache_key, resource_type, resource_id, fetch_fn) do
    case :ets.lookup(state.cache_table, cache_key) do
      [{^cache_key, resource, expiry}] ->
        if DateTime.compare(expiry, DateTime.utc_now()) == :gt do
          # Cache hit
          update_access_stats(state, resource_type, resource_id)
          update_stats(state, :hits)
          Logger.debug("Cache hit for #{resource_type}:#{resource_id}")
          {:reply, {:ok, resource}, state}
        else
          # Cache expired
          Logger.debug("Cache expired for #{resource_type}:#{resource_id}")
          handle_cache_miss(state, resource_type, resource_id, fetch_fn)
        end

      [] ->
        # Cache miss
        Logger.debug("Cache miss for #{resource_type}:#{resource_id}")
        handle_cache_miss(state, resource_type, resource_id, fetch_fn)
    end
  end

  defp handle_invalidate_request(state, resource_type, resource_id) do
    case resource_id do
      :all ->
        # Clear all entries for this resource type
        pattern = {{resource_type, :_}, :_, :_}
        deleted_count = :ets.select_count(state.cache_table, [{pattern, [], [true]}])
        :ets.match_delete(state.cache_table, pattern)

        # Also clear access stats for this resource type
        :ets.match_delete(state.access_table, {{resource_type, :_}, :_})
        :ets.match_delete(state.access_table, {{resource_type, :_, :last_access}, :_})

        Logger.info("Invalidated #{deleted_count} entries for resource type #{resource_type}")
        {:reply, :ok, state}

      specific_id ->
        # Clear specific resource
        cache_key = {resource_type, specific_id}
        :ets.delete(state.cache_table, cache_key)
        :ets.delete(state.access_table, cache_key)
        :ets.delete(state.access_table, {resource_type, specific_id, :last_access})

        Logger.debug("Invalidated #{resource_type}:#{specific_id}")
        {:reply, :ok, state}
    end
  end

  defp handle_warmup_request(state, resource_type, resource_id) do
    case get_warmup_function(resource_type) do
      {:ok, warmup_fn} ->
        handle_warmup_execution(state, resource_type, resource_id, warmup_fn)

      :error ->
        Logger.warning("No warmup function available for resource type #{resource_type}")
        {:reply, {:error, :no_warmup_function}, state}
    end
  end

  defp handle_warmup_execution(state, resource_type, resource_id, warmup_fn) do
    case warmup_fn.(resource_id) do
      {:ok, resource} ->
        # Cache the warmed up resource
        ttl = calculate_adaptive_ttl(state, resource_type, resource_id)
        expiry = DateTime.add(DateTime.utc_now(), ttl, :second)
        cache_key = {resource_type, resource_id}
        :ets.insert(state.cache_table, {cache_key, resource, expiry})

        update_access_stats(state, resource_type, resource_id)
        update_stats(state, :warmups)

        Logger.debug("Warmed up #{resource_type}:#{resource_id}")
        {:reply, :ok, state}

      {:error, reason} ->
        Logger.warning("Failed to warm up #{resource_type}:#{resource_id}: #{inspect(reason)}")
        {:reply, {:error, reason}, state}
    end
  end

  defp handle_cache_miss(state, resource_type, resource_id, fetch_fn) do
    # Try to fetch the resource
    case fetch_fn.() do
      {:ok, resource} ->
        # Cache the resource
        ttl = calculate_adaptive_ttl(state, resource_type, resource_id)
        expiry = DateTime.add(DateTime.utc_now(), ttl, :second)
        cache_key = {resource_type, resource_id}
        :ets.insert(state.cache_table, {cache_key, resource, expiry})

        update_access_stats(state, resource_type, resource_id)
        update_stats(state, :misses)

        Logger.debug("Fetched and cached #{resource_type}:#{resource_id}")
        {:reply, {:ok, resource}, state}

      {:error, reason} ->
        update_stats(state, :misses)
        Logger.warning("Failed to fetch #{resource_type}:#{resource_id}: #{inspect(reason)}")
        {:reply, {:error, reason}, state}
    end
  end

  defp update_access_stats(state, resource_type, resource_id) do
    # Update access count
    access_key = {resource_type, resource_id}

    :ets.update_counter(
      state.access_table,
      access_key,
      {2, 1},
      {access_key, 0}
    )

    # Update last access time
    last_access_key = {resource_type, resource_id, :last_access}
    :ets.insert(state.access_table, {last_access_key, DateTime.utc_now()})
  end

  defp update_stats(state, stat) when is_atom(stat) do
    :ets.update_counter(state.stats_table, stat, {2, 1}, {stat, 0})
  end

  defp get_stat(state, stat) do
    case :ets.lookup(state.stats_table, stat) do
      [{^stat, value}] -> value
      [] -> 0
    end
  end

  defp calculate_hit_rate(hits, misses) do
    total = hits + misses
    if total > 0, do: hits / total, else: 0.0
  end

  defp calculate_adaptive_ttl(state, resource_type, resource_id) do
    # Get access pattern
    access_key = {resource_type, resource_id}

    access_count =
      case :ets.lookup(state.access_table, access_key) do
        [{^access_key, count}] -> count
        [] -> 0
      end

    # Adjust TTL based on access pattern
    base_ttl = state.default_ttl

    cond do
      # Very frequently accessed - cache longer
      access_count > 1000 -> trunc(base_ttl * 3)
      # Frequently accessed
      access_count > 100 -> trunc(base_ttl * 2)
      # Moderately accessed
      access_count > 10 -> trunc(base_ttl * 1.5)
      # Normal access
      access_count > 1 -> base_ttl
      # First access - shorter TTL
      true -> trunc(base_ttl * 0.5)
    end
  end

  defp evict_entries(state) do
    # Get all cache entries with their access times
    cache_entries = :ets.tab2list(state.cache_table)

    # Get access times for sorting
    entries_with_access =
      Enum.map(cache_entries, fn {cache_key, resource, expiry} ->
        {resource_type, resource_id} = cache_key
        last_access_key = {resource_type, resource_id, :last_access}

        last_access =
          case :ets.lookup(state.access_table, last_access_key) do
            [{^last_access_key, time}] -> time
            # Very old if no access time
            [] -> DateTime.from_unix!(0)
          end

        {cache_key, resource, expiry, last_access}
      end)

    # Sort by last access time (oldest first)
    sorted_entries =
      Enum.sort_by(entries_with_access, fn {_, _, _, last_access} ->
        DateTime.to_unix(last_access)
      end)

    # Remove 20% of least recently used entries
    entries_to_remove = Enum.take(sorted_entries, max(1, trunc(length(sorted_entries) * 0.2)))

    evicted_count =
      Enum.reduce(entries_to_remove, 0, fn {cache_key, _, _, _}, acc ->
        {resource_type, resource_id} = cache_key

        :ets.delete(state.cache_table, cache_key)
        :ets.delete(state.access_table, cache_key)
        :ets.delete(state.access_table, {resource_type, resource_id, :last_access})

        acc + 1
      end)

    # Update eviction stats
    :ets.update_counter(state.stats_table, :evictions, {2, evicted_count}, {:evictions, 0})

    Logger.debug("Evicted #{evicted_count} cache entries")
  end

  defp cleanup_expired_entries(state) do
    now = DateTime.utc_now()

    # Find expired entries
    expired_entries =
      :ets.select(state.cache_table, [
        {
          {:"$1", :"$2", :"$3"},
          [{:"=<", :"$3", {:const, now}}],
          [:"$1"]
        }
      ])

    # Delete expired entries
    expired_count =
      Enum.reduce(expired_entries, 0, fn cache_key, acc ->
        {resource_type, resource_id} = cache_key

        :ets.delete(state.cache_table, cache_key)
        :ets.delete(state.access_table, cache_key)
        :ets.delete(state.access_table, {resource_type, resource_id, :last_access})

        acc + 1
      end)

    if expired_count > 0 do
      Logger.debug("Cleaned up #{expired_count} expired cache entries")
    end
  end

  defp warmup_frequent_resources(state) do
    # Get frequently accessed resources (access count > 50)
    frequent_resources =
      :ets.select(state.access_table, [
        {
          {{:"$1", :"$2"}, :"$3"},
          [{:>, :"$3", 50}],
          [{{:"$1", :"$2"}}]
        }
      ])

    # Warm up each resource that's not already cached or is about to expire
    warmed_count =
      Enum.reduce(frequent_resources, 0, fn {resource_type, resource_id}, acc ->
        cache_key = {resource_type, resource_id}

        should_warmup = should_warmup_resource?(state, cache_key)

        if should_warmup do
          acc + warmup_single_resource(state, resource_type, resource_id, cache_key)
        else
          acc
        end
      end)

    if warmed_count > 0 do
      Logger.debug("Warmed up #{warmed_count} frequently accessed resources")
    end
  end

  defp should_warmup_resource?(state, cache_key) do
    case :ets.lookup(state.cache_table, cache_key) do
      # Not cached
      [] ->
        true

      [{^cache_key, _, expiry}] ->
        # Check if expires within next 5 minutes
        expires_soon = DateTime.add(DateTime.utc_now(), 300, :second)
        DateTime.compare(expiry, expires_soon) == :lt
    end
  end

  defp warmup_single_resource(state, resource_type, resource_id, cache_key) do
    case get_warmup_function(resource_type) do
      {:ok, warmup_fn} ->
        case warmup_fn.(resource_id) do
          {:ok, resource} ->
            ttl = calculate_adaptive_ttl(state, resource_type, resource_id)
            expiry = DateTime.add(DateTime.utc_now(), ttl, :second)
            :ets.insert(state.cache_table, {cache_key, resource, expiry})

            update_stats(state, :warmups)
            1

          _ ->
            0
        end

      :error ->
        0
    end
  end

  defp get_warmup_function(resource_type) do
    # Map resource types to their warmup functions
    warmup_functions = %{
      :user => &Spacecast.Resources.UserResource.load/1,
      :post => &Spacecast.Resources.PostResource.load/1,
      :team => &Spacecast.Resources.TeamResource.load/1,
      :comment => &Spacecast.Resources.CommentResource.load/1,
      :category => &Spacecast.Resources.CategoryResource.load/1
    }

    case Map.get(warmup_functions, resource_type) do
      nil -> :error
      warmup_fn -> {:ok, warmup_fn}
    end
  end

  defp schedule_cleanup(state) do
    # Cancel existing timer if any
    if state.cleanup_timer, do: Process.cancel_timer(state.cleanup_timer)

    # Schedule new cleanup
    timer = Process.send_after(self(), :cleanup, state.cleanup_interval * 1000)
    %{state | cleanup_timer: timer}
  end

  defp schedule_warmup(state) do
    # Cancel existing timer if any
    if state.warmup_timer, do: Process.cancel_timer(state.warmup_timer)

    # Schedule new warmup
    timer = Process.send_after(self(), :warmup, state.warmup_interval * 1000)
    %{state | warmup_timer: timer}
  end
end
