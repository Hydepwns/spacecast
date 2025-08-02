defmodule Spacecast.Resources.ResourceSystem do
  @moduledoc """
  Resource system for managing resources in the application.
  Provides create, list, get, update, delete, and reset operations.
  """

  use GenServer
  alias Spacecast.Resources.Resource
  alias Spacecast.RepoHelper
  alias Spacecast.Events.ResourceEventGenerator
  alias Spacecast.Transformations.TransformationPipeline
  import Ecto.Query

  @doc """
  Starts the resource system.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    init_cache()
    {:ok, %{}}
  end

  @doc """
  Creates a new resource with the given attributes.
  """
  def create_resource(attrs) do
    create_resource(attrs, %{})
  end

  @doc """
  Creates a new resource with the given attributes and options.

  ## Options
  * `:skip_events` - Skip event generation (default: false)
  * `:skip_pubsub` - Skip PubSub broadcasting (default: false)
  * `:skip_cache_invalidation` - Skip cache invalidation (default: false)
  """
  def create_resource(attrs, opts) when is_map(opts) do
    # Convert map to keyword list for compatibility
    opts_list = Map.to_list(opts)
    create_resource(attrs, opts_list)
  end

  def create_resource(attrs, opts) when is_list(opts) do
    skip_events = Keyword.get(opts, :skip_events, false)
    skip_pubsub = Keyword.get(opts, :skip_pubsub, false)
    skip_cache_invalidation = Keyword.get(opts, :skip_cache_invalidation, false)

    changeset =
      %Resource{}
      |> Resource.changeset(attrs)

    case RepoHelper.insert(changeset) do
      {:ok, resource} ->
        handle_successful_creation(resource, skip_events, skip_pubsub, skip_cache_invalidation)

      {:error, changeset} ->
        {:error, changeset}

      error ->
        error
    end
  end

  defp handle_successful_creation(resource, skip_events, skip_pubsub, skip_cache_invalidation) do
    unless skip_cache_invalidation do
      invalidate_resource_cache()
    end

    unless skip_events do
      generate_creation_event(resource)
    end

    unless skip_pubsub do
      broadcast_creation(resource)
    end

    {:ok, resource}
  end

  defp generate_creation_event(resource) do
    _event_data = Map.from_struct(resource) |> Map.drop([:__meta__, :__struct__])

    case ResourceEventGenerator.resource_created(resource, %{action: "create"}) do
      {:ok, _event} -> :ok
      # Continue even if event generation fails
      {:error, _reason} -> :ok
    end
  end

  defp broadcast_creation(resource) do
    Phoenix.PubSub.broadcast(
      Spacecast.PubSub,
      "resources",
      {:resource_created, resource}
    )
  end

  @doc """
  Creates multiple resources efficiently in a batch operation.
  This is optimized for performance when creating many resources at once.
  """
  def create_resources_batch(resources_attrs, opts \\ [])

  def create_resources_batch(resources_attrs, opts) when is_map(opts) do
    # Convert map to keyword list for compatibility
    opts_list = Map.to_list(opts)
    create_resources_batch(resources_attrs, opts_list)
  end

  def create_resources_batch(resources_attrs, opts) when is_list(opts) do
    skip_events = Keyword.get(opts, :skip_events, false)
    skip_pubsub = Keyword.get(opts, :skip_pubsub, false)
    skip_cache_invalidation = Keyword.get(opts, :skip_cache_invalidation, false)

    # Create all resources in a transaction
    case create_resources_in_transaction(resources_attrs) do
      {:ok, resources} ->
        handle_batch_success(resources, skip_events, skip_pubsub, skip_cache_invalidation)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp create_resources_in_transaction(resources_attrs) do
    RepoHelper.transaction(fn ->
      Enum.map(resources_attrs, &create_single_resource/1)
    end)
  end

  defp create_single_resource(attrs) do
    changeset = %Resource{} |> Resource.changeset(attrs)

    case RepoHelper.insert(changeset) do
      {:ok, resource} -> {:ok, resource}
      {:error, changeset} -> RepoHelper.rollback(changeset)
    end
  end

  defp handle_batch_success(resources, skip_events, skip_pubsub, skip_cache_invalidation) do
    unless skip_cache_invalidation do
      invalidate_resource_cache()
    end

    unless skip_events do
      generate_batch_events(resources)
    end

    unless skip_pubsub do
      broadcast_batch_creations(resources)
    end

    {:ok, Enum.map(resources, fn {:ok, resource} -> resource end)}
  end

  defp generate_batch_events(resources) do
    Enum.each(resources, fn {:ok, resource} ->
      _event_data = Map.from_struct(resource) |> Map.drop([:__meta__, :__struct__])
      ResourceEventGenerator.resource_created(resource, %{action: "create"})
    end)
  end

  defp broadcast_batch_creations(resources) do
    Enum.each(resources, fn {:ok, resource} ->
      Phoenix.PubSub.broadcast(
        Spacecast.PubSub,
        "resources",
        {:resource_created, resource}
      )
    end)
  end

  @doc """
  Lists all resources with optional pagination and caching.
  """
  def list_resources(opts \\ []) do
    limit = Keyword.get(opts, :limit, 50)
    offset = Keyword.get(opts, :offset, 0)
    use_cache = Keyword.get(opts, :use_cache, true)

    if use_cache do
      cached_list_resources(limit, offset)
    else
      direct_list_resources(limit, offset)
    end
  end

  @doc """
  Lists resources with optional filtering and pagination.
  """
  def list_resources_with_filters(filters, opts \\ []) when is_map(filters) do
    limit = Keyword.get(opts, :limit, 50)
    offset = Keyword.get(opts, :offset, 0)
    use_cache = Keyword.get(opts, :use_cache, true)

    if use_cache do
      cached_list_resources_with_filters(filters, limit, offset)
    else
      direct_list_resources_with_filters(filters, limit, offset)
    end
  end

  @doc """
  Gets the total count of resources (for pagination).
  """
  def count_resources(filters \\ %{}) do
    query = from(r in Resource)

    query =
      case filters do
        %{type: type} when not is_nil(type) ->
          from(r in query, where: r.type == ^type)

        _ ->
          query
      end

    query =
      case filters do
        %{status: status} when not is_nil(status) ->
          from(r in query, where: r.status == ^status)

        _ ->
          query
      end

    query =
      case filters do
        %{parent_id: parent_id} when not is_nil(parent_id) ->
          from(r in query, where: r.parent_id == ^parent_id)

        _ ->
          query
      end

    RepoHelper.aggregate(query, :count, :id)
  end

  # Private functions for direct database queries
  defp direct_list_resources(limit, offset) do
    resources =
      Resource
      |> order_by([r], desc: r.inserted_at)
      |> limit(^limit)
      |> offset(^offset)
      |> RepoHelper.all()

    if Mix.env() == :test do
      IO.puts(
        "[DEBUG] direct_list_resources/2 returned #{length(resources)} resources: #{inspect(Enum.map(resources, & &1.name))}"
      )
    end

    resources
  end

  defp direct_list_resources_with_filters(filters, limit, offset) do
    query = from(r in Resource)

    query =
      case filters do
        %{type: type} when not is_nil(type) ->
          from(r in query, where: r.type == ^type)

        _ ->
          query
      end

    query =
      case filters do
        %{status: status} when not is_nil(status) ->
          from(r in query, where: r.status == ^status)

        _ ->
          query
      end

    query =
      case filters do
        %{parent_id: parent_id} when not is_nil(parent_id) ->
          from(r in query, where: r.parent_id == ^parent_id)

        _ ->
          query
      end

    query
    |> order_by([r], desc: r.inserted_at)
    |> limit(^limit)
    |> offset(^offset)
    |> RepoHelper.all()
  end

  # Private functions for cached queries
  defp cached_list_resources(limit, offset) do
    cache_key = "resources:list:#{limit}:#{offset}"

    case get_cache(cache_key) do
      {:ok, resources} ->
        resources

      {:error, :not_found} ->
        resources = direct_list_resources(limit, offset)
        # Cache for 5 minutes
        set_cache(cache_key, resources, 300)
        resources
    end
  end

  defp cached_list_resources_with_filters(filters, limit, offset) do
    cache_key = "resources:filtered:#{hash_filters(filters)}:#{limit}:#{offset}"

    case get_cache(cache_key) do
      {:ok, resources} ->
        resources

      {:error, :not_found} ->
        resources = direct_list_resources_with_filters(filters, limit, offset)
        # Cache for 5 minutes
        set_cache(cache_key, resources, 300)
        resources
    end
  end

  # Simple cache implementation using ETS
  defp get_cache(key) do
    case :ets.lookup(:resource_cache, key) do
      [{^key, value, expiry}] ->
        if DateTime.compare(expiry, DateTime.utc_now()) == :gt do
          {:ok, value}
        else
          :ets.delete(:resource_cache, key)
          {:error, :not_found}
        end

      [] ->
        {:error, :not_found}
    end
  end

  defp set_cache(key, value, ttl_seconds) do
    expiry = DateTime.add(DateTime.utc_now(), ttl_seconds, :second)
    :ets.insert(:resource_cache, {key, value, expiry})
    :ok
  end

  defp hash_filters(filters) do
    :erlang.phash2(filters)
  end

  # Initialize cache table on startup
  defp init_cache do
    case :ets.info(:resource_cache) do
      :undefined ->
        :ets.new(:resource_cache, [:set, :public, :named_table])

      _ ->
        :ok
    end
  end

  # Invalidate all resource cache entries
  defp invalidate_resource_cache do
    :ets.delete_all_objects(:resource_cache)
  end

  # Reset the cache (for testing)
  def reset_cache do
    :ets.delete_all_objects(:resource_cache)
  end

  @doc """
  Gets a resource by id.
  """
  def get_resource(id) when is_nil(id) or id == "" do
    {:error, :not_found}
  end

  def get_resource(id) do
    case RepoHelper.get(Resource, id) do
      nil -> {:error, :not_found}
      resource -> {:ok, resource}
    end
  end

  @doc """
  Updates a resource by id with new attributes.
  """
  def update_resource(id, attrs) do
    IO.puts("🔍 ResourceSystem.update_resource: Starting update for resource #{id}")

    case RepoHelper.get(Resource, id) do
      nil ->
        IO.puts("❌ ResourceSystem.update_resource: Resource not found")
        {:error, :not_found}

      resource ->
        IO.puts("🔍 ResourceSystem.update_resource: Found resource #{resource.id}, creating changeset")

        case update_resource_changeset(resource, attrs) do
          {:ok, updated_resource} ->
            handle_successful_update(updated_resource, resource, attrs)

          error ->
            IO.puts("❌ ResourceSystem.update_resource: RepoHelper.update failed: #{inspect(error)}")

            error
        end
    end
  end

  defp update_resource_changeset(resource, attrs) do
    resource
    |> Resource.changeset(attrs)
    |> RepoHelper.update()
  end

  defp handle_successful_update(updated_resource, original_resource, attrs) do
    IO.puts("✅ ResourceSystem.update_resource: Resource updated successfully")

    invalidate_resource_cache()
    generate_update_event(updated_resource, attrs)
    broadcast_update(updated_resource)

    case apply_transformations(updated_resource, original_resource) do
      {:ok, transformed_resource} ->
        IO.puts("✅ ResourceSystem.update_resource: Returning transformed resource")
        {:ok, transformed_resource}

      {:error, _resource, _context} ->
        IO.puts("❌ ResourceSystem.update_resource: Transformations failed, returning updated resource")
        {:ok, updated_resource}
    end
  end

  defp generate_update_event(updated_resource, attrs) do
    IO.puts("🔍 ResourceSystem.update_resource: Generating resource_updated event")

    case ResourceEventGenerator.resource_updated(updated_resource, attrs, %{action: "update"}) do
      {:ok, _event} ->
        IO.puts("✅ ResourceSystem.update_resource: resource_updated event generated successfully")

      {:error, reason} ->
        IO.puts("❌ ResourceSystem.update_resource: resource_updated event generation failed: #{inspect(reason)}")
    end
  end

  defp broadcast_update(updated_resource) do
    Phoenix.PubSub.broadcast(
      Spacecast.PubSub,
      "resources",
      {:resource_updated, updated_resource}
    )
  end

  defp apply_transformations(updated_resource, original_resource) do
    IO.puts("🔍 ResourceSystem.update_resource: Applying transformations")

    case TransformationPipeline.apply_transformations(
           updated_resource,
           :resource,
           :update,
           phase: :after_validation,
           original_resource: original_resource
         ) do
      {:ok, transformed_resource, _context} ->
        IO.puts("✅ ResourceSystem.update_resource: Transformations applied successfully")
        generate_transformation_event(transformed_resource)
        broadcast_transformation(transformed_resource)
        {:ok, transformed_resource}

      error ->
        error
    end
  end

  defp generate_transformation_event(transformed_resource) do
    case ResourceEventGenerator.resource_event(transformed_resource, "transformed", %{}, %{action: "transform"}) do
      {:ok, _event} ->
        IO.puts("✅ ResourceSystem.update_resource: transformed event generated successfully")

      {:error, reason} ->
        IO.puts("❌ ResourceSystem.update_resource: transformed event generation failed: #{inspect(reason)}")
    end
  end

  defp broadcast_transformation(transformed_resource) do
    Phoenix.PubSub.broadcast(
      Spacecast.PubSub,
      "resources",
      {:resource_transformed, transformed_resource}
    )
  end

  @doc """
  Deletes a resource by id.
  """
  def delete_resource(id) do
    case RepoHelper.get(Resource, id) do
      nil ->
        {:error, :not_found}

      resource ->
        case RepoHelper.delete(resource) do
          {:ok, deleted_resource} ->
            handle_successful_deletion(deleted_resource)

          error ->
            error
        end
    end
  end

  defp handle_successful_deletion(deleted_resource) do
    # Invalidate cache
    invalidate_resource_cache()
    # Generate event for resource deletion
    generate_deletion_event(deleted_resource)
    # Broadcast PubSub message for real-time updates
    broadcast_deletion(deleted_resource)
    {:ok, deleted_resource}
  end

  defp generate_deletion_event(deleted_resource) do
    _event_data = Map.from_struct(deleted_resource) |> Map.drop([:__meta__, :__struct__])

    case ResourceEventGenerator.resource_deleted(deleted_resource, %{action: "delete"}) do
      {:ok, _event} ->
        IO.puts("✅ ResourceSystem.delete_resource: resource_deleted event generated successfully")

      {:error, reason} ->
        IO.puts("❌ ResourceSystem.delete_resource: resource_deleted event generation failed: #{inspect(reason)}")
    end
  end

  defp broadcast_deletion(deleted_resource) do
    Phoenix.PubSub.broadcast(
      Spacecast.PubSub,
      "resources",
      {:resource_deleted, deleted_resource}
    )
  end

  @doc """
  Resets the store (for tests).
  """
  def reset_store do
    RepoHelper.delete_all(Resource)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent,
      shutdown: 500
    }
  end
end
