defmodule Spacecast.Integration.IntegrationBridge do
  @moduledoc """
  Bridge to integrate external systems with Hydepwns resources.

  This module provides a standardized interface for connecting resources to various
  external services, APIs, and data sources. It handles conversion between the
  external system's data format and the internal resource representation.

  Features:
  - Adapter-based design for easy addition of new integrations
  - Event publishing for integration activities
  - Configurable transformation pipeline
  - Error handling and retry mechanisms
  - Rate limiting for external API calls
  """

  alias Spacecast.Events.Core.Event
  require Logger

  @doc """
  Registers an integration adapter with the bridge.

  ## Parameters
  * `name` - Unique name for the integration adapter
  * `adapter_module` - Module implementing the IntegrationAdapter behavior
  * `config` - Configuration for the adapter

  ## Returns
  * `{:ok, adapter_id}` - Adapter successfully registered
  * `{:error, reason}` - Failed to register adapter
  """
  def register_adapter(name, adapter_module, config \\ %{}) do
    # Basic validation
    with {:module, _} <- Code.ensure_loaded(adapter_module),
         true <- function_exported?(adapter_module, :validate_config, 1),
         :ok <- adapter_module.validate_config(config) do
      adapter_id = "adapter-#{name}-#{:erlang.system_time(:millisecond)}"

      # Store the adapter in persistent storage (ETS, persistent term, or database)
      # For simplicity, we'll use the application env in this example
      adapters = Application.get_env(:spacecast, :integration_adapters, %{})

      adapter_info = %{
        id: adapter_id,
        name: name,
        module: adapter_module,
        config: config,
        registered_at: DateTime.utc_now()
      }

      adapters = Map.put(adapters, adapter_id, adapter_info)
      Application.put_env(:spacecast, :integration_adapters, adapters)

      # Publish adapter registration event
      Spacecast.Events.EventBus.publish(%Event{
        type: "integration_adapter_registered",
        resource_id: adapter_id,
        data: %{name: name, adapter_module: adapter_module},
        metadata: %{config: redact_sensitive_config(config)}
      })

      {:ok, adapter_id}
    else
      {:error, reason} -> {:error, reason}
      false -> {:error, :adapter_missing_validate_config}
      _ -> {:error, :invalid_adapter}
    end
  end

  @doc """
  Imports data from an external system through a registered adapter.

  ## Parameters
  * `adapter_id` - ID of the registered adapter
  * `resource_type` - Type of resource to import into
  * `resource_module` - Resource module to handle the imported data
  * `import_options` - Options for the import operation

  ## Returns
  * `{:ok, imported_resources}` - Successfully imported resources
  * `{:error, reason}` - Import failed
  """
  def import_from_external(adapter_id, resource_type, resource_module, import_options \\ %{}) do
    with {:ok, adapter} <- get_adapter(adapter_id),
         {:ok, external_data} <- call_adapter(adapter, :fetch_data, [import_options]) do
      # Transform external data to resource format
      results =
        Enum.map(external_data, fn item ->
          case transform_to_resource(item, resource_type, resource_module) do
            {:ok, resource_data} ->
              # Create or update resource
              resource_id = Map.get(resource_data, :id) || generate_resource_id(resource_type)

              Spacecast.Events.EventBus.publish(%Event{
                type: "#{resource_type}_imported",
                resource_id: resource_id,
                data: resource_data,
                metadata: %{
                  source: adapter.name,
                  adapter_id: adapter_id,
                  external_id: Map.get(item, :id)
                }
              })

              {:ok, resource_id}

            {:error, reason} ->
              {:error, reason}
          end
        end)

      # Separate successes and failures
      {successes, failures} =
        Enum.split_with(results, fn
          {:ok, _} -> true
          _ -> false
        end)

      success_ids = Enum.map(successes, fn {:ok, id} -> id end)

      # Log any failures
      unless Enum.empty?(failures) do
        failures
        |> Enum.each(fn {:error, reason} ->
          # Log failure here or send to error tracking system
          IO.puts("Import failure: #{inspect(reason)}")
        end)
      end

      {:ok, success_ids}
    end
  end

  @doc """
  Exports resources to an external system through a registered adapter.

  ## Parameters
  * `adapter_id` - ID of the registered adapter
  * `resource_ids` - List of resource IDs to export
  * `resource_module` - Resource module for the resources
  * `export_options` - Options for the export operation

  ## Returns
  * `{:ok, exported_resources}` - Successfully exported resources
  * `{:error, reason}` - Export failed
  """
  def export_to_external(adapter_id, resource_ids, resource_module, export_options \\ %{}) do
    with {:ok, adapter} <- get_adapter(adapter_id) do
      # Load resources
      resources =
        Enum.map(resource_ids, fn id ->
          case resource_module.load(id) do
            {:ok, resource} -> {:ok, resource}
            {:error, reason} -> {:error, id, reason}
          end
        end)

      # Separate successes and failures
      {loaded_resources, _load_failures} =
        Enum.split_with(resources, fn
          {:ok, _} -> true
          _ -> false
        end)

      # Transform resources to external format
      transformed_resources =
        Enum.map(loaded_resources, fn {:ok, resource} ->
          case transform_to_external(resource, adapter) do
            {:ok, external_data} -> {:ok, resource.id, external_data}
            {:error, reason} -> {:error, resource.id, reason}
          end
        end)

      # Separate successful transformations
      {to_export, _transform_failures} =
        Enum.split_with(transformed_resources, fn
          {:ok, _, _} -> true
          _ -> false
        end)

      # Export to external system
      export_data = Enum.map(to_export, fn {:ok, _, data} -> data end)

      case call_adapter(adapter, :export_data, [export_data, export_options]) do
        {:ok, external_ids} ->
          # Map internal IDs to external IDs
          exported =
            Enum.zip_with(
              Enum.map(to_export, fn {:ok, id, _} -> id end),
              external_ids,
              fn internal_id, external_id ->
                Spacecast.Events.EventBus.publish(%Event{
                  type: "#{resource_module.resource_type()}_exported",
                  resource_id: internal_id,
                  data: %{external_id: external_id},
                  metadata: %{
                    target: adapter.name,
                    adapter_id: adapter_id
                  }
                })

                %{internal_id: internal_id, external_id: external_id}
              end
            )

          {:ok, exported}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  @doc """
  Synchronizes resources with an external system.

  This performs a two-way sync between internal resources and the external system,
  updating both sides with the latest changes.

  ## Parameters
  * `adapter_id` - ID of the registered adapter
  * `resource_type` - Type of resource to synchronize
  * `resource_module` - Resource module to handle the synchronized data
  * `sync_options` - Options for the synchronization operation

  ## Returns
  * `{:ok, sync_report}` - Sync completed with a report of changes
  * `{:error, reason}` - Sync failed
  """
  def synchronize(adapter_id, resource_type, resource_module, sync_options \\ %{}) do
    with {:ok, adapter} <- get_adapter(adapter_id) do
      # Default sync direction is both ways
      direction = Map.get(sync_options, :direction, :both)

      case direction do
        :inbound ->
          # Only import from external to internal
          import_from_external(adapter_id, resource_type, resource_module, sync_options)

        :outbound ->
          # Only export from internal to external
          # First, we need to get the resources to export
          # This could be based on a query, modified since, etc.
          resource_ids = get_resources_for_sync(resource_type, sync_options)
          export_to_external(adapter_id, resource_ids, resource_module, sync_options)

        :both ->
          # Two-way sync with conflict resolution
          # For this example, we'll implement a simple last-modified wins strategy

          # 1. Get all resources to sync
          resource_ids = get_resources_for_sync(resource_type, sync_options)

          # 2. Get external data
          # {:ok, adapter} has already been fetched
          {:ok, external_data} = call_adapter(adapter, :fetch_data, [sync_options])

          # 3. Map external data to internal resources
          mapping = create_internal_external_mapping(external_data, resource_ids, resource_module)

          # 4. Sync each pair of resources
          results =
            Enum.map(mapping, fn {internal_id, external_item} ->
              sync_pair(resource_module, internal_id, external_item, adapter, sync_options)
            end)

          # 5. Create report
          {:ok, create_sync_report(results)}
      end
    else
      # Propagate adapter fetch error
      err -> err
    end
  end

  @doc """
  Lists all registered integration adapters.

  ## Returns
  * Map of adapter IDs to adapter info
  """
  def list_adapters do
    Application.get_env(:spacecast, :integration_adapters, %{})
  end

  # Private helpers

  defp get_adapter(adapter_id) do
    adapters = Application.get_env(:spacecast, :integration_adapters, %{})

    case Map.fetch(adapters, adapter_id) do
      {:ok, adapter} -> {:ok, adapter}
      :error -> {:error, :adapter_not_found}
    end
  end

  defp call_adapter(adapter, function, args, retry_opts \\ []) do
    # Default retry options
    max_retries = Keyword.get(retry_opts, :max_retries, 3)
    initial_backoff_ms = Keyword.get(retry_opts, :initial_backoff_ms, 100)
    max_backoff_ms = Keyword.get(retry_opts, :max_backoff_ms, 5000)
    jitter = Keyword.get(retry_opts, :jitter, 0.25)

    # Call with retries
    call_adapter_with_retry(
      adapter,
      function,
      args,
      max_retries,
      initial_backoff_ms,
      max_backoff_ms,
      jitter,
      0
    )
  end

  defp call_adapter_with_retry(
         adapter,
         function,
         args,
         max_retries,
         initial_backoff_ms,
         max_backoff_ms,
         jitter,
         retry_count
       ) do
    # Attempt to call the adapter
    result =
      try do
        apply(adapter.module, function, args)
      rescue
        e -> {:error, {e, __STACKTRACE__}}
      catch
        kind, reason -> {:error, {kind, reason}}
      end

    case result do
      {:ok, _} = success ->
        # Successful call, return the result
        success

      {:error, reason} = error ->
        # Check if we should retry
        if retry_count < max_retries && should_retry?(reason) do
          # Calculate backoff with exponential increase and jitter
          backoff_ms = calculate_backoff(initial_backoff_ms, retry_count, max_backoff_ms, jitter)

          # Log the retry attempt
          Logger.warning(
            "Adapter call failed, retrying in #{backoff_ms}ms (attempt #{retry_count + 1}/#{max_retries}): #{inspect(reason)}"
          )

          # Wait before retrying
          Process.sleep(backoff_ms)

          # Retry the call
          call_adapter_with_retry(
            adapter,
            function,
            args,
            max_retries,
            initial_backoff_ms,
            max_backoff_ms,
            jitter,
            retry_count + 1
          )
        else
          # Max retries reached or non-retryable error
          if retry_count >= max_retries do
            Logger.error("Adapter call failed after #{max_retries} retries: #{inspect(reason)}")
          else
            Logger.error("Adapter call failed with non-retryable error: #{inspect(reason)}")
          end

          error
        end
    end
  end

  # Determine if an error is retryable
  defp should_retry?(reason) do
    case reason do
      # Network-related errors are typically retryable
      {%{reason: :timeout}, _} -> true
      {%{reason: :econnrefused}, _} -> true
      {%{reason: :closed}, _} -> true
      {%{reason: :nxdomain}, _} -> true
      # Rate limiting errors are retryable
      {%{status_code: 429}, _} -> true
      # Server errors are often transient and retryable
      {%{status_code: code}, _} when code >= 500 and code < 600 -> true
      # Other errors that might be retryable
      {:timeout, _} -> true
      {:connection_error, _} -> true
      # Default to not retrying for other errors
      _ -> false
    end
  end

  # Calculate exponential backoff with jitter
  defp calculate_backoff(initial_ms, retry_count, max_ms, jitter) do
    # Calculate base backoff with exponential increase: initial * 2^retry_count
    base_backoff = initial_ms * :math.pow(2, retry_count)

    # Apply maximum limit
    capped_backoff = min(base_backoff, max_ms)

    # Apply jitter to avoid thundering herd problem
    # This adds or subtracts a random percentage (up to jitter%) of the backoff value
    jitter_factor = 1.0 - jitter + :rand.uniform() * jitter * 2

    # Calculate final backoff with jitter and convert to integer
    trunc(capped_backoff * jitter_factor)
  end

  defp transform_to_resource(external_data, _resource_type, _resource_module) do
    # Transform external data to resource format
    external_data
  end

  defp transform_to_external(resource, _adapter) do
    # Transform resource to external format
    resource
  end

  defp redact_sensitive_config(config) do
    # Redact sensitive fields like passwords, API keys, etc.
    sensitive_keys = ~w(password api_key secret token)a

    Enum.reduce(sensitive_keys, config, fn key, acc ->
      if Map.has_key?(acc, key) do
        Map.put(acc, key, "[REDACTED]")
      else
        acc
      end
    end)
  end

  defp generate_resource_id(resource_type) do
    "#{resource_type}-#{Ecto.UUID.generate()}"
  end

  defp get_resources_for_sync(_resource_type, _sync_options) do
    # Get resources for sync
    {:ok, []}
  end

  defp create_internal_external_mapping(_external_data, _resource_ids, _resource_module) do
    # This function needs to match external items with existing internal resources.
    # It might involve looking up by external ID, email, or other unique identifiers.
    # For this placeholder, it returns an empty list.
    []
  end

  defp sync_pair(_resource_module, id, _external_system, _external_id, _sync_config) do
    # Sync a resource with external data
    {:ok, :synced, %{id: id, status: "placeholder_synced"}}
  end

  defp create_sync_report(results) do
    # Summarize what happened during the sync.
    # e.g., number of created, updated, deleted items on both sides.
    %{summary: "Sync completed", details: results}
  end
end
