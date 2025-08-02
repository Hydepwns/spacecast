defmodule Spacecast.Resources.ResourceSandbox do
  @moduledoc """
  Resource sandbox for isolated testing and experimentation.

  This module provides a sandbox environment where developers can experiment
  with resources without affecting production data.
  """

  require Logger

  @type sandbox_id :: String.t()
  @type resource_module :: module()
  @type resource_type :: String.t()
  @type instance_id :: String.t()
  @type sandbox :: %{
          id: sandbox_id,
          created_at: DateTime.t(),
          resources: map(),
          events: list(map()),
          options: keyword()
        }

  @spec create_sandbox(list(resource_module()), keyword()) :: {:ok, sandbox_id()} | {:error, any()}
  @doc """
  Creates a sandbox for experimenting with resources.

  This sets up an isolated environment where developers can experiment
  with resources without affecting production data.

  ## Parameters
  * `resource_modules` - List of resource modules to include
  * `opts` - Sandbox options

  ## Returns
  * `{:ok, sandbox_id}` - Sandbox created
  * `{:error, reason}` - Failed to create sandbox
  """
  def create_sandbox(resource_modules, opts \\ []) do
    sandbox_id = Keyword.get(opts, :id, Ecto.UUID.generate())

    # Initialize sandbox state
    sandbox = %{
      id: sandbox_id,
      created_at: DateTime.utc_now(),
      resources: %{},
      events: [],
      options: opts
    }

    # Initialize resources in sandbox
    resources =
      Enum.reduce(resource_modules, %{}, fn resource_module, acc ->
        resource_type = resource_module.resource_type()

        # Create a test instance
        instance_id = Ecto.UUID.generate()
        initial_state = resource_module.initial_state()

        # Add to sandbox resources
        Map.put(acc, resource_type, %{
          module: resource_module,
          instances: %{
            instance_id => %{
              id: instance_id,
              state: initial_state,
              events: []
            }
          }
        })
      end)

    # Store sandbox with resources
    sandbox = Map.put(sandbox, :resources, resources)
    Process.put({:resource_sandbox, sandbox_id}, sandbox)

    Logger.info("Resource sandbox #{sandbox_id} created with #{length(resource_modules)} resource types")

    {:ok, sandbox_id}
  end

  @spec get_sandbox(sandbox_id()) :: {:ok, sandbox()} | {:error, :sandbox_not_found}
  @doc """
  Gets a sandbox by ID.

  ## Parameters
  * `sandbox_id` - The sandbox ID

  ## Returns
  * `{:ok, sandbox}` - Sandbox found
  * `{:error, :sandbox_not_found}` - Sandbox not found
  """
  def get_sandbox(sandbox_id) do
    case Process.get({:resource_sandbox, sandbox_id}) do
      nil -> {:error, :sandbox_not_found}
      sandbox -> {:ok, sandbox}
    end
  end

  @spec apply_event(sandbox_id(), resource_type(), instance_id(), map()) ::
          {:ok, any()} | {:error, any()}
  @doc """
  Applies an event in a sandbox environment.

  ## Parameters
  * `sandbox_id` - The sandbox ID
  * `resource_type` - The resource type
  * `instance_id` - The resource instance ID
  * `event` - The event to apply

  ## Returns
  * `{:ok, new_state}` - Event applied successfully
  * `{:error, reason}` - Failed to apply event
  """
  def apply_event(sandbox_id, resource_type, instance_id, event) do
    with {:ok, sandbox} <- get_sandbox(sandbox_id),
         {:ok, resource} <- get_sandbox_resource(sandbox, resource_type),
         {:ok, instance} <- get_sandbox_instance(resource, instance_id) do
      # Get the resource module
      resource_module = resource.module

      # Apply the event to get new state
      new_state = resource_module.apply_event(event, instance.state)

      # Update instance with new state and event
      updated_instance = %{
        instance
        | state: new_state,
          events: [event | instance.events]
      }

      # Update resource instances
      updated_resource = %{
        resource
        | instances: Map.put(resource.instances, instance_id, updated_instance)
      }

      # Update sandbox resources
      updated_resources = Map.put(sandbox.resources, resource_type, updated_resource)

      # Update sandbox
      updated_sandbox = %{
        sandbox
        | resources: updated_resources,
          events: [event | sandbox.events]
      }

      # Store updated sandbox
      Process.put({:resource_sandbox, sandbox_id}, updated_sandbox)

      {:ok, new_state}
    end
  end

  @spec get_instance_state(sandbox_id(), resource_type(), instance_id()) ::
          {:ok, any()} | {:error, any()}
  @doc """
  Gets the current state of a resource instance in the sandbox.

  ## Parameters
  * `sandbox_id` - The sandbox ID
  * `resource_type` - The resource type
  * `instance_id` - The resource instance ID

  ## Returns
  * `{:ok, state}` - Current state
  * `{:error, reason}` - Failed to get state
  """
  def get_instance_state(sandbox_id, resource_type, instance_id) do
    with {:ok, sandbox} <- get_sandbox(sandbox_id),
         {:ok, resource} <- get_sandbox_resource(sandbox, resource_type),
         {:ok, instance} <- get_sandbox_instance(resource, instance_id) do
      {:ok, instance.state}
    end
  end

  @spec list_instances(sandbox_id(), resource_type()) :: {:ok, list(instance_id())} | {:error, any()}
  @doc """
  Lists all instances of a resource type in the sandbox.

  ## Parameters
  * `sandbox_id` - The sandbox ID
  * `resource_type` - The resource type

  ## Returns
  * `{:ok, instance_ids}` - List of instance IDs
  * `{:error, reason}` - Failed to list instances
  """
  def list_instances(sandbox_id, resource_type) do
    with {:ok, sandbox} <- get_sandbox(sandbox_id),
         {:ok, resource} <- get_sandbox_resource(sandbox, resource_type) do
      instance_ids = Map.keys(resource.instances)
      {:ok, instance_ids}
    end
  end

  @spec reset_sandbox(sandbox_id()) :: {:ok, sandbox_id()} | {:error, any()}
  @doc """
  Resets a sandbox to its initial state.

  ## Parameters
  * `sandbox_id` - The sandbox ID

  ## Returns
  * `{:ok, sandbox_id}` - Sandbox reset
  * `{:error, reason}` - Failed to reset sandbox
  """
  def reset_sandbox(sandbox_id) do
    with {:ok, sandbox} <- get_sandbox(sandbox_id) do
      # Recreate the sandbox with the same options
      {:ok, new_sandbox_id} = create_sandbox([], sandbox.options)

      # Copy the original resource modules
      resource_modules =
        sandbox.resources
        |> Map.values()
        |> Enum.map(& &1.module)

      if length(resource_modules) > 0 do
        {:ok, _} = create_sandbox(resource_modules, sandbox.options)
      end

      Logger.info("Sandbox #{sandbox_id} reset")

      {:ok, sandbox_id}
    end
  end

  # Private helper functions

  defp get_sandbox_resource(sandbox, resource_type) do
    case Map.get(sandbox.resources, resource_type) do
      nil -> {:error, :resource_not_found_in_sandbox}
      resource -> {:ok, resource}
    end
  end

  defp get_sandbox_instance(resource, instance_id) do
    case Map.get(resource.instances, instance_id) do
      nil -> {:error, :instance_not_found_in_sandbox}
      instance -> {:ok, instance}
    end
  end
end
