defmodule Spacecast.TestSupport.EventTestHelper do
  @moduledoc """
  Test helper for creating and managing events in tests.

  This module provides utilities to:
  - Create events using the main event creation API
  - Ensure consistent event type usage
  - Provide helper functions for common event operations in tests
  """

  alias Spacecast.Events.EventOperations
  alias Spacecast.Events.ResourceIntegration.ResourceEventGenerator

  @doc """
  Creates an event using the main event creation API.
  This ensures events are processed through the same pipeline as production code.

  ## Parameters
  * `event_type` - The type of event (e.g., "resource.created")
  * `data` - The event data
  * `opts` - Optional parameters (resource_id, resource_type, metadata)

  ## Returns
  * `{:ok, event}` - The event was created successfully
  * `{:error, reason}` - The event creation failed
  """
  def create_event(event_type, data, opts \\ %{}) do
    resource_id = Map.get(opts, :resource_id, "test-#{System.unique_integer()}")
    resource_type = Map.get(opts, :resource_type, "resource")
    metadata = Map.get(opts, :metadata, %{})

    event_attrs = %{
      resource_id: resource_id,
      resource_type: resource_type,
      data: data,
      metadata: metadata
    }

    EventOperations.store_event(event_type, event_attrs)
  end

  @doc """
  Creates a resource event using the ResourceEventGenerator.
  This ensures consistency with the main resource event creation flow.

  ## Parameters
  * `resource` - The resource map or struct
  * `event_type` - The event type suffix (e.g., "created", "updated", "deleted")
  * `data` - Additional event data
  * `metadata` - Additional metadata

  ## Returns
  * `{:ok, event}` - The event was created successfully
  * `{:error, reason}` - The event creation failed
  """
  def create_resource_event(resource, event_type, data \\ %{}, metadata \\ %{}) do
    ResourceEventGenerator.resource_event(resource, event_type, data, metadata)
  end

  @doc """
  Creates a resource creation event.

  ## Parameters
  * `resource` - The resource that was created
  * `metadata` - Additional metadata

  ## Returns
  * `{:ok, event}` - The event was created successfully
  * `{:error, reason}` - The event creation failed
  """
  def create_resource_created_event(resource, metadata \\ %{}) do
    ResourceEventGenerator.resource_created(resource, metadata)
  end

  @doc """
  Creates a resource update event.

  ## Parameters
  * `resource` - The resource that was updated
  * `changes` - Map of changes that were made
  * `metadata` - Additional metadata

  ## Returns
  * `{:ok, event}` - The event was created successfully
  * `{:error, reason}` - The event creation failed
  """
  def create_resource_updated_event(resource, changes \\ %{}, metadata \\ %{}) do
    ResourceEventGenerator.resource_updated(resource, changes, metadata)
  end

  @doc """
  Creates a resource deletion event.

  ## Parameters
  * `resource` - The resource that was deleted
  * `metadata` - Additional metadata

  ## Returns
  * `{:ok, event}` - The event was created successfully
  * `{:error, reason}` - The event creation failed
  """
  def create_resource_deleted_event(resource, metadata \\ %{}) do
    ResourceEventGenerator.resource_deleted(resource, metadata)
  end

  @doc """
  Creates a test resource for use in event tests.

  ## Parameters
  * `attrs` - Resource attributes
  * `opts` - Options (type, id)

  ## Returns
  * Resource map with required fields
  """
  def create_test_resource(attrs \\ %{}, opts \\ %{}) do
    resource_type = Map.get(opts, :type, "document")
    resource_id = Map.get(opts, :id, "test-#{resource_type}-#{System.unique_integer()}")

    default_attrs = %{
      id: resource_id,
      name: "Test #{String.capitalize(resource_type)}",
      type: resource_type,
      status: "active",
      content: %{text: "Test content"}
    }

    Map.merge(default_attrs, attrs)
  end

  @doc """
  Asserts that an event was created with the expected properties.

  ## Parameters
  * `event` - The event to check
  * `expected_type` - Expected event type
  * `expected_resource_id` - Expected resource ID
  * `expected_data` - Expected event data (optional)

  ## Returns
  * `true` if the event matches expectations, raises an assertion error otherwise
  """
  def assert_event_properties(event, expected_type, expected_resource_id, expected_data \\ %{}) do
    import ExUnit.Assertions

    assert event.type == expected_type, "Expected event type #{expected_type}, got #{event.type}"

    assert event.resource_id == expected_resource_id,
           "Expected resource ID #{expected_resource_id}, got #{event.resource_id}"

    if map_size(expected_data) > 0 do
      Enum.each(expected_data, fn {key, value} ->
        # Try both atom and string keys for flexibility
        string_key = to_string(key)
        atom_key = String.to_existing_atom(string_key)

        actual_value = Map.get(event.data, atom_key) || Map.get(event.data, string_key)

        assert actual_value == value,
               "Expected event data #{key} to be #{value}, got #{actual_value}"
      end)
    end

    true
  end

  @doc """
  Creates a sequence of events for testing event ordering.

  ## Parameters
  * `resource` - The resource to create events for
  * `event_types` - List of event type suffixes to create

  ## Returns
  * List of created events
  """
  def create_event_sequence(resource, event_types) do
    Enum.map(event_types, fn event_type ->
      case event_type do
        "created" -> create_resource_created_event(resource)
        "updated" -> create_resource_updated_event(resource, %{updated_at: DateTime.utc_now()})
        "deleted" -> create_resource_deleted_event(resource)
        _ -> create_resource_event(resource, event_type)
      end
    end)
    |> Enum.filter(fn {status, _} -> status == :ok end)
    |> Enum.map(fn {:ok, event} -> event end)
  end
end
