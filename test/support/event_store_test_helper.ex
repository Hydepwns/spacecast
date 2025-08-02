defmodule Spacecast.TestSupport.EventStoreTestHelper do
  @moduledoc """
  Test helper for setting up and managing the mock event store in tests.

  This module provides utilities to:
  - Start the mock event store
  - Reset the event store state between tests
  - Provide helper functions for common event store operations in tests
  - Flexible assertions for event counts and types
  """

  import ExUnit.Assertions
  alias Spacecast.TestSupport.MockEventStore

  @doc """
  Sets up the mock event store for a test.
  This should be called in the setup block of any test that uses event-sourced resources.
  """
  def setup_mock_event_store do
    case MockEventStore.start_link([]) do
      {:ok, _pid} ->
        MockEventStore.reset()
        :ok

      {:error, {:already_started, _pid}} ->
        MockEventStore.reset()
        :ok

      error ->
        error
    end
  end

  @doc """
  Resets the mock event store to a clean state.
  This should be called between tests to ensure isolation.
  """
  def reset_mock_event_store do
    MockEventStore.reset()
  end

  @doc """
  Gets all events from the mock event store.
  Useful for assertions in tests.
  """
  def get_all_events do
    MockEventStore.get_all_events()
  end

  @doc """
  Gets events for a specific resource from the mock event store.

  ## Parameters
  * `resource_type` - The type of resource (as atom)
  * `resource_id` - The ID of the resource

  ## Returns
  * List of events for the resource
  """
  def get_events_for_resource(resource_type, resource_id) do
    MockEventStore.get_events_for_resource(resource_type, resource_id)
  end

  @doc """
  Asserts that a specific event exists in the mock event store.

  ## Parameters
  * `event_type` - The type of event to look for
  * `resource_type` - The type of resource (as atom)
  * `resource_id` - The ID of the resource

  ## Returns
  * `true` if the event exists, raises an assertion error otherwise
  """
  def assert_event_exists(event_type, resource_type, resource_id) do
    {:ok, events} = get_events_for_resource(resource_type, resource_id)

    event_exists? =
      Enum.any?(events, fn event ->
        event.type == event_type
      end)

    unless event_exists? do
      flunk("Expected event of type '#{event_type}' for resource #{resource_type}:#{resource_id}, but it was not found")
    end

    true
  end

  @doc """
  Asserts that no events exist for a specific resource.

  ## Parameters
  * `resource_type` - The type of resource (as atom)
  * `resource_id` - The ID of the resource

  ## Returns
  * `true` if no events exist, raises an assertion error otherwise
  """
  def assert_no_events_for_resource(resource_type, resource_id) do
    {:ok, events} = get_events_for_resource(resource_type, resource_id)

    unless Enum.empty?(events) do
      flunk("Expected no events for resource #{resource_type}:#{resource_id}, but found #{length(events)} events")
    end

    true
  end

  @doc """
  Asserts that a specific number of events exist for a resource.

  ## Parameters
  * `expected_count` - The expected number of events
  * `resource_type` - The type of resource (as atom)
  * `resource_id` - The ID of the resource

  ## Returns
  * `true` if the count matches, raises an assertion error otherwise
  """
  def assert_event_count(expected_count, resource_type, resource_id) do
    {:ok, events} = get_events_for_resource(resource_type, resource_id)
    actual_count = length(events)

    unless actual_count == expected_count do
      flunk("Expected #{expected_count} events for resource #{resource_type}:#{resource_id}, but found #{actual_count}")
    end

    true
  end

  @doc """
  Asserts that at least a minimum number of events exist for a resource.
  This is useful for flexible testing when extra events may be generated.

  ## Parameters
  * `minimum_count` - The minimum number of events expected
  * `resource_type` - The type of resource (as atom)
  * `resource_id` - The ID of the resource

  ## Returns
  * `true` if the count is at least the minimum, raises an assertion error otherwise
  """
  def assert_minimum_event_count(minimum_count, resource_type, resource_id) do
    {:ok, events} = get_events_for_resource(resource_type, resource_id)
    actual_count = length(events)

    unless actual_count >= minimum_count do
      flunk(
        "Expected at least #{minimum_count} events for resource #{resource_type}:#{resource_id}, but found #{actual_count}"
      )
    end

    true
  end

  @doc """
  Asserts that events of a specific type exist for a resource.

  ## Parameters
  * `event_type` - The type of event to look for
  * `resource_type` - The type of resource (as atom)
  * `resource_id` - The ID of the resource
  * `expected_count` - Optional expected count (default: at least 1)

  ## Returns
  * `true` if the events exist, raises an assertion error otherwise
  """
  def assert_events_of_type(event_type, resource_type, resource_id, expected_count \\ 1) do
    {:ok, events} = get_events_for_resource(resource_type, resource_id)

    matching_events =
      Enum.filter(events, fn event ->
        event.type == event_type
      end)

    actual_count = length(matching_events)

    if is_integer(expected_count) do
      unless actual_count == expected_count do
        flunk(
          "Expected #{expected_count} events of type '#{event_type}' for resource #{resource_type}:#{resource_id}, but found #{actual_count}"
        )
      end
    else
      unless actual_count >= 1 do
        flunk(
          "Expected at least 1 event of type '#{event_type}' for resource #{resource_type}:#{resource_id}, but found #{actual_count}"
        )
      end
    end

    true
  end

  @doc """
  Asserts that events are in the expected order for a resource.

  ## Parameters
  * `expected_types` - List of expected event types in order
  * `resource_type` - The type of resource (as atom)
  * `resource_id` - The ID of the resource

  ## Returns
  * `true` if events are in order, raises an assertion error otherwise
  """
  def assert_event_order(expected_types, resource_type, resource_id) do
    {:ok, events} = get_events_for_resource(resource_type, resource_id)
    actual_types = Enum.map(events, & &1.type)

    # Check if expected types are a subsequence of actual types
    has_order =
      Enum.reduce_while(expected_types, actual_types, fn expected_type, remaining_types ->
        case Enum.find_index(remaining_types, &(&1 == expected_type)) do
          nil -> {:halt, false}
          index -> {:cont, Enum.drop(remaining_types, index + 1)}
        end
      end)

    unless has_order do
      flunk(
        "Expected events in order #{inspect(expected_types)} for resource #{resource_type}:#{resource_id}, but got #{inspect(actual_types)}"
      )
    end

    true
  end

  @doc """
  Gets events filtered by criteria (supports both maps and keyword lists).

  ## Parameters
  * `criteria` - Map or keyword list of filtering criteria

  ## Returns
  * List of events matching the criteria
  """
  def get_events_with_criteria(criteria) do
    MockEventStore.get_events(criteria)
  end
end
