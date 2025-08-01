defmodule Spacecast.Resources.TestEventGenerator do
  @moduledoc """
  Test event generation for resource systems.

  This module provides functionality for generating test events that can be used
  to test resource behavior without affecting production data.
  """

  require Logger
  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.Core.EventStore

  @type resource_module :: module()
  @type resource_id :: any()
  @type event_spec :: {atom(), map()}
  @type generated_event :: %Event{}

  @spec generate_test_events(resource_module(), resource_id(), list(event_spec()), keyword()) ::
          {:ok, list(generated_event())} | {:error, any()}
  @doc """
  Generates test events for a resource.

  This creates events that can be used to test resource behavior
  without affecting production data.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `event_specs` - Specifications for events to generate
  * `opts` - Generation options

  ## Options
  * `:correlation_id` - Correlation ID for the events (default: auto-generated)
  * `:store` - Whether to store events in the event store (default: false)
  * `:timestamp` - Base timestamp for events (default: current time)

  ## Returns
  * `{:ok, events}` - Generated events
  * `{:error, reason}` - Failed to generate events
  """
  def generate_test_events(resource_module, id, event_specs, opts \\ []) do
    resource_type = resource_module.resource_type()
    correlation_id = Keyword.get(opts, :correlation_id, Ecto.UUID.generate())
    base_timestamp = Keyword.get(opts, :timestamp, DateTime.utc_now())

    # Generate events from specifications
    events =
      event_specs
      |> Enum.with_index()
      |> Enum.map(fn {{event_type, data}, index} ->
        # Add timestamp offset for each event
        timestamp = DateTime.add(base_timestamp, index, :second)

        %Event{
          type: event_type,
          resource_type: resource_type,
          resource_id: id,
          data: data,
          metadata: %{
            generated: true,
            test: true,
            generator: "test_event_generator",
            sequence: index
          },
          correlation_id: correlation_id,
          timestamp: timestamp
        }
      end)

    # Store events if requested
    if Keyword.get(opts, :store, false) do
      Enum.each(events, fn event ->
        EventStore.store_event(event)
      end)
    end

    {:ok, events}
  end

  @spec generate_event_sequence(resource_module(), resource_id(), list(atom()), keyword()) ::
          {:ok, list(generated_event())} | {:error, any()}
  @doc """
  Generates a sequence of events with default data.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `event_types` - List of event types to generate
  * `opts` - Generation options

  ## Returns
  * `{:ok, events}` - Generated events
  * `{:error, reason}` - Failed to generate events
  """
  def generate_event_sequence(resource_module, id, event_types, opts \\ []) do
    # Generate default data for each event type
    event_specs =
      Enum.map(event_types, fn event_type ->
        {event_type, generate_default_data(event_type)}
      end)

    generate_test_events(resource_module, id, event_specs, opts)
  end

  @spec generate_random_events(resource_module(), resource_id(), integer(), keyword()) ::
          {:ok, list(generated_event())} | {:error, any()}
  @doc """
  Generates random events for stress testing.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `count` - Number of events to generate
  * `opts` - Generation options

  ## Returns
  * `{:ok, events}` - Generated events
  * `{:error, reason}` - Failed to generate events
  """
  def generate_random_events(resource_module, id, count, opts \\ []) do
    resource_type = resource_module.resource_type()

    # Get available event types
    event_types =
      try do
        resource_module.event_types()
      rescue
        _ ->
          # Fallback to common event types
          [:created, :updated, :deleted, :activated, :deactivated]
      end

    # Generate random event specifications
    event_specs =
      Enum.map(1..count, fn _ ->
        event_type = Enum.random(event_types)
        data = generate_random_data(event_type)
        {event_type, data}
      end)

    generate_test_events(resource_module, id, event_specs, opts)
  end

  @spec generate_scenario_events(resource_module(), resource_id(), atom(), keyword()) ::
          {:ok, list(generated_event())} | {:error, any()}
  @doc """
  Generates events for a specific scenario.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `scenario` - Scenario to generate (:create_update_delete, :lifecycle, :bulk_operations)
  * `opts` - Generation options

  ## Returns
  * `{:ok, events}` - Generated events
  * `{:error, reason}` - Failed to generate events
  """
  def generate_scenario_events(resource_module, id, scenario, opts \\ []) do
    event_specs = get_scenario_specs(scenario)
    generate_test_events(resource_module, id, event_specs, opts)
  end

  @spec validate_event_specs(resource_module(), list(event_spec())) ::
          {:ok, list(event_spec())} | {:error, list(String.t())}
  @doc """
  Validates event specifications against the resource module.

  ## Parameters
  * `resource_module` - The resource module
  * `event_specs` - Event specifications to validate

  ## Returns
  * `{:ok, validated_specs}` - Validated specifications
  * `{:error, errors}` - List of validation errors
  """
  def validate_event_specs(resource_module, event_specs) do
    # Get valid event types
    valid_event_types =
      try do
        resource_module.event_types()
      rescue
        _ -> []
      end

    # Validate each specification
    {valid_specs, errors} =
      Enum.reduce(event_specs, {[], []}, fn {event_type, data}, {valid, errors} ->
        case validate_event_spec(event_type, data, valid_event_types, resource_module) do
          {:ok, validated_spec} ->
            {[validated_spec | valid], errors}

          {:error, error} ->
            {valid, [error | errors]}
        end
      end)

    if Enum.empty?(errors) do
      {:ok, Enum.reverse(valid_specs)}
    else
      {:error, Enum.reverse(errors)}
    end
  end

  # Private helper functions

  defp generate_default_data(event_type) do
    # Generate sensible default data based on event type
    case event_type do
      :created ->
        %{
          id: Ecto.UUID.generate(),
          name: "Test Resource",
          created_at: DateTime.utc_now()
        }

      :updated ->
        %{
          name: "Updated Test Resource",
          updated_at: DateTime.utc_now()
        }

      :deleted ->
        %{
          deleted_at: DateTime.utc_now(),
          reason: "test_cleanup"
        }

      :activated ->
        %{
          activated_at: DateTime.utc_now(),
          activated_by: "test_user"
        }

      :deactivated ->
        %{
          deactivated_at: DateTime.utc_now(),
          deactivated_by: "test_user",
          reason: "test_deactivation"
        }

      _ ->
        %{
          timestamp: DateTime.utc_now(),
          test_data: true
        }
    end
  end

  defp generate_random_data(event_type) do
    # Generate random data for stress testing
    base_data = generate_default_data(event_type)

    # Add some random fields
    random_fields = %{
      random_id: Ecto.UUID.generate(),
      random_value: :rand.uniform(1000),
      random_string: "random_#{:rand.uniform(999999)}",
      random_boolean: :rand.uniform() > 0.5
    }

    Map.merge(base_data, random_fields)
  end

  defp get_scenario_specs(scenario) do
    case scenario do
      :create_update_delete ->
        [
          {:created, generate_default_data(:created)},
          {:updated, generate_default_data(:updated)},
          {:deleted, generate_default_data(:deleted)}
        ]

      :lifecycle ->
        [
          {:created, generate_default_data(:created)},
          {:activated, generate_default_data(:activated)},
          {:updated, generate_default_data(:updated)},
          {:deactivated, generate_default_data(:deactivated)},
          {:deleted, generate_default_data(:deleted)}
        ]

      :bulk_operations ->
        # Generate multiple update events
        Enum.map(1..5, fn i ->
          {:updated, %{
            name: "Bulk Update #{i}",
            updated_at: DateTime.utc_now(),
            bulk_operation: true,
            sequence: i
          }}
        end)

      _ ->
        [
          {:created, generate_default_data(:created)}
        ]
    end
  end

  defp validate_event_spec(event_type, data, valid_event_types, resource_module) do
    cond do
      length(valid_event_types) > 0 and event_type not in valid_event_types ->
        {:error, "Invalid event type: #{event_type}"}

      not is_map(data) ->
        {:error, "Event data must be a map for event type: #{event_type}"}

      true ->
        # Try to validate against schema if available
        try do
          schema = resource_module.event_schema(event_type)
          validate_against_schema(data, schema, event_type)
        rescue
          _ ->
            # No schema available, assume valid
            {:ok, {event_type, data}}
        end
    end
  end

  defp validate_against_schema(data, schema, event_type) do
    # Simple schema validation
    required_fields = Map.get(schema, :required, [])
    missing_fields = Enum.filter(required_fields, &(!Map.has_key?(data, &1)))

    if length(missing_fields) > 0 do
      {:error, "Missing required fields for #{event_type}: #{Enum.join(missing_fields, ", ")}"}
    else
      {:ok, {event_type, data}}
    end
  end
end
