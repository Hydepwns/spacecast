defmodule Spacecast.Resources.TestFramework do
  require Logger

  @moduledoc """
  Testing framework for event-sourced resources.

  This module provides utilities for testing event-sourced resources, including:
  - DSL for writing expressive tests for event-sourced resources
  - Test helpers for verifying resource behavior
  - Event sequence verification
  - State evolution verification
  - Property-based testing for resources
  - Randomized event generation for testing
  """

  @doc """
  Creates a test context for an event-sourced resource.

  ## Parameters
  * `resource_module` - The resource module to test
  * `opts` - Test options

  ## Returns
  * Test context for the resource
  """
  def resource_test(resource_module, opts \\ []) do
    resource_id = Keyword.get(opts, :resource_id, "test-#{Ecto.UUID.generate()}")

    %{
      resource_module: resource_module,
      resource_id: resource_id,
      resource_type: resource_module.resource_type(),
      initial_state: resource_module.initial_state(resource_id),
      current_state: resource_module.initial_state(resource_id),
      events: [],
      errors: []
    }
  end

  @doc """
  Executes a command against the resource.

  ## Parameters
  * `context` - Test context
  * `command` - Function that takes the current state and returns `{:ok, new_state, events}` or `{:error, reason}`
  * `expected_events` - Optional list of expected events

  ## Returns
  * Updated test context
  """
  def when_command(context, command, expected_events \\ []) do
    # Execute the command
    result = command.(context.current_state)

    case result do
      {:ok, new_state, generated_events} ->
        # Verify expected events
        events_match = verify_events(generated_events, expected_events, context)

        # Update context
        %{
          context
          | current_state: new_state,
            events: context.events ++ generated_events,
            errors:
              if(events_match,
                do: context.errors,
                else: ["Events didn't match expectations" | context.errors]
              )
        }

      {:error, reason} ->
        # Record error
        %{
          context
          | status: :error,
            errors: ["Command failed: #{inspect(reason)}" | context.errors]
        }
    end
  end

  @doc """
  Applies a sequence of events to the resource.

  ## Parameters
  * `context` - Test context
  * `events` - List of events to apply

  ## Returns
  * Updated test context
  """
  def given_events(context, events) do
    # Apply events to get new state
    new_state =
      Enum.reduce(events, context.current_state, fn event, state ->
        context.resource_module.apply(state, event)
      end)

    # Update context
    %{
      context
      | current_state: new_state,
        events: context.events ++ events
    }
  end

  @doc """
  Verifies the resource state matches expectations.

  ## Parameters
  * `context` - Test context
  * `validations` - Function that takes the current state and returns :ok or {:error, reason}

  ## Returns
  * Updated test context
  """
  def then_state(context, validations) do
    case validations.(context.current_state) do
      :ok ->
        # State is valid
        context

      {:error, reason} ->
        Logger.error("Failed to update state: #{inspect(reason)}")
        {:error, reason}

      other ->
        # Unknown validation result
        %{
          context
          | errors: ["State validation error: #{inspect(other)}" | context.errors]
        }
    end
  end

  @doc """
  Verifies specific fields in the resource state match expected values.

  ## Parameters
  * `context` - Test context
  * `expected_fields` - Map of field names to expected values

  ## Returns
  * Updated test context
  """
  def then_fields(context, expected_fields) when is_map(expected_fields) do
    errors =
      Enum.reduce(expected_fields, [], fn {field, expected_value}, acc_errors ->
        current_value = Map.get(context.current_state, field)

        if current_value == expected_value do
          acc_errors
        else
          [
            "Field '#{field}' mismatch. Expected: #{inspect(expected_value)}, Got: #{inspect(current_value)}"
            | acc_errors
          ]
        end
      end)

    if Enum.empty?(errors) do
      context
    else
      # Add errors in the order they were found (reversed from Enum.reduce accumulator)
      %{
        context
        | errors: Enum.reverse(errors) ++ context.errors
      }
    end
  end

  @doc """
  Verifies that the test context has no errors.

  ## Parameters
  * `context` - Test context

  ## Returns
  * :ok or {:error, errors}
  """
  def verify(context) do
    if Enum.empty?(context.errors) do
      :ok
    else
      # Return errors in chronological order (first error first)
      errors = Enum.reverse(context.errors)

      %{
        context
        | errors: errors ++ context.errors
      }
    end
  end

  @doc """
  Runs a scenario test with a sequence of commands.

  ## Parameters
  * `resource_module` - The resource module to test
  * `scenario` - Function that creates the scenario
  * `opts` - Test options

  ## Returns
  * Test result
  """
  def scenario_test(resource_module, scenario, opts \\ []) do
    # Create initial context
    context = resource_test(resource_module, opts)

    # Run the scenario
    final_context = scenario.(context)

    # Check for errors
    if final_context.errors == [] do
      {:ok, final_context}
    else
      {:error, Enum.reverse(final_context.errors), final_context}
    end
  end

  @doc """
  Generates random events for property-based testing.

  ## Parameters
  * `resource_module` - The resource module
  * `count` - Number of events to generate
  * `opts` - Generation options

  ## Returns
  * List of random events
  """
  def generate_random_events(resource_module, count, opts \\ []) do
    resource_type = resource_module.resource_type()
    resource_id = Keyword.get(opts, :resource_id, "test-#{Ecto.UUID.generate()}")

    # Try to get event types from the module
    event_types =
      try do
        resource_module.event_types()
      rescue
        _ -> Keyword.get(opts, :event_types, ["created", "updated", "deleted"])
      end

    # Generate random events
    Enum.map(1..count, fn i ->
      # Pick a random event type
      event_type = Enum.random(event_types)

      # Generate random data based on event type
      data = generate_random_data(resource_module, event_type, opts)

      # Create the event
      %Spacecast.Events.Core.Event{
        type: event_type,
        resource_type: resource_type,
        resource_id: resource_id,
        data: data,
        metadata: %{
          random_generated: true,
          seed: Keyword.get(opts, :seed, :os.system_time(:millisecond)),
          sequence: i
        },
        correlation_id: Keyword.get(opts, :correlation_id, Ecto.UUID.generate())
      }
    end)
  end

  @doc """
  Runs a property-based test on a resource.

  ## Parameters
  * `resource_module` - The resource module
  * `property` - Function that checks a property
  * `opts` - Test options

  ## Returns
  * Test result
  """
  def property_test(resource_module, property, opts \\ []) do
    # Default options
    iterations = Keyword.get(opts, :iterations, 100)
    events_per_iteration = Keyword.get(opts, :events_per_iteration, 10)
    max_failures = Keyword.get(opts, :max_failures, 5)

    # Run iterations
    Enum.reduce_while(1..iterations, %{failures: [], successes: 0}, fn i, acc ->
      # Generate random events for this iteration
      seed = :os.system_time(:millisecond) + i

      events =
        generate_random_events(
          resource_module,
          events_per_iteration,
          Keyword.put(opts, :seed, seed)
        )

      # Create test context
      context = resource_test(resource_module, opts)

      # Apply events
      context_with_events = given_events(context, events)

      # Check property
      case property.(context_with_events) do
        %{errors: []} ->
          # Property holds, continue
          {:cont, %{acc | successes: acc.successes + 1}}

        context_with_errors ->
          # Property failed
          failure = %{
            iteration: i,
            seed: seed,
            events: events,
            errors: context_with_errors.errors
          }

          failures = [failure | acc.failures]

          if length(failures) >= max_failures do
            # Too many failures, stop
            {:halt, %{acc | failures: failures}}
          else
            # Continue testing
            {:cont, %{acc | failures: failures}}
          end
      end
    end)
    |> case do
      %{failures: [], successes: successes} ->
        {:ok, %{successes: successes}}

      %{failures: failures, successes: successes} ->
        {:error, %{failures: failures, successes: successes}}
    end
  end

  @doc """
  Creates a test helper for a specific resource command.

  ## Parameters
  * `name` - Name for the helper
  * `command_fn` - Function that executes the command
  * `expected_events_fn` - Function that generates expected events

  ## Returns
  * Helper function
  """
  def command_helper(name, command_fn, expected_events_fn \\ nil) do
    helper_fn = fn context, args ->
      # Get expected events if provided
      expected_events = if expected_events_fn, do: expected_events_fn.(args, context), else: []

      # Create command function
      command = fn state -> command_fn.(state, args) end

      # Execute the command
      updated_context = when_command(context, command, expected_events)

      # Add helper name to context for tracing
      Map.update(updated_context, :command_trace, [name], &[name | &1])
    end

    {name, helper_fn}
  end

  @doc """
  Creates a test suite for a resource.

  ## Parameters
  * `resource_module` - The resource module
  * `test_cases` - Map of test case name -> test function
  * `opts` - Test options

  ## Returns
  * Test suite results
  """
  def test_suite(resource_module, test_cases, opts \\ []) do
    # Run each test case
    test_results =
      Enum.map(test_cases, fn {name, test_fn} ->
        # Run the test
        result = scenario_test(resource_module, test_fn, opts)

        # Format the result
        {name, result}
      end)

    # Summarize results
    passed = Enum.count(test_results, fn {_, result} -> match?({:ok, _}, result) end)
    failed = length(test_results) - passed

    %{
      resource_type: resource_module.resource_type(),
      total_tests: length(test_results),
      passed: passed,
      failed: failed,
      results: test_results
    }
  end

  # Private helper functions

  defp verify_events(generated_events, expected_events, _context) do
    # Check count and content
    Enum.count(generated_events) == Enum.count(expected_events) &&
      Enum.zip(generated_events, expected_events)
      |> Enum.all?(fn {actual, expected} -> event_matches?(actual, expected) end)
  end

  defp event_matches?(
         %Spacecast.Events.Core.Event{} = actual,
         %Spacecast.Events.Core.Event{} = expected
       ) do
    # Compare directly with fallbacks for missing fields
    actual.type == expected.type &&
      actual.resource_id == expected.resource_id &&
      (expected.data == nil || actual.data == expected.data) &&
      (expected.metadata == nil || actual.metadata == expected.metadata)
  end

  defp event_matches?(%Spacecast.Events.Core.Event{} = actual, %{} = expected)
       when is_map(expected) do
    # Compare just the specified fields
    Enum.all?(expected, fn {key, value} ->
      Map.get(actual, key) == value
    end)
  end

  defp event_matches?(%Spacecast.Events.Core.Event{type: type}, type)
       when is_binary(type) or is_atom(type) do
    # Just check the event type
    true
  end

  defp event_matches?(_, _) do
    # No match
    false
  end

  defp generate_random_data(_resource_module, "created", _opts) do
    # Generate data for a creation event
    %{
      name: "Test Resource #{:rand.uniform(1000)}",
      status: Enum.random(["active", "inactive", "pending"]),
      created_at: DateTime.utc_now()
    }
  end

  defp generate_random_data(_resource_module, "updated", _opts) do
    # Generate data for an update event
    %{
      name: "Updated Resource #{:rand.uniform(1000)}",
      status: Enum.random(["active", "inactive", "pending"]),
      updated_at: DateTime.utc_now()
    }
  end

  defp generate_random_data(_resource_module, "deleted", _opts) do
    # Generate data for a deletion event
    %{
      deleted_at: DateTime.utc_now(),
      reason: Enum.random(["user_request", "admin_action", "expired"])
    }
  end

  defp generate_random_data(_resource_module, _event_type, _opts) do
    # Default random data
    %{
      value: :rand.uniform(100),
      timestamp: DateTime.utc_now()
    }
  end
end
