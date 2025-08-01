defmodule Spacecast.Events.Core.TransactionalEventStore do
  @moduledoc """
  Provides transactional guarantees for resource changes and events.

  This module ensures that resource changes and their corresponding events
  are stored atomically in a transaction. If any part of the transaction fails,
  the entire operation is rolled back.

  Key features:
  - Transaction-based event and change storage
  - Automatic rollback on failure
  - Compensating events for failed operations
  - Correlation tracking across transactions
  """

  require Logger
  alias Spacecast.Repo
  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.Core.EventStore

  @doc """
  Executes a function in a transaction, storing events on success.

  The given function should return either:
  - `{:ok, result, events}` - Transaction successful with result and events to store
  - `{:error, reason}` - Transaction failed with a reason

  ## Parameters
  * `fun` - The function to execute in the transaction

  ## Returns
  * `{:ok, result, events}` - Transaction was successful
  * `{:error, reason}` - Transaction failed
  """
  @spec transaction_with_events((-> {:ok, any(), [map()]} | {:error, any()})) ::
          {:ok, any(), [map()]} | {:error, any()}
  def transaction_with_events(fun) when is_function(fun, 0) do
    Repo.transaction(fn ->
      case fun.() do
        {:ok, result, events} when is_list(events) ->
          # Store all events within the transaction
          stored_events =
            Enum.map(events, fn event ->
              {:ok, stored_event} = EventStore.store_event(event, %{transaction: true})
              stored_event
            end)

          {result, stored_events}

        {:error, reason} ->
          # Roll back the transaction
          Repo.rollback(reason)
      end
    end)
    |> case do
      {:ok, {result, events}} ->
        # Publish events after successful transaction
        Enum.each(events, fn event ->
          Spacecast.Events.EventBus.publish(event, store: false)
        end)

        {:ok, result, events}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Executes a resource change function with event generation and transaction safety.

  ## Parameters
  * `resource_module` - The resource module
  * `id` - The resource ID
  * `change_fn` - Function that implements the change and returns events
  * `opts` - Additional options for the transaction

  ## Returns
  * `{:ok, resource, events}` - The resource was updated and events stored
  * `{:error, reason}` - The update failed
  """
  @spec change_resource_with_events(
          module(),
          any(),
          (-> {:ok, any(), [map()]} | {:error, any()}),
          Keyword.t()
        ) :: {:ok, any(), [map()]} | {:error, any()}
  def change_resource_with_events(resource_module, id, change_fn, opts \\ []) do
    transaction_with_events(fn ->
      result = change_fn.()

      case result do
        {:ok, resource, events} ->
          correlation_id = Keyword.get(opts, :correlation_id, Ecto.UUID.generate())

          # Enhance events with metadata
          events =
            Enum.map(events, fn event ->
              Map.merge(event, %{
                correlation_id: correlation_id,
                resource_type: resource_module.resource_type(),
                resource_id: id
              })
            end)

          {:ok, resource, events}

        {:error, reason} ->
          {:error, reason}
      end
    end)
  end

  @doc """
  Compensating action for a failed operation with events.

  Creates and stores compensating events for a failed operation.

  ## Parameters
  * `original_events` - The events that were attempted in the failed operation
  * `error_reason` - The reason for the failure
  * `user_id` - The ID of the user who initiated the operation

  ## Returns
  * `{:ok, compensating_events}` - Compensating events were stored
  * `{:error, reason}` - Failed to store compensating events
  """
  @spec compensate_for_failure([map()], any(), any()) :: {:ok, any(), [map()]} | {:error, any()}
  def compensate_for_failure(original_events, error_reason, user_id \\ nil) do
    # Create compensating events for each original event
    compensating_events =
      Enum.map(original_events, fn event ->
        %Event{
          type: "#{event.type}.compensated",
          resource_type: event.resource_type,
          resource_id: event.resource_id,
          data: %{
            original_event_type: event.type,
            error_reason: inspect(error_reason)
          },
          metadata: %{
            compensation_timestamp: DateTime.utc_now(),
            user_id: user_id,
            original_event_data: event.data
          },
          correlation_id: event.correlation_id || Ecto.UUID.generate(),
          # Use a causation ID if available, otherwise generate one
          causation_id: event.id || Ecto.UUID.generate()
        }
      end)

    # Store the compensating events
    transaction_with_events(fn ->
      {:ok, :compensated, compensating_events}
    end)
  end

  @doc """
  Bulk updates multiple resources in a single transaction with event generation.

  ## Parameters
  * `resource_changes` - List of {resource_module, id, change_fn} tuples
  * `opts` - Additional options for the transaction

  ## Returns
  * `{:ok, results, events}` - All resources were updated and events stored
  * `{:error, reason}` - The bulk update failed
  """
  @spec bulk_change_resources(
          [{module(), any(), (-> {:ok, any(), [map()]} | {:error, any()})}],
          Keyword.t()
        ) :: {:ok, [any()], [map()]} | {:error, any()}
  def bulk_change_resources(resource_changes, opts \\ []) do
    # Generate a single correlation ID for the entire bulk operation
    correlation_id = Keyword.get(opts, :correlation_id, Ecto.UUID.generate())

    transaction_with_events(fn ->
      {results, all_events} =
        Enum.reduce_while(resource_changes, {[], []}, fn {resource_module, id, change_fn},
                                                         {results_acc, events_acc} ->
          case change_fn.() do
            {:ok, resource, events} ->
              # Enhance events with metadata
              enhanced_events =
                Enum.map(events, fn event ->
                  Map.merge(event, %{
                    correlation_id: correlation_id,
                    resource_type: resource_module.resource_type(),
                    resource_id: id
                  })
                end)

              # Continue to the next resource change
              {:cont, {[{:ok, resource} | results_acc], enhanced_events ++ events_acc}}

            {:error, reason} ->
              # Stop and roll back the transaction
              {:halt, {{:error, reason}, []}}
          end
        end)

      case results do
        {:error, reason} ->
          {:error, reason}

        _ ->
          {:ok, Enum.reverse(results), all_events}
      end
    end)
  end

  @doc """
  Aggregates events from multiple resources for cross-resource operations.

  ## Parameters
  * `resource_events` - Map of resource_type -> resource_id -> events
  * `correlation_id` - Optional correlation ID for the aggregated events

  ## Returns
  * `{:ok, aggregated_events}` - Events were aggregated
  """
  @spec aggregate_resource_events(%{any() => %{any() => [map()]}}, any()) :: {:ok, [map()]}
  def aggregate_resource_events(resource_events, correlation_id \\ nil) do
    correlation_id = correlation_id || Ecto.UUID.generate()

    # Flatten the nested map structure into a list of events
    aggregated_events =
      resource_events
      |> Enum.flat_map(fn {resource_type, resource_ids} ->
        Enum.flat_map(resource_ids, fn {resource_id, events} ->
          Enum.map(events, fn event ->
            Map.merge(event, %{
              correlation_id: correlation_id,
              resource_type: resource_type,
              resource_id: resource_id
            })
          end)
        end)
      end)

    {:ok, aggregated_events}
  end
end
