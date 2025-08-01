defmodule Spacecast.Events.Core.EventValidator do
  @moduledoc """
  Validates event structures to ensure they conform to the expected schema.

  This module is responsible for validating event data before it's published
  to the EventBus, ensuring that events have the required fields and
  that those fields have the correct types.
  """

  @doc """
  Validates an event according to the standard event schema.

  ## Parameters

  * `event` - The event to validate

  ## Returns

  * `{:ok, event}` - The event is valid
  * `{:error, reason}` - The event is invalid, with reason
  """
  @spec validate(map()) :: {:ok, map()} | {:error, String.t()}
  def validate(event) when is_map(event) do
    with :ok <- validate_required_fields(event),
         :ok <- validate_field_types(event) do
      {:ok, event}
    end
  end

  def validate(_), do: {:error, "Event must be a map"}

  @doc """
  Returns the standard event schema with required and optional fields.

  ## Returns

  * `%{required: [fields], optional: [fields]}` - The event schema
  """
  @spec schema() :: %{required: [atom()], optional: [atom()]}
  def schema do
    %{
      required: [:type],
      optional: [
        :id,
        :source,
        :timestamp,
        :resource_type,
        :resource_id,
        :correlation_id,
        :causation_id,
        :metadata,
        :data
      ]
    }
  end

  @doc """
  Returns the expected types for event fields.

  ## Returns

  * `%{field => type_spec}` - The field types
  """
  @spec field_types() :: %{atom() => atom()}
  def field_types do
    %{
      id: :string,
      type: :atom,
      source: :string,
      timestamp: :datetime,
      resource_type: :string,
      resource_id: :string,
      correlation_id: :string,
      causation_id: :string,
      metadata: :map,
      data: :map
    }
  end

  # Private functions

  # Validates that all required fields are present
  defp validate_required_fields(event) do
    missing_fields =
      schema().required
      |> Enum.filter(fn field -> not Map.has_key?(event, field) end)

    case missing_fields do
      [] -> :ok
      _ -> {:error, "Missing required fields: #{inspect(missing_fields)}"}
    end
  end

  # Validates the types of fields that are present
  defp validate_field_types(event) do
    invalid_fields =
      event
      |> Map.take(Map.keys(field_types()))
      |> Enum.filter(fn {field, value} -> not valid_type?(field, value) end)
      |> Enum.map(fn {field, _value} -> field end)

    case invalid_fields do
      [] -> :ok
      _ -> {:error, "Invalid field types: #{inspect(invalid_fields)}"}
    end
  end

  # Checks if a value matches the expected type for a field
  defp valid_type?(field, value) do
    case {field_types()[field], value} do
      {:string, v} when is_binary(v) -> true
      {:atom, v} when is_atom(v) -> true
      {:datetime, %DateTime{}} -> true
      {:map, v} when is_map(v) -> true
      {:list, v} when is_list(v) -> true
      # Allow nil values
      {_, nil} -> true
      _ -> false
    end
  end
end
