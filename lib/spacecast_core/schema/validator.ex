defmodule Hydepwns.Schema.Validator do
  @moduledoc """
  Provides validation functions for schema types.
  """

  @spec validate_type({:list, any()} | {:map, any()}, any()) :: [String.t()]
  def validate_type({:list, item_type}, value) when is_list(value) do
    validate_list_type(item_type, value)
  end

  @spec validate_type({:map, any()}, any()) :: [String.t()]
  def validate_type({:map, field_types}, value) when is_map(value) do
    validate_map_type(field_types, value)
  end

  # Helper function for list validation
  @spec validate_list_type(any(), any()) :: [String.t()]
  defp validate_list_type(item_type, value) do
    errors =
      Enum.with_index(value)
      |> Enum.flat_map(fn {item, index} ->
        case validate_type(item_type, item) do
          [] -> []
          item_errors -> Enum.map(item_errors, &"at index #{index}: #{&1}")
        end
      end)

    if Enum.empty?(errors), do: [], else: errors
  end

  # Helper function for map validation
  @spec validate_map_type(any(), any()) :: [String.t()]
  defp validate_map_type(field_types, value) do
    errors =
      Enum.flat_map(field_types, fn {field, type} ->
        validate_field(field, type, value)
      end)

    if Enum.empty?(errors), do: [], else: errors
  end

  # Helper function for field validation
  @spec validate_field(String.t(), any(), any()) :: [String.t()]
  defp validate_field(field, type, value) do
    case Map.fetch(value, field) do
      {:ok, field_value} ->
        case validate_type(type, field_value) do
          [] -> []
          field_errors -> Enum.map(field_errors, &"field '#{field}': #{&1}")
        end

      :error ->
        ["field '#{field}' is required"]
    end
  end
end
