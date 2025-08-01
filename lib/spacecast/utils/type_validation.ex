defmodule Spacecast.Utils.TypeValidation do
  @moduledoc """
  Provides utilities for validating types and required fields in LiveView socket assigns.
  """

  require Logger

  @doc """
  Validates a value against a type specification.
  """
  def validate_type(value, type) do
    validators = basic_type_validators()

    if Map.has_key?(validators, type) do
      validators[type].(value)
    else
      case type do
        {:list, item_type} -> validate_list_type(item_type, value)
        {:map, field_types} -> validate_map_type(field_types, value)
        {:one_of, allowed} -> validate_one_of(value, allowed)
        {:union, types} -> validate_union(value, types)
        {:custom, validator} -> validate_custom(value, validator)
        _ -> {:error, "unsupported type specification: #{inspect(type)}"}
      end
    end
  end

  def validate_string(value) when is_binary(value), do: {:ok, value}
  def validate_string(value), do: {:error, "expected string, got: #{inspect(value)}"}

  defp validate_integer(value) when is_integer(value), do: {:ok, value}
  defp validate_integer(value), do: {:error, "expected integer, got: #{inspect(value)}"}

  defp validate_boolean(value) when is_boolean(value), do: {:ok, value}
  defp validate_boolean(value), do: {:error, "expected boolean, got: #{inspect(value)}"}

  defp validate_map(value) when is_map(value), do: {:ok, value}
  defp validate_map(value), do: {:error, "expected map, got: #{inspect(value)}"}

  defp validate_list(value) when is_list(value), do: {:ok, value}
  defp validate_list(value), do: {:error, "expected list, got: #{inspect(value)}"}

  defp validate_atom(value) when is_atom(value), do: {:ok, value}
  defp validate_atom(value), do: {:error, "expected atom, got: #{inspect(value)}"}

  defp validate_function(value) when is_function(value), do: {:ok, value}
  defp validate_function(value), do: {:error, "expected function, got: #{inspect(value)}"}

  defp validate_float(value) when is_float(value), do: {:ok, value}
  defp validate_float(value), do: {:error, "expected float, got: #{inspect(value)}"}

  defp validate_number(value) when is_number(value), do: {:ok, value}
  defp validate_number(value), do: {:error, "expected number, got: #{inspect(value)}"}

  @doc """
  Validates a list against a type specification.
  """
  def validate_list_type(item_type, value) when is_list(value) do
    results = Enum.map(value, &validate_type(&1, item_type))

    if Enum.all?(results, &match?({:ok, _}, &1)) do
      {:ok, value}
    else
      errors = Enum.filter(results, &match?({:error, _}, &1))
      {:error, "list validation failed: #{inspect(errors)}"}
    end
  end

  def validate_list_type(_, value), do: {:error, "expected list, got: #{inspect(value)}"}

  @doc """
  Validates a map against a schema.
  """
  def validate_map_type(field_types, value) when is_map(value) do
    results =
      Enum.map(field_types, fn {field, type} ->
        case Map.fetch(value, field) do
          {:ok, field_value} -> {field, validate_type(field_value, type)}
          :error -> {field, {:error, "missing required field"}}
        end
      end)

    if Enum.all?(results, fn {_, result} -> match?({:ok, _}, result) end) do
      {:ok, value}
    else
      errors = Enum.filter(results, fn {_, result} -> match?({:error, _}, result) end)
      {:error, "schema validation failed: #{inspect(errors)}"}
    end
  end

  def validate_map_type(_, value), do: {:error, "expected map, got: #{inspect(value)}"}

  @doc """
  Validates a field against a type specification.
  """
  def validate_field(field, type, value) do
    case validate_type(value, type) do
      {:ok, _} -> {:ok, value}
      {:error, message} -> {:error, "#{field}: #{message}"}
    end
  end

  @doc """
  Ensures all required assigns are present in the socket.
  Returns {:ok, socket} or {:error, missing_keys}
  """
  def validate_required(socket, required_keys) do
    missing = Enum.filter(required_keys, &(Map.get(socket.assigns, &1) == nil))

    if Enum.empty?(missing) do
      {:ok, socket}
    else
      {:error, missing}
    end
  end

  @doc """
  Validates type specifications for socket assigns.
  """
  def validate_type_specs(socket, type_specs) do
    results =
      Enum.map(type_specs, fn {key, type} ->
        case Map.fetch(socket.assigns, key) do
          {:ok, value} -> {key, validate_type(value, type)}
          :error -> {key, {:error, "missing required assign"}}
        end
      end)

    if Enum.all?(results, fn {_, result} -> match?({:ok, _}, result) end) do
      {:ok, socket}
    else
      errors = Enum.filter(results, fn {_, result} -> match?({:error, _}, result) end)
      {:error, "type validation failed: #{inspect(errors)}", socket}
    end
  end

  defp validate_one_of(value, allowed) do
    if Enum.member?(allowed, value),
      do: {:ok, value},
      else: {:error, "expected one of #{inspect(allowed)}, got: #{inspect(value)}"}
  end

  defp validate_union(value, types) do
    results = Enum.map(types, fn type -> {type, validate_type(value, type)} end)

    if Enum.any?(results, fn {_, result} -> match?({:ok, _}, result) end),
      do: {:ok, value},
      else: {:error, "Value matched none of the union types: #{inspect(types)}"}
  end

  defp validate_custom(value, validator) do
    case validator.(value) do
      true -> {:ok, value}
      false -> {:error, "custom validation failed"}
      {:error, message} -> {:error, message}
      other -> {:error, "custom validator returned unexpected result: #{inspect(other)}"}
    end
  end

  defp basic_type_validators do
    %{
      string: &validate_string/1,
      integer: &validate_integer/1,
      boolean: &validate_boolean/1,
      map: &validate_map/1,
      list: &validate_list/1,
      atom: &validate_atom/1,
      function: &validate_function/1,
      float: &validate_float/1,
      number: &validate_number/1
    }
  end

  @doc """
  Validates a value against a type specification with a custom error message.

  ## Parameters
  * `value` - The value to validate
  * `type` - The type specification to validate against
  * `error_message` - Custom error message to use if validation fails

  ## Returns
  * `{:ok, value}` - The value is valid
  * `{:error, error_message}` - The value is invalid
  """
  def type_validation(value, type, error_message) do
    case validate_type(value, type) do
      {:ok, _} -> {:ok, value}
      {:error, _} -> {:error, error_message}
    end
  end
end
