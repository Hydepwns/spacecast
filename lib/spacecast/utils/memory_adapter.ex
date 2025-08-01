defmodule Spacecast.Utils.MemoryAdapter do
  @moduledoc """
  Adapter for in-memory data that implements the ResourceAdapter behavior.

  This adapter is useful for testing and scenarios where persistence
  is not required or is handled outside the normal database flow.

  ## Usage

  ```elixir
  defmodule MyApp.UserResource do
    use Spacecast.Utils.LiveViewResource
    
    # Use the MemoryAdapter with an in-memory schema
    adapter Spacecast.Utils.MemoryAdapter, schema: %{
      fields: [:id, :name, :email],
      required: [:id, :name],
      types: %{id: :string, name: :string, email: :string}
    }
  end
  ```
  """

  @behaviour Spacecast.Utils.ResourceAdapter

  alias Spacecast.Utils.ResourceAdapter

  # In-memory store for data
  @store_name :memory_adapter_store

  @doc """
  Initializes the in-memory store.

  This function should be called before using the adapter
  to ensure the store is properly initialized.
  """
  def init do
    :ets.new(@store_name, [:set, :public, :named_table])
  end

  @doc """
  Extracts attributes from an in-memory schema.

  This function extracts attribute information from a map
  that defines fields, required fields, and types.
  """
  @spec extract_attributes(map()) :: [map()]
  def extract_attributes(schema) when is_map(schema) do
    fields = Map.get(schema, :fields, [])
    required = Map.get(schema, :required, [])
    types = Map.get(schema, :types, %{})

    Enum.map(fields, fn field ->
      %{
        name: field,
        type: Map.get(types, field, :any),
        required: Enum.member?(required, field),
        default: nil,
        nested_attributes: nil
      }
    end)
  end

  @doc """
  Generates type specifications from an in-memory schema.

  This function extracts type information from a map that
  defines fields and their types.
  """
  @impl ResourceAdapter
  def generate_type_specs(schema) when is_map(schema) do
    Map.get(schema, :types, %{})
  end

  @doc """
  Validates a map of values against an in-memory schema.

  This function validates that all required fields are present
  and that the values match their expected types.
  """
  @impl ResourceAdapter
  def validate(schema, values) when is_map(schema) and is_map(values) do
    # Extract schema information
    _fields = Map.get(schema, :fields, [])
    required = Map.get(schema, :required, [])
    types = Map.get(schema, :types, %{})

    # Check required fields
    case validate_required_fields(required, values) do
      {:error, message} -> {:error, message}
      :ok -> validate_type_fields(types, values)
    end
  end

  defp validate_required_fields(required, values) do
    missing =
      Enum.filter(required, fn field ->
        is_nil(Map.get(values, field)) && is_nil(Map.get(values, to_string(field)))
      end)

    if length(missing) > 0 do
      missing_fields = Enum.map_join(missing, ", ", &to_string/1)
      {:error, "Missing required fields: #{missing_fields}"}
    else
      :ok
    end
  end

  defp validate_type_fields(types, values) do
    type_errors =
      Enum.filter(Map.keys(values), fn field ->
        atom_field = to_atom_key(field)
        Map.has_key?(types, atom_field) && !matches_field_type(values, field, types, atom_field)
      end)

    if length(type_errors) > 0 do
      type_error_fields = Enum.map_join(type_errors, ", ", &to_string/1)
      {:error, "Type mismatch for fields: #{type_error_fields}"}
    else
      {:ok, values}
    end
  end

  defp matches_field_type(values, field, types, atom_field) do
    expected_type = Map.get(types, atom_field)
    value = Map.get(values, field)
    matches_type?(value, expected_type)
  end

  @doc """
  Loads data from the in-memory store based on a primary key.

  This function retrieves data from the in-memory store using
  the provided key.
  """
  @impl ResourceAdapter
  def load(_schema, id) do
    ensure_store_exists()

    case :ets.lookup(@store_name, id) do
      [{^id, data}] -> {:ok, data}
      [] -> {:error, "Record not found"}
    end
  end

  @doc """
  Saves data to the in-memory store.

  This function persists data to the in-memory store using
  the provided key.
  """
  @impl ResourceAdapter
  def save(schema, values) when is_map(schema) and is_map(values) do
    ensure_store_exists()

    case validate(schema, values) do
      {:ok, validated_values} ->
        # Extract the ID field (assuming it's called :id)
        id_field = Map.get(schema, :id_field, :id)
        id = Map.get(validated_values, id_field) || Map.get(validated_values, to_string(id_field))

        if is_nil(id) do
          {:error, "Missing ID field: #{id_field}"}
        else
          # Store the data
          :ets.insert(@store_name, {id, validated_values})
          {:ok, validated_values}
        end

      {:error, _} = error ->
        error
    end
  end

  @doc """
  Clears all data from the in-memory store.

  This function is useful for testing to ensure a clean state.
  """
  def clear do
    ensure_store_exists()
    :ets.delete_all_objects(@store_name)
    :ok
  end

  # Helper function to ensure the store exists
  defp ensure_store_exists do
    unless :ets.whereis(@store_name) != :undefined do
      init()
    end
  end

  # Helper function to check if a value matches a type
  defp matches_type?(value, :string) when is_binary(value), do: true
  defp matches_type?(value, :integer) when is_integer(value), do: true
  defp matches_type?(value, :float) when is_float(value), do: true
  defp matches_type?(value, :boolean) when is_boolean(value), do: true
  defp matches_type?(value, :map) when is_map(value), do: true
  defp matches_type?(value, {:list, _type}) when is_list(value), do: true
  defp matches_type?(_value, :any), do: true
  defp matches_type?(_value, _type), do: false

  # Helper function to convert a string key to an atom
  defp to_atom_key(key) when is_atom(key), do: key

  defp to_atom_key(key) when is_binary(key) do
    try do
      String.to_existing_atom(key)
    rescue
      ArgumentError -> key
    end
  end
end
