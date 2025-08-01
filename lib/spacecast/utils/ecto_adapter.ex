defmodule Spacecast.Utils.EctoAdapter do
  @moduledoc """
  Adapter for Ecto schemas that implements the ResourceAdapter behavior.

  This adapter allows LiveViewResource to work with Ecto schemas,
  providing validation, loading, and saving functionality based on
  the schema's field definitions, validations, and associations.

  ## Usage

  ```elixir
  defmodule MyApp.UserResource do
    use Spacecast.Utils.LiveViewResource
    
    # Use the EctoAdapter with the User schema
    adapter Spacecast.Utils.EctoAdapter, schema: MyApp.User
  end
  ```
  """

  @behaviour Spacecast.Utils.ResourceAdapter

  alias Spacecast.Utils.ResourceAdapter

  @doc """
  Extracts attributes from an Ecto schema.

  This function uses Ecto's introspection capabilities to extract
  field information from a schema, converting it to a format
  compatible with LiveViewResource.
  """
  @spec extract_attributes(module()) :: [map()]
  def extract_attributes(schema) when is_atom(schema) do
    # Get field information from the Ecto schema
    fields = schema.__schema__(:fields)
    types = Map.new(fields, fn field -> {field, schema.__schema__(:type, field)} end)

    # Get association information from the Ecto schema
    associations = schema.__schema__(:associations)

    assoc_info =
      Map.new(associations, fn assoc ->
        {assoc, schema.__schema__(:association, assoc)}
      end)

    # Convert the fields to LiveViewResource attribute definitions
    field_attrs =
      Enum.map(fields, fn field ->
        ecto_type = Map.get(types, field)

        # Convert Ecto type to LiveViewResource type
        lv_type = ecto_to_lv_type(ecto_type)

        # Check if the field has any constraints
        required = !Enum.member?(schema.__schema__(:fields_with_nulls), field)

        %{
          name: field,
          type: lv_type,
          required: required,
          # We don't have easy access to defaults from Ecto schemas
          default: nil,
          nested_attributes: nil
        }
      end)

    # Convert the associations to LiveViewResource relationship definitions
    relationship_attrs =
      Enum.map(associations, fn assoc ->
        assoc_data = assoc_info[assoc]

        %{
          name: assoc,
          type: ecto_assoc_to_lv_relationship_type(assoc_data),
          resource: assoc_data.queryable,
          foreign_key: assoc_data.owner_key,
          cardinality: ecto_assoc_to_lv_cardinality(assoc_data)
        }
      end)

    # Combine field and association attributes
    field_attrs ++ relationship_attrs
  end

  @doc """
  Generates type specifications from an Ecto schema.

  This function converts Ecto field types to LiveViewResource
  type specifications that can be used for validation.
  """
  @impl ResourceAdapter
  def generate_type_specs(schema) when is_atom(schema) do
    fields = schema.__schema__(:fields)
    types = Map.new(fields, fn field -> {field, schema.__schema__(:type, field)} end)

    types
    |> Enum.map(fn {field, ecto_type} -> {field, ecto_to_lv_type(ecto_type)} end)
    |> Map.new()
  end

  @doc """
  Validates a map of values against an Ecto schema.

  This function uses Ecto's changeset functionality to validate
  a map of values against an Ecto schema's constraints.
  """
  @impl ResourceAdapter
  def validate(schema, values) when is_atom(schema) and is_map(values) do
    # Try to ensure we have atom keys
    values = keys_to_atoms(values)

    # Create a struct from the schema
    struct = struct(schema)

    # Get required fields from the schema
    required_fields = schema.__schema__(:fields) -- schema.__schema__(:fields_with_nulls)

    # Create a changeset
    changeset =
      struct
      |> Ecto.Changeset.cast(values, schema.__schema__(:fields))
      |> Ecto.Changeset.validate_required(required_fields)

    # Check if the changeset is valid
    if changeset.valid? do
      # Extract the validated changes
      validated_values = Ecto.Changeset.apply_changes(changeset)
      {:ok, validated_values}
    else
      # Extract error messages
      errors = format_changeset_errors(changeset)
      {:error, errors}
    end
  end

  @doc """
  Loads data from an Ecto schema based on a primary key.

  This function uses the provided repo to load data from
  the database based on the primary key value.
  """
  @impl ResourceAdapter
  def load(schema, id) when is_atom(schema) do
    # We need a repo to actually load the data
    # This implementation assumes a repo is configured
    repo = get_repo()

    if repo do
      # Try to load the data
      case Spacecast.RepoHelper.get(schema, id) do
        nil -> {:error, "Record not found"}
        data -> {:ok, data}
      end
    else
      {:error, "No repo configured for EctoAdapter"}
    end
  end

  @doc """
  Saves data to an Ecto schema.

  This function uses the provided repo to save data to
  the database based on the primary key value.
  """
  @impl ResourceAdapter
  def save(schema, values) when is_atom(schema) and is_map(values) do
    # We need a repo to actually save the data
    # This implementation assumes a repo is configured
    repo = get_repo()

    if repo do
      # Try to ensure we have atom keys
      values = keys_to_atoms(values)

      # Create a struct from the schema
      struct = struct(schema)

      # Get required fields from the schema
      required_fields = schema.__schema__(:fields) -- schema.__schema__(:fields_with_nulls)

      # Create a changeset
      changeset =
        struct
        |> Ecto.Changeset.cast(values, schema.__schema__(:fields))
        |> Ecto.Changeset.validate_required(required_fields)

      # Try to insert/update the data
      case repo.insert_or_update(changeset) do
        {:ok, data} -> {:ok, data}
        {:error, changeset} -> {:error, format_changeset_errors(changeset)}
      end
    else
      {:error, "No repo configured for EctoAdapter"}
    end
  end

  # Helper function to get the configured repo
  defp get_repo do
    Application.get_env(:spacecast, :ecto_adapter_repo)
  end

  # Helper function to convert Ecto association types to LiveViewResource relationship types
  defp ecto_assoc_to_lv_relationship_type(%{cardinality: :one, owner: true}), do: :belongs_to
  defp ecto_assoc_to_lv_relationship_type(%{cardinality: :many, owner: true}), do: :has_many
  defp ecto_assoc_to_lv_relationship_type(%{cardinality: :one, owner: false}), do: :has_one
  defp ecto_assoc_to_lv_relationship_type(%{cardinality: :many, owner: false}), do: :has_many

  # Helper function to convert Ecto association cardinality to LiveViewResource cardinality
  defp ecto_assoc_to_lv_cardinality(%{cardinality: :one}), do: :one
  defp ecto_assoc_to_lv_cardinality(%{cardinality: :many}), do: :many

  # Helper function to convert Ecto types to LiveViewResource types
  defp ecto_to_lv_type(:string), do: :string
  defp ecto_to_lv_type(:binary), do: :string
  defp ecto_to_lv_type(:integer), do: :integer
  defp ecto_to_lv_type(:float), do: :float
  defp ecto_to_lv_type(:boolean), do: :boolean
  defp ecto_to_lv_type(:map), do: :map
  defp ecto_to_lv_type(:array), do: {:list, :any}
  defp ecto_to_lv_type({:array, type}), do: {:list, ecto_to_lv_type(type)}
  defp ecto_to_lv_type(:date), do: :date
  defp ecto_to_lv_type(:time), do: :time
  defp ecto_to_lv_type(:naive_datetime), do: :datetime
  defp ecto_to_lv_type(:utc_datetime), do: :datetime
  defp ecto_to_lv_type(_), do: :any

  # Helper function to convert string keys to atom keys
  defp keys_to_atoms(map) when is_map(map) do
    map
    |> Enum.map(fn {k, v} -> {to_atom_key(k), v} end)
    |> Map.new()
  end

  defp to_atom_key(key) when is_atom(key), do: key
  defp to_atom_key(key) when is_binary(key), do: String.to_existing_atom(key)

  # Helper function to format changeset errors
  defp format_changeset_errors(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
    |> Enum.map_join("; ", fn {k, v} -> "#{k}: #{v}" end)
  end
end
