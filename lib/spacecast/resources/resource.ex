defmodule Spacecast.Resources.Resource do
  @moduledoc """
  Schema and changeset functions for Resources in the application.

  This module defines a generic resource entity that can be categorized, tagged,
  and structured in hierarchical relationships through parent-child associations.
  Resources contain customizable content, metadata and settings as map fields.
  """
  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{
          id: String.t() | nil,
          name: String.t() | nil,
          type: String.t() | nil,
          status: String.t() | nil,
          description: String.t() | nil,
          content: map() | nil,
          metadata: map() | nil,
          settings: map() | nil,
          version: integer() | nil,
          parent_id: String.t() | nil,
          child_ids: [String.t()] | nil,
          tags: [String.t()] | nil,
          categories: [String.t()] | nil,
          created_by: String.t() | nil,
          updated_by: String.t() | nil,
          inserted_at: DateTime.t() | nil,
          updated_at: DateTime.t() | nil
        }
  @derive {Jason.Encoder,
           only: [
             :id,
             :name,
             :type,
             :status,
             :description,
             :content,
             :metadata,
             :settings,
             :version,
             :parent_id,
             :child_ids,
             :tags,
             :categories,
             :created_by,
             :updated_by,
             :inserted_at,
             :updated_at
           ]}
  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "resources" do
    field :name, :string
    field :type, :string
    field :status, :string
    field :description, :string
    field :content, :map, default: %{}
    field :metadata, :map, default: %{}
    field :settings, :map, default: %{}
    field :version, :integer, default: 1
    field :parent_id, :binary_id
    field :child_ids, {:array, :binary_id}
    field :tags, {:array, :string}
    field :categories, {:array, :string}
    field :created_by, :binary_id
    field :updated_by, :binary_id

    timestamps(type: :utc_datetime_usec)
  end

  # Implement String.Chars protocol for Resource struct
  defimpl String.Chars, for: __MODULE__ do
    def to_string(resource) do
      "#{resource.name} (#{resource.id})"
    end
  end

  # Implement Access behavior for Resource struct
  @behaviour Access

  @impl Access
  def fetch(resource, key) when is_atom(key) do
    case Map.get(Map.from_struct(resource), key) do
      nil -> :error
      value -> {:ok, value}
    end
  end

  @impl Access
  def fetch(resource, key) when is_binary(key) do
    # Convert string key to atom and try again
    fetch(resource, String.to_atom(key))
  end

  @impl Access
  def get_and_update(resource, key, fun) when is_atom(key) do
    case Map.get(Map.from_struct(resource), key) do
      nil ->
        case fun.(nil) do
          {get, update} -> {get, Map.put(resource, key, update)}
          :pop -> {nil, resource}
        end

      value ->
        case fun.(value) do
          {get, update} -> {get, Map.put(resource, key, update)}
          :pop -> {value, Map.delete(resource, key)}
        end
    end
  end

  @impl Access
  def get_and_update(resource, key, fun) when is_binary(key) do
    # Convert string key to atom and try again
    get_and_update(resource, String.to_atom(key), fun)
  end

  @impl Access
  def pop(resource, key) when is_atom(key) do
    case Map.get(Map.from_struct(resource), key) do
      nil -> {nil, resource}
      value -> {value, Map.delete(resource, key)}
    end
  end

  @impl Access
  def pop(resource, key) when is_binary(key) do
    # Convert string key to atom and try again
    pop(resource, String.to_atom(key))
  end

  @doc """
  Creates a changeset for resource records.

  Validates required fields and ensures data integrity for the resource.
  Also validates relationships and data format consistency.
  """
  def changeset(resource, attrs) do
    # Pre-process content field to handle both string and map inputs
    attrs = process_content_field(attrs)

    resource
    |> cast(attrs, [
      :name,
      :type,
      :status,
      :description,
      :content,
      :metadata,
      :settings,
      :version,
      :parent_id,
      :child_ids,
      :tags,
      :categories,
      :created_by,
      :updated_by
    ])
    |> validate_required([:name, :type, :status])
    |> validate_length(:name, min: 3, max: 255)
    |> validate_inclusion(:status, ["draft", "published", "archived", "deleted", "active"])
    |> validate_number(:version, greater_than: 0)
    |> validate_format(:type, ~r/^[a-z][a-z0-9_]*$/,
      message: "must start with a lowercase letter and only contain lowercase letters, numbers, and underscores"
    )
    |> validate_circular_relationship()
    |> validate_relationship_type()
  end

  defp process_content_field(attrs) do
    # Convert all keys to atoms to ensure consistency
    attrs =
      for {key, value} <- attrs, into: %{} do
        new_key = if is_binary(key), do: String.to_atom(key), else: key
        {new_key, value}
      end

    case Map.get(attrs, :content) do
      content when is_binary(content) ->
        case Jason.decode(content) do
          {:ok, decoded} -> Map.put(attrs, :content, decoded)
          _ -> Map.put(attrs, :content, %{text: content})
        end

      content when is_map(content) ->
        attrs

      _ ->
        Map.put(attrs, :content, %{})
    end
  end

  defp validate_circular_relationship(changeset) do
    parent_id = get_field(changeset, :parent_id)
    id = get_field(changeset, :id)

    cond do
      !parent_id || !id ->
        changeset

      parent_id == id ->
        add_error(changeset, :parent_id, "Circular relationship detected")

      would_create_circular_relationship?(id, parent_id) ->
        add_error(changeset, :parent_id, "Circular relationship detected")

      true ->
        changeset
    end
  end

  defp would_create_circular_relationship?(child_id, parent_id) do
    # Check if setting parent_id would create a circular relationship
    # by traversing up the parent chain from the potential parent
    check_parent_chain(parent_id, child_id, MapSet.new())
  end

  defp check_parent_chain(current_id, target_id, visited) do
    cond do
      MapSet.member?(visited, current_id) ->
        false

      current_id == target_id ->
        true

      true ->
        visited = MapSet.put(visited, current_id)

        case Spacecast.Resources.ResourceSystem.get_resource(current_id) do
          {:ok, %{parent_id: parent_id}} when not is_nil(parent_id) ->
            check_parent_chain(parent_id, target_id, visited)

          _ ->
            false
        end
    end
  end

  defp validate_relationship_type(changeset) do
    parent_id = get_field(changeset, :parent_id)
    resource_type = get_field(changeset, :type)

    if parent_id do
      validate_parent_relationship(changeset, parent_id, resource_type)
    else
      changeset
    end
  end

  defp validate_parent_relationship(changeset, parent_id, resource_type) do
    case Spacecast.Resources.ResourceSystem.get_resource(parent_id) do
      {:ok, parent_resource} ->
        if incompatible_relationship?(parent_resource.type, resource_type) do
          add_error(changeset, :parent_id, "Incompatible resource types")
        else
          changeset
        end

      _ ->
        changeset
    end
  end

  defp incompatible_relationship?(parent_type, child_type) do
    # Define incompatible relationships
    # Document can't be parent of folder
    parent_type == "document" && child_type == "folder"
  end
end
