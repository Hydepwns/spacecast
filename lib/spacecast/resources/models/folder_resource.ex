defmodule Spacecast.Resources.FolderResource do
  @moduledoc """
  Defines the Folder resource for LiveViews and tests.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :string, []}
  @derive {Phoenix.Param, key: :id}
  schema "folder_resources" do
    field :name, :string
    field :type, :string
    field :content, :map
    field :parent_id, :string
    field :description, :string
    field :status, :string

    timestamps()
  end

  @doc """
  Returns the initial state for a folder resource as a struct.
  """
  def initial_state do
    %__MODULE__{
      id: "",
      name: "",
      type: "folder",
      content: %{},
      parent_id: "",
      description: "",
      status: "active"
    }
  end

  @doc """
  Applies an event to the folder resource state, always returning a struct.
  """
  def apply_event(event, %__MODULE__{} = state) do
    case event.type do
      "folder.created" ->
        struct(state, Map.merge(Map.from_struct(state), event.data))

      "folder.updated" ->
        struct(state, Map.merge(Map.from_struct(state), event.data))

      "folder.deleted" ->
        struct(state, %{content: %{}})

      _ ->
        struct(state, Map.merge(Map.from_struct(state), event.data || %{}))
    end
  end

  @doc """
  Creates a changeset for a folder resource.
  Can be used for both creating new resources and updating existing ones.
  `attrs` should be the parameters to apply.
  `folder_struct` is the resource struct, typically `%FolderResource{}` for new,
  or an existing struct for updates.
  """
  def changeset(folder_struct \\ initial_state(), attrs)

  def changeset(folder_struct, attrs) when not is_map(attrs) do
    changeset(folder_struct, %{})
  end

  def changeset(folder_struct, attrs) do
    folder_struct
    |> cast(attrs, [:id, :name, :type, :content, :description, :parent_id, :status])
    |> validate_required([:name, :type])
    |> validate_length(:name, min: 3)
    |> validate_inclusion(:type, ["folder"])
    |> put_change(:status, Map.get(attrs, :status, "active"))
    |> put_change(:description, Map.get(attrs, :description, ""))
    |> validate_content_type()
  end

  defp validate_content_type(changeset) do
    case get_change(changeset, :content) do
      nil -> changeset
      content when is_map(content) -> changeset
      _ -> add_error(changeset, :content, "must be a map")
    end
  end
end
