defmodule Spacecast.Resources.DocumentResource do
  @moduledoc """
  Defines the Document resource for LiveViews and tests.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :string, []}
  @derive {Phoenix.Param, key: :id}
  schema "document_resources" do
    field :name, :string
    field :type, :string
    field :content, :map, default: %{text: ""}
    field :parent_id, :string
    field :description, :string
    field :status, :string

    timestamps()
  end

  @doc """
  Returns the initial state for a document resource as a struct.
  """
  def initial_state do
    %__MODULE__{
      id: nil,
      name: nil,
      type: "document",
      content: %{text: ""},
      parent_id: nil,
      description: nil,
      status: "active"
    }
  end

  @doc """
  Applies an event to the document resource state, always returning a struct.
  """
  def apply_event(event, %__MODULE__{} = state) do
    case event.type do
      "document.created" ->
        struct(state, Map.merge(Map.from_struct(state), event.data))

      "document.updated" ->
        struct(state, Map.merge(Map.from_struct(state), event.data))

      "document.deleted" ->
        %{state | content: %{text: ""}}

      _ ->
        struct(state, Map.merge(Map.from_struct(state), event.data || %{}))
    end
  end

  @doc """
  Creates a changeset for a document resource.
  Can be used for both creating new resources and updating existing ones.
  `attrs` should be the parameters to apply.
  `document_struct` is the resource struct, typically `%DocumentResource{}` for new,
  or an existing struct for updates.
  """
  def changeset(document_struct \\ initial_state(), attrs) do
    document_struct
    |> cast(attrs, [:id, :name, :type, :content, :description, :parent_id, :status])
    |> validate_required([:id, :name, :type])
    |> validate_length(:name, min: 3)
    |> validate_inclusion(:type, ["document"])
    |> put_change(:status, Map.get(attrs, :status, "active"))
    |> put_change(:description, Map.get(attrs, :description, ""))
    |> validate_content()
  end

  defp validate_content(changeset) do
    case get_change(changeset, :content) do
      nil ->
        put_change(changeset, :content, %{text: ""})

      content when is_map(content) ->
        changeset

      content when is_binary(content) ->
        put_change(changeset, :content, %{text: content})

      _ ->
        add_error(changeset, :content, "must be a map or string")
    end
  end
end
