defmodule Spacecast.Resources.RevisionResource do
  @moduledoc """
  Defines the Revision resource for LiveViews.

  This resource demonstrates belongs_to relationships with posts.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:post_id, :string, required: true)
  attribute(:editor_id, :string, required: true)
  attribute(:content, :string, required: true)
  attribute(:version, :integer, required: true)
  attribute(:created_at, :datetime)
  attribute(:approved, :boolean, default: false)

  # Demonstrate belongs_to relationships
  belongs_to(:post, Spacecast.Resources.PostResource, foreign_key: :post_id)
  belongs_to(:editor, Spacecast.Resources.UserResource, foreign_key: :editor_id)

  validate(:content_not_empty, fn resource ->
    if resource.content && String.length(resource.content) > 0 do
      :ok
    else
      {:error, "Revision content cannot be empty"}
    end
  end)

  @doc """
  Loads a revision resource by ID.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    revision = %{
      id: id,
      post_id: "post-123",
      editor_id: "user-456",
      content: "Revision content #{id}",
      version: 1,
      created_at: DateTime.utc_now(),
      approved: false,
      __resource_module__: __MODULE__
    }

    {:ok, revision}
  end

  @doc """
  Returns the initial state for a revision resource.
  """
  @spec initial_state() :: map()
  def initial_state do
    %{
      id: nil,
      post_id: nil,
      editor_id: nil,
      content: "",
      version: 1,
      created_at: nil,
      approved: false,
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Validates a revision resource.
  """
  @spec validate(map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate(resource) do
    []
    |> validate_content(resource)
    |> case do
      [] -> {:ok, resource}
      errors -> {:error, errors}
    end
  end

  defp validate_content(errors, resource) do
    if !resource.content || String.length(resource.content) == 0 do
      ["Revision content cannot be empty" | errors]
    else
      errors
    end
  end

  # Event sourcing methods
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
