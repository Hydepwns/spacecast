defmodule Spacecast.Resources.ModerationNoteResource do
  @moduledoc """
  Defines the ModerationNote resource for LiveViews.

  This resource demonstrates has_one relationships with comments.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:comment_id, :string, required: true)
  attribute(:moderator_id, :string, required: true)
  attribute(:note, :string, required: true)
  attribute(:action, {:one_of, ["approve", "reject", "flag"]}, required: true)
  attribute(:created_at, :datetime)

  # Demonstrate belongs_to relationships
  belongs_to(:comment, Spacecast.Resources.CommentResource, foreign_key: :comment_id)
  belongs_to(:moderator, Spacecast.Resources.UserResource, foreign_key: :moderator_id)

  validate(:note_not_empty, fn resource ->
    if resource.note && String.length(resource.note) > 0 do
      :ok
    else
      {:error, "Moderation note cannot be empty"}
    end
  end)

  @doc """
  Loads a moderation note resource by ID.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    note = %{
      id: id,
      comment_id: "comment-123",
      moderator_id: "user-456",
      note: "Moderation note #{id}",
      action: "approve",
      created_at: DateTime.utc_now(),
      __resource_module__: __MODULE__
    }

    {:ok, note}
  end

  @doc """
  Returns the initial state for a moderation note resource.
  """
  @spec initial_state() :: map()
  def initial_state do
    %{
      id: nil,
      comment_id: nil,
      moderator_id: nil,
      note: "",
      action: "approve",
      created_at: nil,
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Validates a moderation note resource.
  """
  @spec validate(map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate(resource) do
    []
    |> validate_note(resource)
    |> case do
      [] -> {:ok, resource}
      errors -> {:error, errors}
    end
  end

  defp validate_note(errors, resource) do
    if !resource.note || String.length(resource.note) == 0 do
      ["Moderation note cannot be empty" | errors]
    else
      errors
    end
  end

  # Event sourcing methods
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
