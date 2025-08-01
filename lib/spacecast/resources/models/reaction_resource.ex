defmodule Spacecast.Resources.ReactionResource do
  @moduledoc """
  Defines the Reaction resource for LiveViews.

  This resource demonstrates belongs_to relationships with comments.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:comment_id, :string, required: true)
  attribute(:user_id, :string, required: true)
  attribute(:reaction_type, {:one_of, ["like", "love", "laugh", "sad", "angry"]}, required: true)
  attribute(:created_at, :datetime)

  # Demonstrate belongs_to relationships
  belongs_to(:comment, Spacecast.Resources.CommentResource, foreign_key: :comment_id)
  belongs_to(:user, Spacecast.Resources.UserResource, foreign_key: :user_id)

  validate(:reaction_type_valid, fn resource ->
    valid_types = ["like", "love", "laugh", "sad", "angry"]

    if resource.reaction_type && resource.reaction_type in valid_types do
      :ok
    else
      {:error, "Invalid reaction type"}
    end
  end)

  @doc """
  Loads a reaction resource by ID.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    reaction = %{
      id: id,
      comment_id: "comment-123",
      user_id: "user-456",
      reaction_type: "like",
      created_at: DateTime.utc_now(),
      __resource_module__: __MODULE__
    }

    {:ok, reaction}
  end

  @doc """
  Returns the initial state for a reaction resource.
  """
  @spec initial_state() :: map()
  def initial_state do
    %{
      id: nil,
      comment_id: nil,
      user_id: nil,
      reaction_type: "like",
      created_at: nil,
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Validates a reaction resource.
  """
  @spec validate(map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate(resource) do
    []
    |> validate_reaction_type(resource)
    |> case do
      [] -> {:ok, resource}
      errors -> {:error, errors}
    end
  end

  defp validate_reaction_type(errors, resource) do
    valid_types = ["like", "love", "laugh", "sad", "angry"]

    if !resource.reaction_type || !(resource.reaction_type in valid_types) do
      ["Invalid reaction type" | errors]
    else
      errors
    end
  end

  # Event sourcing methods
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
