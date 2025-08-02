defmodule Spacecast.Resources.CommentResource do
  @moduledoc """
  Defines the Comment resource for LiveViews.

  This resource demonstrates additional relationship management functions
  including belongs_to and has_many relationships.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:content, :string, required: true)
  attribute(:author_id, :string, required: true)
  attribute(:post_id, :string)
  attribute(:parent_comment_id, :string)
  attribute(:status, {:one_of, ["pending", "approved", "spam"]}, default: "pending")
  attribute(:created_at, :datetime)
  attribute(:updated_at, :datetime)

  # Demonstrate belongs_to relationships
  belongs_to(:author, Spacecast.Resources.UserResource, foreign_key: :author_id)
  belongs_to(:post, Spacecast.Resources.PostResource, foreign_key: :post_id)

  belongs_to(:parent_comment, Spacecast.Resources.CommentResource, foreign_key: :parent_comment_id)

  # Demonstrate has_many relationships
  has_many(:replies, Spacecast.Resources.CommentResource, foreign_key: :parent_comment_id)
  has_many(:reactions, Spacecast.Resources.ReactionResource, foreign_key: :comment_id)

  # Demonstrate has_one relationship
  has_one(:moderation_note, Spacecast.Resources.ModerationNoteResource, foreign_key: :comment_id)

  # Demonstrate has_many_through relationship
  has_many_through(:author_comments, through: [:author, :comments])

  # Demonstrate polymorphic relationship
  polymorphic(:commentable,
    types: [
      Spacecast.Resources.PostResource,
      Spacecast.Resources.CommentResource
    ]
  )

  validate(:content_not_empty, fn resource ->
    if resource.content && String.length(resource.content) > 0 do
      :ok
    else
      {:error, "Comment content cannot be empty"}
    end
  end)

  validate(:author_required, fn resource ->
    if resource.author_id do
      :ok
    else
      {:error, "Comment must have an author"}
    end
  end)

  @doc """
  Loads a comment resource by ID.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    comment = %{
      id: id,
      content: "Comment #{id}",
      author_id: "user-123",
      post_id: "post-456",
      parent_comment_id: nil,
      status: "approved",
      created_at: DateTime.utc_now(),
      updated_at: DateTime.utc_now(),
      __resource_module__: __MODULE__
    }

    {:ok, comment}
  end

  @doc """
  Returns the initial state for a comment resource.
  """
  @spec initial_state() :: map()
  def initial_state do
    %{
      id: nil,
      content: "",
      author_id: nil,
      post_id: nil,
      parent_comment_id: nil,
      status: "pending",
      created_at: nil,
      updated_at: nil,
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Validates a comment resource.
  """
  @spec validate(map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate(resource) do
    []
    |> validate_content(resource)
    |> validate_author(resource)
    |> case do
      [] -> {:ok, resource}
      errors -> {:error, errors}
    end
  end

  defp validate_content(errors, resource) do
    if !resource.content || String.length(resource.content) == 0 do
      ["Comment content cannot be empty" | errors]
    else
      errors
    end
  end

  defp validate_author(errors, resource) do
    if !resource.author_id do
      ["Comment must have an author" | errors]
    else
      errors
    end
  end

  # Event sourcing methods
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
