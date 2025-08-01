defmodule Spacecast.Resources.PostResource do
  @moduledoc """
  Defines the Post resource for LiveViews.

  This resource demonstrates the usage of relationship management functions
  including belongs_to, has_many, has_many_through, and polymorphic relationships.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:title, :string, required: true)
  attribute(:content, :string)
  attribute(:status, {:one_of, ["draft", "published", "archived"]}, default: "draft")
  attribute(:published_at, :datetime)
  attribute(:author_id, :string)
  attribute(:category_id, :string)
  attribute(:tags, {:list, :string}, default: [])

  # Demonstrate belongs_to relationship
  belongs_to(:author, Spacecast.Resources.UserResource, foreign_key: :author_id)

  # Demonstrate belongs_to with custom foreign key
  belongs_to(:category, Spacecast.Resources.CategoryResource, foreign_key: :category_id)

  # Demonstrate has_many relationship
  has_many(:comments, Spacecast.Resources.CommentResource, foreign_key: :post_id)

  # Demonstrate has_many with custom foreign key
  has_many(:revisions, Spacecast.Resources.RevisionResource, foreign_key: :post_id)

  # Demonstrate has_one relationship
  has_one(:featured_image, Spacecast.Resources.MediaResource, foreign_key: :post_id)

  # Demonstrate has_many_through relationship
  has_many_through(:author_posts, through: [:author, :posts])

  # Demonstrate has_many_through with options
  has_many_through(:team_posts, through: [:author, :team])

  # Demonstrate has_one_through relationship
  has_one_through(:author_profile, through: [:author, :profile])

  # Demonstrate polymorphic relationship
  polymorphic(:commentable,
    types: [
      Spacecast.Resources.PostResource,
      Spacecast.Resources.CommentResource
    ]
  )

  # Demonstrate polymorphic with options
  polymorphic(:attachable,
    types: [
      Spacecast.Resources.MediaResource,
      Spacecast.Resources.DocumentResource
    ]
  )

  validate(:title_not_empty, fn resource ->
    if resource.title && String.length(resource.title) > 0 do
      :ok
    else
      {:error, "Post title cannot be empty"}
    end
  end)

  validate(:content_required_for_published, fn resource ->
    if resource.status == "published" &&
         (!resource.content || String.length(resource.content) == 0) do
      {:error, "Published posts must have content"}
    else
      :ok
    end
  end)

  @doc """
  Loads a post resource by ID.

  This is a stub implementation for testing purposes.
  In a real application, this would fetch the post from a database.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    # This is a simple stub that always returns a post with the given ID
    # In a real application, this would query the database
    post = %{
      id: id,
      title: "Post #{id}",
      content: "Content for post #{id}",
      status: "draft",
      author_id: "user-123",
      category_id: "cat-456",
      tags: ["example", "test"],
      published_at: nil,
      __resource_module__: __MODULE__
    }

    {:ok, post}
  end

  @doc """
  Returns the initial state for a post resource as a struct.
  """
  @spec initial_state() :: map()
  def initial_state do
    %{
      id: nil,
      title: "",
      content: "",
      status: "draft",
      published_at: nil,
      author_id: nil,
      category_id: nil,
      tags: [],
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Validates a post resource.
  """
  @spec validate(map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate(resource) do
    []
    |> validate_title(resource)
    |> validate_content_for_published(resource)
    |> case do
      [] -> {:ok, resource}
      errors -> {:error, errors}
    end
  end

  defp validate_title(errors, resource) do
    if !resource.title || String.length(resource.title) == 0 do
      ["Title cannot be empty" | errors]
    else
      errors
    end
  end

  defp validate_content_for_published(errors, resource) do
    if resource.status == "published" &&
         (!resource.content || String.length(resource.content) == 0) do
      ["Published posts must have content" | errors]
    else
      errors
    end
  end

  @doc """
  Resolves relationships for a post resource.
  """
  @spec resolve_relationships(map()) :: {:ok, map()} | {:error, String.t()}
  def resolve_relationships(resource) do
    # This would typically use the RelationshipResolver to load related resources
    # For now, we'll return the resource as-is
    {:ok, resource}
  end

  @doc """
  Resolves a specific relationship for a post resource.
  """
  @spec resolve_relationship(map(), atom()) :: {:ok, any()} | {:error, String.t()}
  def resolve_relationship(resource, relationship_name) do
    case relationship_name do
      :author ->
        if resource.author_id do
          Spacecast.Resources.UserResource.load(resource.author_id)
        else
          {:ok, nil}
        end

      :comments ->
        # In a real implementation, this would query for comments
        {:ok, []}

      :category ->
        if resource.category_id do
          Spacecast.Resources.CategoryResource.load(resource.category_id)
        else
          {:ok, nil}
        end

      _ ->
        {:error, "Unknown relationship: #{relationship_name}"}
    end
  end

  @doc """
  Performs deep validation of the resource.
  """
  def validate_deep(resource) do
    case validate(resource) do
      {:ok, validated} -> {:ok, validated}
      {:error, errors} -> {:error, errors}
    end
  end

  # Event sourcing methods
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
