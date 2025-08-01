defmodule Spacecast.Resources.CategoryResource do
  @moduledoc """
  Defines the Category resource for LiveViews.

  This resource demonstrates has_many relationships with posts.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:name, :string, required: true)
  attribute(:description, :string)
  attribute(:slug, :string)
  attribute(:parent_id, :string)
  attribute(:active, :boolean, default: true)

  # Demonstrate has_many relationship
  has_many(:posts, Spacecast.Resources.PostResource, foreign_key: :category_id)

  # Demonstrate belongs_to relationship (self-referential for hierarchical categories)
  belongs_to(:parent, Spacecast.Resources.CategoryResource, foreign_key: :parent_id)

  # Demonstrate has_many relationship (self-referential for child categories)
  has_many(:children, Spacecast.Resources.CategoryResource, foreign_key: :parent_id)

  # Demonstrate has_many_through relationship
  has_many_through(:all_posts, through: [:children, :posts])

  validate(:name_not_empty, fn resource ->
    if resource.name && String.length(resource.name) > 0 do
      :ok
    else
      {:error, "Category name cannot be empty"}
    end
  end)

  @doc """
  Loads a category resource by ID.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    category = %{
      id: id,
      name: "Category #{id}",
      description: "Description for category #{id}",
      slug: "category-#{id}",
      parent_id: nil,
      active: true,
      __resource_module__: __MODULE__
    }

    {:ok, category}
  end

  @doc """
  Returns the initial state for a category resource.
  """
  @spec initial_state() :: map()
  def initial_state do
    %{
      id: nil,
      name: "",
      description: "",
      slug: "",
      parent_id: nil,
      active: true,
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Validates a category resource.
  """
  @spec validate(map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate(resource) do
    []
    |> validate_name(resource)
    |> case do
      [] -> {:ok, resource}
      errors -> {:error, errors}
    end
  end

  defp validate_name(errors, resource) do
    if !resource.name || String.length(resource.name) == 0 do
      ["Category name cannot be empty" | errors]
    else
      errors
    end
  end

  # Event sourcing methods
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
