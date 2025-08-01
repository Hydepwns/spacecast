defmodule Spacecast.Resources do
  @moduledoc """
  Main interface for working with resources in the system.
  This module provides common functions for CRUD operations on resources.

  ## Resource Types

  The system supports various types of resources:

  * `document` - Text-based resources with content
  * `user` - User resources with authentication info
  * `media` - Media resources like images and videos

  ## Examples

      # Create a new document resource
      {:ok, resource} = create_resource(%{
        "name" => "My Document",
        "type" => "document",
        "content" => %{"text" => "Hello world"}
      })

      # Get resource statistics
      stats = get_resource_stats()
      # => %{total: 10, published: 5, draft: 5}

      # List child resources
      children = list_child_resources("parent-123")
  """

  alias Spacecast.Resources.DocumentResource
  alias Spacecast.Resources.RelationshipManager
  alias Spacecast.Resources.ResourceSystem
  alias Spacecast.Resources.Resource

  @type resource_id :: String.t()
  @type resource_type :: String.t()
  @type resource_status :: String.t()
  @type resource_params :: %{String.t() => any()}
  @type resource_stats :: %{
          total: non_neg_integer(),
          published: non_neg_integer(),
          draft: non_neg_integer()
        }
  @type relationship :: %{parent: Resource.t(), child: Resource.t()}

  @doc """
  Gets a resource by id.

  ## Parameters

    * `id` - The unique identifier of the resource

  ## Returns

    * `Resource.t()` - The found resource
    * A mock resource if not found (in development)

  ## Examples

      iex> get_resource!("doc-123")
      %DocumentResource{id: "doc-123", name: "Test Resource", ...}
  """
  @spec get_resource!(resource_id()) :: Resource.t()
  def get_resource!(id) do
    case ResourceSystem.get_resource(id) do
      {:ok, resource} ->
        resource

      {:error, :not_found} ->
        # For now, create a mock resource for testing
        # In a real implementation, this would likely fetch from a database
        struct(DocumentResource, %{
          id: id,
          name: "Test Resource",
          content: %{text: "Test content for resource #{id}"},
          description: "A test resource for development",
          status: "active",
          type: "document",
          parent_id: nil
        })
    end
  end

  @doc """
  Updates a resource with the given params.

  ## Parameters

    * `resource` - The resource to update
    * `params` - Map of parameters to update

  ## Returns

    * `{:ok, Resource.t()}` - On successful update
    * `{:error, Ecto.Changeset.t()}` - On validation error

  ## Examples

      iex> update_resource(resource, %{"name" => "Updated Name"})
      {:ok, %DocumentResource{name: "Updated Name", ...}}
  """
  @spec update_resource(Resource.t(), resource_params()) ::
          {:ok, Resource.t()} | {:error, Ecto.Changeset.t()}
  def update_resource(resource, params) do
    # Convert string keys to atom keys in params
    atom_params = for {k, v} <- params, into: %{}, do: {String.to_existing_atom(k), v}
    ResourceSystem.update_resource(resource.id, atom_params)
  end

  @doc """
  Deletes a resource by id.
  """
  def delete_resource(id) do
    ResourceSystem.delete_resource(id)
  end

  @doc """
  Creates a changeset for a resource.
  """
  def change_resource(resource, attrs \\ %{}) do
    DocumentResource.changeset(resource, attrs)
  end

  @doc """
  Creates a new resource with the given params.
  """
  def create_resource(params) do
    # Convert string keys to atom keys in params
    atom_params = for {k, v} <- params, into: %{}, do: {String.to_existing_atom(k), v}
    ResourceSystem.create_resource(atom_params)
  end

  @doc """
  Lists all resources.
  """
  def list_resources do
    ResourceSystem.list_resources([])
  end

  @doc """
  Lists resources by type.
  """
  def list_resources_by_type(type) when type in ["", nil] do
    list_resources()
  end

  def list_resources_by_type(type) do
    list_resources()
    |> Enum.filter(&(&1.type == type))
  end

  @doc """
  Gets statistics about resources in the system.

  ## Returns

    * `resource_stats()` - Map containing:
      * `:total` - Total number of resources
      * `:published` - Number of published resources
      * `:draft` - Number of draft resources

  ## Examples

      iex> get_resource_stats()
      %{total: 10, published: 5, draft: 5}
  """
  @spec get_resource_stats() :: resource_stats()
  def get_resource_stats do
    resources = list_resources()

    %{
      total: length(resources),
      published: Enum.count(resources, &(&1.status == "published")),
      draft: Enum.count(resources, &(&1.status == "draft"))
    }
  end

  @doc """
  Lists child resources for a given parent resource ID.
  """
  def list_child_resources(parent_id) do
    list_resources()
    |> Enum.filter(&(&1.parent_id == parent_id))
  end

  @doc """
  Lists all resource relationships.
  """
  def list_relationships do
    resources = list_resources()

    Enum.flat_map(resources, fn resource ->
      build_relationship_for_resource(resource)
    end)
  end

  defp build_relationship_for_resource(%{parent_id: nil}), do: []

  defp build_relationship_for_resource(resource) do
    case get_resource(resource.parent_id) do
      nil -> []
      parent -> [{parent, resource}]
    end
  end

  @doc """
  Creates a relationship between two resources.
  """
  @spec create_relationship(resource_id(), resource_id()) :: {:ok, any()}
  def create_relationship(parent_id, child_id) do
    {:ok, relationship} = RelationshipManager.create_relationship(parent_id, child_id, "parent_child")
    # Update the child resource with the parent_id
    child = get_resource!(child_id)
    update_resource(child, %{parent_id: parent_id})
    {:ok, relationship}
  end

  @spec remove_relationship(resource_id(), resource_id()) :: {:ok, any()}
  @doc """
  Removes a relationship between two resources.
  """
  def remove_relationship(parent_id, child_id) do
    # Update the child resource to remove the parent_id
    child = get_resource!(child_id)
    update_resource(child, %{parent_id: nil})
    {:ok, struct(DocumentResource, %{parent_id: parent_id, id: child_id})}
  end

  @doc """
  Retrieves a resource by ID.
  """
  @spec get_resource(resource_id()) :: {:ok, Resource.t()} | {:error, :not_found}
  def get_resource(id) do
    case ResourceSystem.get_resource(id) do
      {:ok, resource} -> resource
      {:error, :not_found} -> nil
    end
  end

  @doc """
  Executes a validation plan for a user resource.

  ## Parameters

    * `user_resource` - The user resource to validate
    * `plan` - The validation plan to execute

  ## Returns

    * `{:ok, user_resource}` - When all validations pass
    * `{:error, validation_errors}` - When some validations fail
    * `{:checkpoint, checkpoint_state}` - When validation is partially complete

  ## Examples

      iex> {:ok, plan} = ValidationDependencyResolver.resolve_dependencies(user_resource)
      iex> execute_validation_plan(user_resource, plan)
      {:ok, user_resource}
  """
  @spec execute_validation_plan(Resource.t(), map()) ::
          {:ok, Resource.t()}
          | {:error, map()}
          | {:checkpoint, map()}
  def execute_validation_plan(user_resource, plan) do
    case Spacecast.Utils.ValidationDependencyResolver.execute_validation_plan(
           plan,
           user_resource
         ) do
      {:ok, _results} ->
        {:ok, user_resource}

      {:error, errors} ->
        {:error, errors}

      {:checkpoint, checkpoint} ->
        {:checkpoint, checkpoint}
    end
  end
end
