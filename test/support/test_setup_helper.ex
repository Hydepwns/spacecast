defmodule SpacecastWeb.TestSetupHelper do
  @moduledoc """
  Helper functions for setting up test data and ensuring test isolation.

  This module provides utilities for:
  - Creating test resources with consistent data
  - Cleaning up test data between tests
  - Setting up complex test scenarios
  - Ensuring proper test isolation
  """

  alias Spacecast.Resources.ResourceSystem

  @doc """
  Creates a test resource with the given type and optional overrides.

  ## Parameters
  - resource_type: The type of resource to create (:basic, :workflow, :relationship)
  - overrides: Optional map of field overrides

  ## Returns
  {:ok, resource} on success, {:error, reason} on failure
  """
  def create_test_resource(resource_type, overrides \\ %{}) do
    base_data = SpacecastWeb.TestConfig.test_resource(resource_type)
    resource_params = Map.merge(base_data, overrides)

    case ResourceSystem.create_resource(resource_params) do
      {:ok, resource} -> {:ok, resource}
      {:error, changeset} -> {:error, changeset}
    end
  end

  @doc """
  Creates multiple test resources for relationship testing.

  ## Parameters
  - count: Number of resources to create
  - resource_type: Type of resources to create

  ## Returns
  List of created resources
  """
  def create_test_resources(count, resource_type \\ :basic) do
    Enum.map(1..count, fn i ->
      overrides = %{name: "Test Resource #{i}"}
      {:ok, resource} = create_test_resource(resource_type, overrides)
      resource
    end)
  end

  @doc """
  Creates a parent-child relationship between resources.

  ## Parameters
  - parent: Parent resource
  - child: Child resource

  ## Returns
  {:ok, updated_child} on success, {:error, reason} on failure
  """
  def create_resource_relationship(parent, child) do
    ResourceSystem.update_resource(child.id, %{parent_id: parent.id})
  end

  @doc """
  Cleans up test resources by deleting them.

  ## Parameters
  - resources: List of resources to delete

  ## Returns
  List of deletion results
  """
  def cleanup_test_resources(resources) when is_list(resources) do
    Enum.map(resources, &cleanup_test_resource/1)
  end

  def cleanup_test_resource(resource) do
    case ResourceSystem.delete_resource(resource.id) do
      {:ok, _} -> {:ok, resource.id}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Sets up a complete test scenario with multiple related resources.

  ## Parameters
  - scenario: The scenario to set up (:basic, :with_relationships, :complex)

  ## Returns
  Map containing the created resources and their relationships
  """
  def setup_test_scenario(scenario) do
    case scenario do
      :basic ->
        {:ok, parent} = create_test_resource(:basic, %{name: "Parent Resource"})
        {:ok, child1} = create_test_resource(:basic, %{name: "Child Resource 1"})
        {:ok, child2} = create_test_resource(:basic, %{name: "Child Resource 2"})

        # Create relationships
        {:ok, child1} = create_resource_relationship(parent, child1)
        {:ok, child2} = create_resource_relationship(parent, child2)

        %{
          parent: parent,
          children: [child1, child2],
          all_resources: [parent, child1, child2]
        }

      :with_relationships ->
        {:ok, grandparent} = create_test_resource(:basic, %{name: "Grandparent Resource"})
        {:ok, parent} = create_test_resource(:basic, %{name: "Parent Resource"})
        {:ok, child} = create_test_resource(:basic, %{name: "Child Resource"})

        # Create hierarchical relationships
        {:ok, parent} = create_resource_relationship(grandparent, parent)
        {:ok, child} = create_resource_relationship(parent, child)

        %{
          grandparent: grandparent,
          parent: parent,
          child: child,
          all_resources: [grandparent, parent, child]
        }

      :complex ->
        # Create a more complex scenario with multiple types
        {:ok, document} = create_test_resource(:basic, %{name: "Test Document", type: "document"})
        {:ok, task} = create_test_resource(:basic, %{name: "Test Task", type: "task"})
        {:ok, note} = create_test_resource(:basic, %{name: "Test Note", type: "note"})

        %{
          document: document,
          task: task,
          note: note,
          all_resources: [document, task, note]
        }
    end
  end

  @doc """
  Cleans up a test scenario by deleting all created resources.

  ## Parameters
  - scenario_data: The scenario data returned from setup_test_scenario

  ## Returns
  List of cleanup results
  """
  def cleanup_test_scenario(scenario_data) do
    resources = scenario_data.all_resources || []
    cleanup_test_resources(resources)
  end

  @doc """
  Waits for a resource to be available in the database.

  ## Parameters
  - resource_id: The ID of the resource to wait for
  - timeout: Timeout in milliseconds (default: 5000)

  ## Returns
  {:ok, resource} on success, {:error, :timeout} on timeout
  """
  def wait_for_resource_in_db(resource_id, timeout \\ 5000) do
    start_time = System.monotonic_time(:millisecond)

    do_wait_for_resource_in_db(resource_id, timeout, start_time)
  end

  defp do_wait_for_resource_in_db(resource_id, timeout, start_time) do
    case ResourceSystem.get_resource(resource_id) do
      {:ok, resource} ->
        {:ok, resource}

      {:error, :not_found} ->
        now = System.monotonic_time(:millisecond)

        if now - start_time < timeout do
          Process.sleep(100)
          do_wait_for_resource_in_db(resource_id, timeout, start_time)
        else
          {:error, :timeout}
        end
    end
  end

  @doc """
  Verifies that a resource relationship exists in the database.

  ## Parameters
  - child_id: The child resource ID
  - parent_id: The parent resource ID (nil for no parent)

  ## Returns
  true if relationship is correct, false otherwise
  """
  def verify_resource_relationship(child_id, parent_id) do
    case ResourceSystem.get_resource(child_id) do
      {:ok, resource} ->
        resource.parent_id == parent_id

      {:error, _} ->
        false
    end
  end

  @doc """
  Creates test data for form validation testing.

  ## Returns
  Map containing valid and invalid test data
  """
  def create_validation_test_data do
    %{
      valid: %{
        "resource[name]" => "Valid Resource",
        "resource[description]" => "Valid description",
        "resource[content]" => "Valid content",
        "resource[type]" => "document",
        "resource[status]" => "published"
      },
      invalid_empty: %{
        "resource[name]" => "",
        "resource[description]" => "",
        "resource[content]" => ""
      },
      invalid_long: %{
        "resource[name]" => String.duplicate("a", 1000), # Assuming there's a length limit
        "resource[description]" => "Valid description",
        "resource[content]" => "Valid content"
      }
    }
  end
end
