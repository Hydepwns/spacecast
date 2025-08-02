defmodule SpacecastWeb.TestDataFactory do
  @moduledoc """
  Test data factory for generating consistent test data.

  This module provides utilities for:
  - Generating test resources with various attributes
  - Creating test scenarios with multiple related entities
  - Building form data for different test cases
  - Ensuring test data consistency and uniqueness
  """

  alias Spacecast.Resources.ResourceSystem

  @doc """
  Generates a unique resource name for testing.

  ## Parameters
  - base_name: Base name for the resource
  - prefix: Optional prefix to add

  ## Returns
  A unique resource name
  """
  def unique_resource_name(base_name, prefix \\ "Test") do
    timestamp = System.monotonic_time(:millisecond)
    "#{prefix} #{base_name} #{timestamp}"
  end

  @doc """
  Creates a basic resource with default values.

  ## Parameters
  - overrides: Optional map of field overrides

  ## Returns
  Map of resource attributes
  """
  def build_resource(overrides \\ %{}) do
    base_resource = %{
      name: unique_resource_name("Resource"),
      description: "A test resource created by TestDataFactory",
      content: %{text: "Test content for the resource"},
      type: "document",
      status: "published",
      metadata: %{created_by: "test_factory"}
    }

    Map.merge(base_resource, overrides)
  end

  @doc """
  Creates a resource with specific type and status.

  ## Parameters
  - type: Resource type (document, task, note, etc.)
  - status: Resource status (draft, published, archived, etc.)
  - overrides: Optional additional overrides

  ## Returns
  Map of resource attributes
  """
  def build_resource_with_type(type, status, overrides \\ %{}) do
    build_resource(
      %{
        type: type,
        status: status
      }
      |> Map.merge(overrides)
    )
  end

  @doc """
  Creates form data for resource creation/editing.

  ## Parameters
  - resource_data: Resource data map
  - form_type: Type of form (create, edit)

  ## Returns
  Map of form field names to values
  """
  def build_resource_form_data(resource_data, form_type \\ :create) do
    fields = SpacecastWeb.TestConfig.form_fields(:resource)

    form_data = %{
      fields.name => resource_data.name,
      fields.description => resource_data.description,
      fields.content => resource_data.content,
      fields.type => resource_data.type,
      fields.status => resource_data.status
    }

    # Add parent_id if present and this is an edit form
    if Map.has_key?(resource_data, :parent_id) and form_type == :edit do
      Map.put(form_data, fields.parent_id, resource_data.parent_id)
    else
      form_data
    end
  end

  @doc """
  Creates a hierarchy of resources for relationship testing.

  ## Parameters
  - depth: Depth of the hierarchy (1 = parent only, 2 = parent + children, etc.)
  - children_per_level: Number of children per parent

  ## Returns
  Map containing the hierarchy structure
  """
  def build_resource_hierarchy(depth, children_per_level \\ 2) do
    build_hierarchy_level(1, depth, children_per_level, nil, [])
  end

  defp build_hierarchy_level(current_level, max_depth, children_per_level, parent_id, all_resources) do
    if current_level > max_depth do
      %{resources: all_resources, hierarchy: build_hierarchy_map(all_resources)}
    else
      level_resources = create_level_resources(current_level, children_per_level, parent_id)
      all_resources = all_resources ++ level_resources

      # Recursively create children for each resource at this level
      Enum.reduce(level_resources, %{resources: all_resources, hierarchy: %{}}, fn resource, acc ->
        child_result =
          build_hierarchy_level(current_level + 1, max_depth, children_per_level, resource.id, acc.resources)

        %{
          resources: child_result.resources,
          hierarchy: Map.merge(acc.hierarchy, child_result.hierarchy)
        }
      end)
    end
  end

  defp create_level_resources(level, count, parent_id) do
    Enum.map(1..count, fn i ->
      build_resource(%{
        name: unique_resource_name("Level #{level} Resource #{i}"),
        description: "Resource at level #{level}, position #{i}",
        metadata: %{level: level, position: i, parent_id: parent_id}
      })
    end)
  end

  defp build_hierarchy_map(resources) do
    Enum.reduce(resources, %{}, fn resource, acc ->
      parent_id = resource.metadata[:parent_id]

      if parent_id do
        Map.update(acc, parent_id, [resource], fn children -> [resource | children] end)
      else
        acc
      end
    end)
  end

  @doc """
  Creates test data for validation testing.

  ## Returns
  Map containing various validation test scenarios
  """
  def build_validation_test_data do
    %{
      valid: build_resource(),
      invalid_empty: %{
        name: "",
        description: "",
        content: %{text: ""},
        type: "document",
        status: "published"
      },
      invalid_long_name:
        build_resource(%{
          name: String.duplicate("a", 1000)
        }),
      invalid_invalid_type:
        build_resource(%{
          type: "invalid_type"
        }),
      invalid_invalid_status:
        build_resource(%{
          status: "invalid_status"
        }),
      edge_cases: %{
        minimal: %{
          name: "Minimal",
          description: "",
          content: %{text: ""},
          type: "document",
          status: "draft"
        },
        maximal: %{
          name: "Maximal Resource with Very Long Name",
          description:
            "A very detailed description with lots of information about this resource and its purpose in the system",
          content: %{text: "Very detailed content with lots of information and data"},
          type: "document",
          status: "published",
          metadata: %{tags: ["important", "featured"], priority: "high"}
        }
      }
    }
  end

  @doc """
  Creates test data for workflow testing.

  ## Returns
  Map containing workflow test scenarios
  """
  def build_workflow_test_data do
    %{
      creation_workflow: %{
        draft: build_resource_with_type("document", "draft"),
        published: build_resource_with_type("document", "published"),
        archived: build_resource_with_type("document", "archived")
      },
      type_workflow: %{
        document: build_resource_with_type("document", "published"),
        task: build_resource_with_type("task", "published"),
        note: build_resource_with_type("note", "published")
      },
      relationship_workflow: %{
        parent: build_resource_with_type("document", "published"),
        child: build_resource_with_type("document", "published"),
        grandchild: build_resource_with_type("document", "published")
      }
    }
  end

  @doc """
  Creates a batch of resources for performance testing.

  ## Parameters
  - count: Number of resources to create
  - batch_size: Size of each batch (for database operations)

  ## Returns
  List of resource data maps
  """
  def build_resource_batch(count, batch_size \\ 10) do
    Enum.chunk_every(1..count, batch_size)
    |> Enum.flat_map(fn batch ->
      Enum.map(batch, fn i ->
        build_resource(%{
          name: unique_resource_name("Batch Resource #{i}"),
          description: "Resource #{i} of #{count} in batch",
          metadata: %{batch_index: i, total_count: count}
        })
      end)
    end)
  end

  @doc """
  Creates test data for search and filtering scenarios.

  ## Returns
  Map containing search test data
  """
  def build_search_test_data do
    %{
      documents: [
        build_resource_with_type("document", "published", %{name: "API Documentation"}),
        build_resource_with_type("document", "published", %{name: "User Guide"}),
        build_resource_with_type("document", "draft", %{name: "Draft Specification"})
      ],
      tasks: [
        build_resource_with_type("task", "published", %{name: "Implement Feature A"}),
        build_resource_with_type("task", "published", %{name: "Fix Bug B"}),
        build_resource_with_type("task", "draft", %{name: "Plan Feature C"})
      ],
      notes: [
        build_resource_with_type("note", "published", %{name: "Meeting Notes"}),
        build_resource_with_type("note", "published", %{name: "Ideas for Improvement"}),
        build_resource_with_type("note", "draft", %{name: "Personal Notes"})
      ]
    }
  end

  @doc """
  Creates test data for event testing scenarios.

  ## Returns
  Map containing event test data
  """
  def build_event_test_data do
    %{
      event_triggers: [
        build_resource_with_type("document", "draft", %{name: "Event Trigger 1"}),
        build_resource_with_type("task", "published", %{name: "Event Trigger 2"}),
        build_resource_with_type("note", "archived", %{name: "Event Trigger 3"})
      ],
      event_subscribers: [
        build_resource_with_type("document", "published", %{name: "Event Subscriber 1"}),
        build_resource_with_type("task", "published", %{name: "Event Subscriber 2"})
      ]
    }
  end

  @doc """
  Cleans up test data created by the factory.

  ## Parameters
  - resources: List of resources to clean up

  ## Returns
  List of cleanup results
  """
  def cleanup_test_data(resources) when is_list(resources) do
    Enum.map(resources, fn resource ->
      case ResourceSystem.delete_resource(resource.id) do
        {:ok, _} -> {:ok, resource.id}
        {:error, reason} -> {:error, reason}
      end
    end)
  end
end
