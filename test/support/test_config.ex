defmodule SpacecastWeb.TestConfig do
  @moduledoc """
  Centralized test configuration for Wallaby and LiveView tests.

  This module provides consistent timeouts, selectors, and test data
  across all UI tests to improve reliability and maintainability.
  """

  # Timeout configurations
  @default_timeout 3000
  @long_timeout 6000
  @very_long_timeout 10000

  # Flash message selectors
  @flash_selectors %{
    success: [
      "[class*='bg-emerald-50'][class*='text-emerald-800']",
      "[class*='alert-success']",
      "[class*='bg-green-100']",
      "[data-test-id='flash-success']"
    ],
    error: [
      "[class*='bg-rose-50'][class*='text-rose-900']",
      "[class*='alert-error']",
      "[class*='bg-red-100']",
      "[data-test-id='flash-error']"
    ]
  }

  # Common test data
  @test_resources %{
    basic: %{
      name: "Test Resource",
      description: "A test resource for automated testing",
      content: "Test content for the resource",
      type: "document",
      status: "published"
    },
    workflow: %{
      name: "Workflow Test Resource",
      description: "A resource for testing workflows",
      content: "Workflow test content",
      type: "document",
      status: "published"
    },
    relationship: %{
      name: "Relationship Test Resource",
      description: "A resource for testing relationships",
      content: "Relationship test content",
      type: "document",
      status: "published"
    }
  }

  # Common form field mappings
  @form_fields %{
    resource: %{
      name: "resource[name]",
      description: "resource[description]",
      content: "resource[content]",
      type: "resource[type]",
      status: "resource[status]",
      parent_id: "resource[parent_id]"
    }
  }

  # Common button texts
  @button_texts %{
    create_resource: "Create Resource",
    save_resource: "Save Resource",
    update_resource: "Update Resource",
    delete_resource: "Delete Resource"
  }

  # Common paths
  @paths %{
    resources: "/resources",
    new_resource: "/resources/new",
    events: "/events",
    timeline: "/timeline"
  }

  # Common test IDs
  @test_ids %{
    create_resource_link: "create-resource-link",
    edit_resource_link: "edit-resource-link",
    delete_resource_button: "delete-resource-button",
    resource_link: "resource-link",
    back_to_resources: "back-to-resources-link"
  }

  # Getters for configuration values
  def default_timeout, do: @default_timeout
  def long_timeout, do: @long_timeout
  def very_long_timeout, do: @very_long_timeout

  def flash_selectors, do: @flash_selectors
  def flash_selectors(type), do: Map.get(@flash_selectors, type, [])

  def test_resources, do: @test_resources
  def test_resource(type), do: Map.get(@test_resources, type)

  def form_fields, do: @form_fields
  def form_fields(form_type), do: Map.get(@form_fields, form_type, %{})

  def button_texts, do: @button_texts
  def button_text(action), do: Map.get(@button_texts, action)

  def paths, do: @paths
  def path(name), do: Map.get(@paths, name)

  def test_ids, do: @test_ids
  def test_id(name), do: Map.get(@test_ids, name)

  @doc """
  Gets a complete form data map for a resource type.

  ## Parameters
  - resource_type: The type of resource data to get (:basic, :workflow, :relationship)
  - overrides: Optional map of field overrides

  ## Returns
  A map of form field names to values
  """
  def resource_form_data(resource_type, overrides \\ %{}) do
    base_data = test_resource(resource_type)
    fields = form_fields(:resource)

    # Map the base data to form field names
    form_data = %{
      fields.name => base_data.name,
      fields.description => base_data.description,
      fields.content => base_data.content,
      fields.type => base_data.type,
      fields.status => base_data.status
    }

    # Apply any overrides
    Map.merge(form_data, overrides)
  end

  @doc """
  Gets a CSS selector for a test ID.

  ## Parameters
  - test_id_name: The name of the test ID

  ## Returns
  A CSS selector string
  """
  def test_id_selector(test_id_name) do
    test_id = test_id(test_id_name)
    "[data-test-id='#{test_id}']"
  end

  @doc """
  Gets a resource link selector for a specific resource ID.

  ## Parameters
  - resource_id: The resource ID

  ## Returns
  A CSS selector string
  """
  def resource_link_selector(resource_id) do
    "[data-test-id='resource-link-#{resource_id}']"
  end
end
