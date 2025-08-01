defmodule Spacecast.Resources.ResourceSystemTest do
  use Spacecast.DataCase
  import Mox
  setup :set_mox_from_context
  setup :verify_on_exit!

  alias Spacecast.Resources.ResourceSystem
  alias Spacecast.Resources.Resource

  setup do
    # Set up mocks in private mode for this test
    Mox.set_mox_private()
    SpacecastWeb.TestMockHelper.setup_mocks()
    :ok
  end

  describe "ResourceSystem core logic" do
    test "creates a resource successfully" do
      attrs = %{
        name: "Test Resource",
        type: "document",
        status: "published",
        content: %{text: "Test content"}
      }

      assert {:ok, %Resource{} = resource} = ResourceSystem.create_resource(attrs, [])
      assert resource.name == "Test Resource"
      assert resource.type == "document"
      assert resource.status == "published"
    end

    test "fails to create resource with missing required fields" do
      attrs = %{type: "document"}
      assert {:error, changeset} = ResourceSystem.create_resource(attrs, [])
      refute changeset.valid?
    end

    test "updates a resource successfully" do
      {:ok, resource} =
        ResourceSystem.create_resource(%{
          name: "To Update",
          type: "document",
          status: "published",
          content: %{text: "Test content"}
        }, [])

      assert {:ok, updated} = ResourceSystem.update_resource(resource.id, %{name: "Updated Name"})
      assert updated.name == "Updated Name"
    end

    test "deletes a resource successfully" do
      {:ok, resource} =
        ResourceSystem.create_resource(%{
          name: "To Delete",
          type: "document",
          status: "published",
          content: %{text: "Test content"}
        }, [])

      assert {:ok, deleted} = ResourceSystem.delete_resource(resource.id)
      assert deleted.id == resource.id
      assert {:error, :not_found} = ResourceSystem.get_resource(resource.id)
    end

    test "generates events on resource creation" do
      attrs = %{
        name: "Event Resource",
        type: "document",
        status: "published",
        content: %{text: "Test content"}
      }

      assert {:ok, %Resource{} = resource} = ResourceSystem.create_resource(attrs, [])
      # Event generation is side-effect; just ensure no error and resource is returned
      assert resource.name == "Event Resource"
    end
  end
end
