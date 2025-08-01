defmodule Spacecast.Resources.ExampleUserResourceTest do
  use Spacecast.DataCase
  alias Spacecast.TestSupport.EventStoreTestHelper
  alias Spacecast.Resources.TestExampleUserResource

  import Spacecast.Events.ResourceIntegration.EventSourcedResource,
    only: [rebuild_from_events: 3]

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "example user resource event sourcing" do
    test "creates example user and generates created event" do
      user_params = %{
        id: "test-example-user-1",
        email: "test@example.com",
        name: "Test User",
        status: "active",
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        metadata: %{role: "viewer"}
      }

      {:ok, user} = TestExampleUserResource.create(user_params)

      assert user.id == "test-example-user-1"
      assert user.email == "test@example.com"
      assert user.name == "Test User"
      assert user.status == "active"
      assert user.metadata == %{role: "viewer"}

      EventStoreTestHelper.assert_event_exists(
        "user.created",
        TestExampleUserResource,
        "test-example-user-1"
      )
    end

    test "updates example user and generates updated event" do
      user_params = %{
        id: "test-example-user-2",
        email: "original@example.com",
        name: "Original Name",
        status: "active",
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        metadata: %{}
      }

      {:ok, _user} = TestExampleUserResource.create(user_params)
      update_params = %{email: "updated@example.com", name: "Updated Name"}
      {:ok, updated_user} = TestExampleUserResource.update("test-example-user-2", update_params)

      assert updated_user.email == "updated@example.com"
      assert updated_user.name == "Updated Name"

      EventStoreTestHelper.assert_event_exists(
        "user.updated",
        TestExampleUserResource,
        "test-example-user-2"
      )
    end

    test "deletes example user and generates deleted event" do
      user_params = %{
        id: "test-example-user-3",
        email: "delete@example.com",
        name: "To Delete",
        status: "active",
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        metadata: %{}
      }

      {:ok, _user} = TestExampleUserResource.create(user_params)
      {:ok, events} = TestExampleUserResource.delete("test-example-user-3", %{})

      assert length(events) == 1
      assert hd(events).type == "user.deleted"

      EventStoreTestHelper.assert_event_exists(
        "user.deleted",
        TestExampleUserResource,
        "test-example-user-3"
      )
    end

    test "rebuilds example user state from events" do
      user_params = %{
        id: "test-example-user-4",
        email: "original@example.com",
        name: "Original Name",
        status: "active",
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        metadata: %{}
      }

      {:ok, _user} = TestExampleUserResource.create(user_params)

      {:ok, _updated_user} =
        TestExampleUserResource.update("test-example-user-4", %{name: "Updated Name"})

      {:ok, _events} = TestExampleUserResource.delete("test-example-user-4", %{})

      # Get events from the store and rebuild state
      {:ok, events} = TestExampleUserResource.get_history("test-example-user-4")

      rebuilt =
        rebuild_from_events(
          events,
          TestExampleUserResource.initial_state(),
          &TestExampleUserResource.apply_event/2
        )

      assert rebuilt.name == "Updated Name"
      assert rebuilt.status == "deleted"
    end
  end
end
