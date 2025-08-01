defmodule Spacecast.Resources.UserResourceTest do
  use Spacecast.DataCase
  alias Spacecast.TestSupport.EventStoreTestHelper
  alias Spacecast.Resources.TestUserResource

  import Spacecast.Events.ResourceIntegration.EventSourcedResource,
    only: [rebuild_from_events: 3]

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "user resource event sourcing" do
    test "creates user and generates created event" do
      user_params = %{
        id: "test-user-1",
        name: "Test User",
        email: "test@example.com",
        role: "viewer",
        settings: %{theme: "system", notifications: true, sidebar_collapsed: false},
        permissions: [],
        active: true,
        last_login: nil,
        team_id: nil
      }

      {:ok, user} = TestUserResource.create(user_params)

      assert user.id == "test-user-1"
      assert user.name == "Test User"
      assert user.email == "test@example.com"
      assert user.active == true

      EventStoreTestHelper.assert_event_exists("user.created", TestUserResource, "test-user-1")
    end

    test "updates user and generates updated event" do
      user_params = %{
        id: "test-user-2",
        name: "Original Name",
        email: "original@example.com",
        role: "viewer",
        settings: %{theme: "system", notifications: true, sidebar_collapsed: false},
        permissions: [],
        active: true,
        last_login: nil,
        team_id: nil
      }

      {:ok, _user} = TestUserResource.create(user_params)
      update_params = %{name: "Updated Name", email: "updated@example.com"}
      {:ok, updated_user} = TestUserResource.update("test-user-2", update_params)

      assert updated_user.name == "Updated Name"
      assert updated_user.email == "updated@example.com"

      EventStoreTestHelper.assert_event_exists("user.updated", TestUserResource, "test-user-2")
    end

    test "deletes user and generates deleted event" do
      user_params = %{
        id: "test-user-3",
        name: "To Delete",
        email: "delete@example.com",
        role: "viewer",
        settings: %{theme: "system", notifications: true, sidebar_collapsed: false},
        permissions: [],
        active: true,
        last_login: nil,
        team_id: nil
      }

      {:ok, _user} = TestUserResource.create(user_params)
      {:ok, events} = TestUserResource.delete("test-user-3", %{})

      assert length(events) == 1
      assert hd(events).type == "user.deleted"

      EventStoreTestHelper.assert_event_exists("user.deleted", TestUserResource, "test-user-3")
    end

    test "rebuilds user state from events" do
      user_params = %{
        id: "test-user-4",
        name: "Original Name",
        email: "original@example.com",
        role: "viewer",
        settings: %{theme: "system", notifications: true, sidebar_collapsed: false},
        permissions: [],
        active: true,
        last_login: nil,
        team_id: nil
      }

      {:ok, _user} = TestUserResource.create(user_params)
      {:ok, _updated_user} = TestUserResource.update("test-user-4", %{name: "Updated Name"})
      {:ok, _events} = TestUserResource.delete("test-user-4", %{})

      # Get events from the store and rebuild state
      {:ok, events} = TestUserResource.get_history("test-user-4")

      rebuilt =
        rebuild_from_events(
          events,
          TestUserResource.initial_state(),
          &TestUserResource.apply_event/2
        )

      assert rebuilt.name == "Updated Name"
      assert rebuilt.active == false
    end
  end
end
