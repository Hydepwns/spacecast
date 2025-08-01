defmodule Spacecast.Resources.TeamResourceTest do
  use Spacecast.DataCase
  alias Spacecast.TestSupport.EventStoreTestHelper
  alias Spacecast.Resources.TestTeamResource

  import Spacecast.Events.ResourceIntegration.EventSourcedResource,
    only: [rebuild_from_events: 3]

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "team resource event sourcing" do
    test "creates team and generates created event" do
      team_params = %{
        id: "test-team-1",
        name: "Test Team",
        description: "A test team",
        created_at: DateTime.utc_now(),
        active: true
      }

      {:ok, team} = TestTeamResource.create(team_params)

      assert team.id == "test-team-1"
      assert team.name == "Test Team"
      assert team.description == "A test team"
      assert team.active == true

      EventStoreTestHelper.assert_event_exists("team.created", TestTeamResource, "test-team-1")
    end

    test "updates team and generates updated event" do
      team_params = %{
        id: "test-team-2",
        name: "Original Team",
        description: "Original description",
        created_at: DateTime.utc_now(),
        active: true
      }

      {:ok, _team} = TestTeamResource.create(team_params)
      update_params = %{name: "Updated Team", description: "Updated description"}
      {:ok, updated_team} = TestTeamResource.update("test-team-2", update_params)

      assert updated_team.name == "Updated Team"
      assert updated_team.description == "Updated description"

      EventStoreTestHelper.assert_event_exists("team.updated", TestTeamResource, "test-team-2")
    end

    test "deletes team and generates deleted event" do
      team_params = %{
        id: "test-team-3",
        name: "To Delete",
        description: "Will be deleted",
        created_at: DateTime.utc_now(),
        active: true
      }

      {:ok, _team} = TestTeamResource.create(team_params)
      {:ok, events} = TestTeamResource.delete("test-team-3", %{})

      assert length(events) == 1
      assert hd(events).type == "team.deleted"

      EventStoreTestHelper.assert_event_exists("team.deleted", TestTeamResource, "test-team-3")
    end

    test "rebuilds team state from events" do
      team_params = %{
        id: "test-team-4",
        name: "Original Team",
        description: "Original description",
        created_at: DateTime.utc_now(),
        active: true
      }

      {:ok, _team} = TestTeamResource.create(team_params)
      {:ok, _updated_team} = TestTeamResource.update("test-team-4", %{name: "Updated Team"})
      {:ok, _events} = TestTeamResource.delete("test-team-4", %{})

      # Get events from the store and rebuild state
      {:ok, events} = TestTeamResource.get_history("test-team-4")

      rebuilt =
        rebuild_from_events(
          events,
          TestTeamResource.initial_state(),
          &TestTeamResource.apply_event/2
        )

      assert rebuilt.name == "Updated Team"
      assert rebuilt.active == false
    end
  end
end
