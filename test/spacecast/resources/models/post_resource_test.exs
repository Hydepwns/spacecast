defmodule Spacecast.Resources.PostResourceTest do
  use Spacecast.DataCase
  alias Spacecast.TestSupport.EventStoreTestHelper
  alias Spacecast.Resources.TestPostResource

  import Spacecast.Events.ResourceIntegration.EventSourcedResource,
    only: [rebuild_from_events: 3]

  setup do
    EventStoreTestHelper.setup_mock_event_store()
    :ok
  end

  describe "post resource event sourcing" do
    test "creates post and generates created event" do
      post_params = %{
        id: "test-post-1",
        title: "Test Post",
        content: "This is a test post content",
        published: false,
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        author_id: "user-1",
        team_id: "team-1"
      }

      {:ok, post} = TestPostResource.create(post_params)

      assert post.id == "test-post-1"
      assert post.title == "Test Post"
      assert post.content == "This is a test post content"
      assert post.published == false
      assert post.author_id == "user-1"
      assert post.team_id == "team-1"

      EventStoreTestHelper.assert_event_exists("post.created", TestPostResource, "test-post-1")
    end

    test "updates post and generates updated event" do
      post_params = %{
        id: "test-post-2",
        title: "Original Title",
        content: "Original content",
        published: false,
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        author_id: "user-1",
        team_id: "team-1"
      }

      {:ok, _post} = TestPostResource.create(post_params)
      update_params = %{title: "Updated Title", content: "Updated content", published: true}
      {:ok, updated_post} = TestPostResource.update("test-post-2", update_params)

      assert updated_post.title == "Updated Title"
      assert updated_post.content == "Updated content"
      assert updated_post.published == true

      EventStoreTestHelper.assert_event_exists("post.updated", TestPostResource, "test-post-2")
    end

    test "deletes post and generates deleted event" do
      post_params = %{
        id: "test-post-3",
        title: "To Delete",
        content: "Will be deleted",
        published: true,
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        author_id: "user-1",
        team_id: "team-1"
      }

      {:ok, _post} = TestPostResource.create(post_params)
      {:ok, events} = TestPostResource.delete("test-post-3", %{})

      assert length(events) == 1
      assert hd(events).type == "post.deleted"

      EventStoreTestHelper.assert_event_exists("post.deleted", TestPostResource, "test-post-3")
    end

    test "rebuilds post state from events" do
      post_params = %{
        id: "test-post-4",
        title: "Original Title",
        content: "Original content",
        published: false,
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now(),
        author_id: "user-1",
        team_id: "team-1"
      }

      {:ok, _post} = TestPostResource.create(post_params)

      {:ok, _updated_post} =
        TestPostResource.update("test-post-4", %{title: "Updated Title", published: true})

      {:ok, _events} = TestPostResource.delete("test-post-4", %{})

      # Get events from the store and rebuild state
      {:ok, events} = TestPostResource.get_history("test-post-4")

      rebuilt =
        rebuild_from_events(
          events,
          TestPostResource.initial_state(),
          &TestPostResource.apply_event/2
        )

      assert rebuilt.title == "Updated Title"
      # deleted sets published to false
      assert rebuilt.published == false
    end
  end
end
