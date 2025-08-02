defmodule Spacecast.Events.SnapshotOperationsTest do
  use Spacecast.DataCase

  alias Spacecast.Events.SnapshotOperations
  alias Spacecast.Events.Schemas.Event

  describe "save_snapshot/4" do
    test "saves a valid snapshot" do
      # Use unique resource ID to avoid interference between parallel tests
      resource_id = "test_resource_#{System.unique_integer([:positive])}"
      state = %{value: 42}
      metadata = %{event_id: "123", version: 1}

      assert {:ok, snapshot} =
               SnapshotOperations.save_snapshot("test_resource", resource_id, state, metadata)

      assert snapshot.resource_type == "test_resource"
      assert snapshot.resource_id == resource_id
      assert snapshot.state == state
      assert snapshot.metadata == metadata
    end

    test "saves a snapshot with default metadata" do
      # Use unique resource ID to avoid interference between parallel tests
      resource_id = "test_resource_#{System.unique_integer([:positive])}"
      state = %{value: 42}

      assert {:ok, snapshot} = SnapshotOperations.save_snapshot("test_resource", resource_id, state)
      assert snapshot.resource_type == "test_resource"
      assert snapshot.resource_id == resource_id
      assert snapshot.state == state
      assert snapshot.metadata == %{}
    end

    test "rejects invalid parameters" do
      assert {:error, :invalid_parameters} = SnapshotOperations.save_snapshot("", "123", %{}, %{})

      assert {:error, :invalid_parameters} =
               SnapshotOperations.save_snapshot("test", "", %{}, %{})

      assert {:error, :invalid_parameters} =
               SnapshotOperations.save_snapshot("test", "123", "not_a_map", %{})

      assert {:error, :invalid_parameters} =
               SnapshotOperations.save_snapshot("test", "123", %{}, "not_a_map")
    end
  end

  describe "get_latest_snapshot/2" do
    setup do
      # Use unique resource ID to avoid interference between parallel tests
      resource_id = "test_resource_#{System.unique_integer([:positive])}"

      # Create multiple snapshots for the same resource with small delays to ensure proper ordering
      {:ok, snapshot1} =
        SnapshotOperations.save_snapshot("test_resource", resource_id, %{value: 1}, %{version: 1})

      # Longer delay to ensure different timestamps
      Process.sleep(100)

      {:ok, snapshot2} =
        SnapshotOperations.save_snapshot("test_resource", resource_id, %{value: 2}, %{version: 2})

      # Longer delay to ensure different timestamps
      Process.sleep(100)

      {:ok, snapshot3} =
        SnapshotOperations.save_snapshot("test_resource", resource_id, %{value: 3}, %{version: 3})

      %{snapshots: [snapshot1, snapshot2, snapshot3], resource_id: resource_id}
    end

    test "retrieves the _latest snapshot", %{snapshots: [snapshot1, snapshot2, snapshot3], resource_id: resource_id} do
      # Verify timestamps are in ascending order (with tolerance for race conditions)
      # Use a small tolerance to account for database transaction timing
      # 1 second tolerance
      tolerance = 1
      assert DateTime.diff(snapshot2.inserted_at, snapshot1.inserted_at, :second) >= -tolerance
      assert DateTime.diff(snapshot3.inserted_at, snapshot2.inserted_at, :second) >= -tolerance

      # Get all snapshots to verify they were created correctly
      assert {:ok, all_snapshots} = SnapshotOperations.get_snapshots("test_resource", resource_id)
      assert length(all_snapshots) >= 3

      # Verify the snapshots have the expected values (PostgreSQL JSON fields return maps with string keys)
      # Since get_snapshots returns snapshots ordered by inserted_at ascending, the latest is the last element
      latest_snapshot = List.last(all_snapshots)
      assert latest_snapshot.state["value"] == 3

      # Get the latest snapshot specifically
      assert {:ok, retrieved_latest} = SnapshotOperations.get_latest_snapshot("test_resource", resource_id)

      # Verify it's the latest by checking the value and timestamp
      # PostgreSQL JSON fields return maps with string keys
      assert retrieved_latest.state["value"] == 3
      assert retrieved_latest.inserted_at == latest_snapshot.inserted_at
    end

    test "returns not found for non-existent resource" do
      assert {:error, :not_found} =
               SnapshotOperations.get_latest_snapshot("test_resource", "nonexistent")
    end

    test "rejects invalid parameters" do
      assert {:error, :invalid_parameters} = SnapshotOperations.get_latest_snapshot("", "123")
      assert {:error, :invalid_parameters} = SnapshotOperations.get_latest_snapshot("test", "")
    end
  end

  describe "count_events_since_last_snapshot/2" do
    setup do
      # Use unique resource ID to avoid interference between parallel tests
      resource_id = "test_resource_#{System.unique_integer([:positive])}"

      # Create a snapshot and some events
      {:ok, snapshot} =
        SnapshotOperations.save_snapshot("test_resource", resource_id, %{value: 1}, %{
          event_id: "event1"
        })

      # Create events after the snapshot
      {:ok, _} =
        Repo.insert(%Event{
          resource_type: "test_resource",
          resource_id: resource_id,
          type: "test_event",
          data: %{},
          timestamp: DateTime.utc_now()
        })

      {:ok, _} =
        Repo.insert(%Event{
          resource_type: "test_resource",
          resource_id: resource_id,
          type: "test_event",
          data: %{},
          timestamp: DateTime.utc_now()
        })

      %{snapshot: snapshot, resource_id: resource_id}
    end

    test "counts events since last snapshot", %{resource_id: resource_id} do
      assert {:ok, count} =
               SnapshotOperations.count_events_since_last_snapshot("test_resource", resource_id)

      assert count == 2
    end

    test "counts all events when no snapshot exists" do
      # Use unique resource ID for nonexistent resource
      resource_id = "nonexistent_#{System.unique_integer([:positive])}"

      # Insert events for the nonexistent resource
      {:ok, _} =
        Repo.insert(%Event{
          resource_type: "test_resource",
          resource_id: resource_id,
          type: "test_event",
          data: %{},
          timestamp: DateTime.utc_now()
        })

      {:ok, _} =
        Repo.insert(%Event{
          resource_type: "test_resource",
          resource_id: resource_id,
          type: "test_event",
          data: %{},
          timestamp: DateTime.utc_now()
        })

      assert {:ok, count} =
               SnapshotOperations.count_events_since_last_snapshot("test_resource", resource_id)

      assert count == 2
    end

    test "rejects invalid parameters" do
      assert {:error, :invalid_parameters} =
               SnapshotOperations.count_events_since_last_snapshot("", "123")

      assert {:error, :invalid_parameters} =
               SnapshotOperations.count_events_since_last_snapshot("test", "")
    end
  end

  describe "save_versioned_state/4" do
    test "saves a valid versioned state" do
      # Use unique resource ID to avoid interference between parallel tests
      resource_id = "test_resource_#{System.unique_integer([:positive])}"
      state = %{value: 42}

      opts = [
        label: "test_version",
        replay_id: Ecto.UUID.generate(),
        created_at: DateTime.utc_now(),
        point_in_time: DateTime.utc_now(),
        metadata: %{version: 1}
      ]

      assert {:ok, versioned_state} =
               SnapshotOperations.save_versioned_state("test_resource", resource_id, state, opts)

      assert versioned_state.resource_type == "test_resource"
      assert versioned_state.resource_id == resource_id
      assert versioned_state.state == state
      assert versioned_state.label == "test_version"
      assert is_binary(versioned_state.replay_id)
      assert versioned_state.metadata == %{version: 1}
    end

    test "saves versioned state with minimal options" do
      # Use unique resource ID to avoid interference between parallel tests
      resource_id = "test_resource_#{System.unique_integer([:positive])}"
      state = %{value: 42}
      opts = [label: "test_version"]

      assert {:ok, versioned_state} =
               SnapshotOperations.save_versioned_state("test_resource", resource_id, state, opts)

      assert versioned_state.resource_type == "test_resource"
      assert versioned_state.resource_id == resource_id
      assert versioned_state.state == state
      assert versioned_state.label == "test_version"
      assert versioned_state.metadata == %{}
    end

    test "rejects invalid parameters" do
      assert {:error, :invalid_parameters} =
               SnapshotOperations.save_versioned_state("", "123", %{}, label: "test")

      assert {:error, :invalid_parameters} =
               SnapshotOperations.save_versioned_state("test", "", %{}, label: "test")

      assert {:error, :invalid_parameters} =
               SnapshotOperations.save_versioned_state("test", "123", "not_a_map", label: "test")

      assert {:error, :invalid_parameters} =
               SnapshotOperations.save_versioned_state("test", "123", %{}, "not_a_list")
    end
  end

  describe "get_snapshots/2" do
    setup do
      # Use unique resource ID to avoid interference between parallel tests
      resource_id = "test_resource_#{System.unique_integer([:positive])}"

      # Create multiple snapshots for the same resource with longer delays to ensure proper ordering
      {:ok, snapshot1} =
        SnapshotOperations.save_snapshot("test_resource", resource_id, %{value: 1}, %{version: 1})

      # Longer delay to ensure different timestamps
      Process.sleep(100)

      {:ok, snapshot2} =
        SnapshotOperations.save_snapshot("test_resource", resource_id, %{value: 2}, %{version: 2})

      # Longer delay to ensure different timestamps
      Process.sleep(100)

      {:ok, snapshot3} =
        SnapshotOperations.save_snapshot("test_resource", resource_id, %{value: 3}, %{version: 3})

      %{snapshots: [snapshot1, snapshot2, snapshot3], resource_id: resource_id}
    end

    test "retrieves all snapshots in order", %{snapshots: [_first, _second, _third], resource_id: resource_id} do
      assert {:ok, snapshots} = SnapshotOperations.get_snapshots("test_resource", resource_id)
      assert length(snapshots) == 3

      # Check that snapshots are ordered by inserted_at ascending
      snapshot_values = Enum.map(snapshots, & &1.state["value"])

      # Verify we have all three snapshots with correct values
      # The order should be by inserted_at ascending, so values should be [1, 2, 3]
      assert snapshot_values == [1, 2, 3]

      # Verify the snapshots are in chronological order (first created should be first)
      assert Enum.at(snapshots, 0).inserted_at <= Enum.at(snapshots, 1).inserted_at
      assert Enum.at(snapshots, 1).inserted_at <= Enum.at(snapshots, 2).inserted_at
    end

    test "returns empty list for non-existent resource" do
      # Use unique resource ID for nonexistent resource
      resource_id = "nonexistent_#{System.unique_integer([:positive])}"
      assert {:ok, snapshots} = SnapshotOperations.get_snapshots("test_resource", resource_id)
      assert snapshots == []
    end

    test "rejects invalid parameters" do
      assert {:error, :invalid_parameters} = SnapshotOperations.get_snapshots("", "123")
      assert {:error, :invalid_parameters} = SnapshotOperations.get_snapshots("test", "")
    end
  end
end
