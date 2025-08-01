defmodule Spacecast.Utils.ChangeTrackerTest do
  use ExUnit.Case, async: true
  alias Spacecast.Utils.ChangeTracker

  describe "track_change/3" do
    test "tracks changes to a resource" do
      resource = %{
        id: "test-id",
        name: "Original Name",
        email: "original@example.com",
        active: true,
        settings: %{
          theme: "light",
          notifications: true
        }
      }

      changes = %{
        name: "New Name",
        email: "new@example.com"
      }

      metadata = %{
        actor: "test@example.com",
        reason: "Test update",
        source: "test"
      }

      {:ok, updated_resource} = ChangeTracker.track_change(resource, changes, metadata)

      # Check that changes were applied
      assert updated_resource.name == "New Name"
      assert updated_resource.email == "new@example.com"

      # Check that history was created
      assert updated_resource.__change_history__ != nil
      assert length(updated_resource.__change_history__) == 1

      # Check history record
      [change] = updated_resource.__change_history__
      assert change.version == 1
      assert change.before == resource
      assert change.changes == changes
      assert change.metadata.actor == "test@example.com"
      assert change.metadata.reason == "Test update"
      assert change.metadata.source == "test"
      assert change.metadata.timestamp != nil
    end

    test "supports selective field tracking" do
      resource = %{
        id: "test-id",
        name: "Original Name",
        email: "original@example.com",
        role: "user"
      }

      changes = %{
        name: "New Name",
        email: "new@example.com",
        role: "admin"
      }

      # Track only name and role changes
      metadata = %{
        tracked_fields: [:name, :role]
      }

      {:ok, updated_resource} = ChangeTracker.track_change(resource, changes, metadata)

      # All changes should be applied to the resource
      assert updated_resource.name == "New Name"
      assert updated_resource.email == "new@example.com"
      assert updated_resource.role == "admin"

      # But only tracked fields should be in the change record
      [change] = updated_resource.__change_history__
      assert Map.has_key?(change.changes, :name)
      assert Map.has_key?(change.changes, :role)
      refute Map.has_key?(change.changes, :email)
    end
  end

  describe "diff/2" do
    test "creates a diff between simple resources" do
      initial_resource_state = %{
        id: "123",
        name: "Original Name",
        email: "original@example.com"
      }

      # State at version 1 (same as initial for this test setup)
      resource_v1_data = initial_resource_state

      changes_to_v2 = %{name: "New Name"}

      # State at version 2
      resource_v2_data = Map.merge(resource_v1_data, changes_to_v2)

      # Construct a resource as if it's currently at version 2
      resource_at_v2 =
        resource_v2_data
        |> Map.put(:__change_history__, [
          %{version: 2, before: resource_v1_data, changes: changes_to_v2, metadata: %{}},
          # Simplified initial version
          %{version: 1, before: %{}, changes: resource_v1_data, metadata: %{}}
        ])

      {:ok, diff} = ChangeTracker.diff(resource_at_v2, version1: 1, version2: 2)

      assert Map.has_key?(diff, :name)
      assert diff.name.before == "Original Name"
      assert diff.name.after == "New Name"
      # No change
      refute Map.has_key?(diff, :email)
    end

    test "creates a deep diff for nested structures" do
      resource_v1_data = %{
        id: "123",
        settings: %{
          theme: "light",
          notifications: true,
          preferences: %{
            language: "en"
          }
        },
        tags: ["tag1", "tag2"]
      }

      changes_to_v2 = %{
        settings: %{
          # changed
          theme: "dark",
          # same
          notifications: true,
          preferences: %{
            # changed
            language: "fr"
          }
        },
        # changed
        tags: ["tag1", "tag3"]
      }

      resource_v2_data = Map.merge(resource_v1_data, changes_to_v2)

      # Simulate tracking this change to build a resource with history
      # This will be our resource_at_v2
      {:ok, _tracked_resource_to_v2} =
        ChangeTracker.track_change(resource_v1_data, changes_to_v2, %{actor: "test"})

      # To make the test more direct for diffing v1 and v2 from tracked_resource_to_v2,
      # its history should be set up as if it already had a v1.
      # The track_change above creates a history entry for the transition from v1_data to v2_data.
      # Its history will look like: [%{version: 1, before: resource_v1_data, changes: changes_to_v2, ...}]
      # This is good for testing diff(tracked_resource_to_v2) which implies diff from version 0 to 1 (in this case)

      # For diffing version 1 and 2 explicitly, we need a resource that has both versions in its history correctly.
      # Let's assume an initial empty state for version 0.
      initial_empty_resource = %{id: "123"}

      {:ok, resource_after_v1_changes} =
        ChangeTracker.track_change(initial_empty_resource, resource_v1_data)

      {:ok, resource_after_v2_changes} =
        ChangeTracker.track_change(resource_after_v1_changes, changes_to_v2)

      # Now resource_after_v2_changes has a history like:
      # [ {v:2, before: v1_state, changes: to_v2}, {v:1, before: empty, changes: to_v1_state} ]

      {:ok, diff} = ChangeTracker.diff(resource_after_v2_changes, version1: 1, version2: 2)

      # Check settings diff
      assert diff.settings.before == resource_v1_data.settings
      assert diff.settings.after == resource_v2_data.settings

      # Check nested diff
      nested_diff = diff.settings.nested_diff
      assert nested_diff.theme.before == "light"
      assert nested_diff.theme.after == "dark"

      # Check deeply nested diff
      assert nested_diff.preferences.nested_diff.language.before == "en"
      assert nested_diff.preferences.nested_diff.language.after == "fr"

      # Check array diff
      assert diff.tags.before == ["tag1", "tag2"]
      assert diff.tags.after == ["tag1", "tag3"]
    end
  end
end
