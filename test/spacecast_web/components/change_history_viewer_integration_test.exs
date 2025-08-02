defmodule SpacecastWeb.Components.ChangeHistoryViewerIntegrationTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.ChangeHistoryViewer

  describe "ChangeHistoryViewer integration" do
    test "renders with timeline view mode" do
      change_history = [
        %{
          version: 2,
          timestamp: "2024-01-15T09:15:00Z",
          description: "Added email validation",
          changes: [%{field: :email, action: "added"}]
        },
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation",
          changes: [%{field: :id, action: "created"}]
        }
      ]

      html =
        ChangeHistoryViewer.render(%{
          resource: %{__change_history__: change_history},
          selected_version: 2,
          view_mode: "timeline",
          on_view_version: "view_version",
          on_diff_versions: "diff_versions"
        })

      # Should render timeline view
      assert html =~ "timeline-view"
      assert html =~ "Version 2"
      assert html =~ "Version 1"
      assert html =~ "Added email validation"
      assert html =~ "Initial user creation"
    end

    test "renders with list view mode" do
      change_history = [
        %{
          version: 2,
          timestamp: "2024-01-15T09:15:00Z",
          description: "Added email validation",
          changes: [%{field: :email, action: "added"}]
        },
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation",
          changes: [%{field: :id, action: "created"}]
        }
      ]

      html =
        ChangeHistoryViewer.render(%{
          resource: %{__change_history__: change_history},
          selected_version: 2,
          view_mode: "list",
          on_view_version: "view_version",
          on_diff_versions: "diff_versions"
        })

      # Should render list view
      assert html =~ "list-view"
      assert html =~ "v2"
      assert html =~ "v1"
      assert html =~ "Added email validation"
      assert html =~ "Initial user creation"
    end

    test "handles resource without change history" do
      html =
        ChangeHistoryViewer.render(%{
          resource: %{},
          selected_version: nil,
          view_mode: "timeline",
          on_view_version: "view_version",
          on_diff_versions: "diff_versions"
        })

      # Should show no history message
      assert html =~ "No change history available"
    end

    test "handles empty change history" do
      html =
        ChangeHistoryViewer.render(%{
          resource: %{__change_history__: []},
          selected_version: nil,
          view_mode: "timeline",
          on_view_version: "view_version",
          on_diff_versions: "diff_versions"
        })

      # Should show no history message
      assert html =~ "No change history available"
    end

    test "switches between view modes" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation",
          changes: []
        }
      ]

      # Test timeline mode
      timeline_html =
        ChangeHistoryViewer.render(%{
          resource: %{__change_history__: change_history},
          selected_version: nil,
          view_mode: "timeline",
          on_view_version: "view_version",
          on_diff_versions: "diff_versions"
        })

      assert timeline_html =~ "timeline-view"
      refute timeline_html =~ "list-view"

      # Test list mode
      list_html =
        ChangeHistoryViewer.render(%{
          resource: %{__change_history__: change_history},
          selected_version: nil,
          view_mode: "list",
          on_view_version: "view_version",
          on_diff_versions: "diff_versions"
        })

      assert list_html =~ "list-view"
      refute list_html =~ "timeline-view"
    end

    test "passes correct props to child components" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Test",
          changes: []
        }
      ]

      html =
        ChangeHistoryViewer.render(%{
          resource: %{__change_history__: change_history},
          selected_version: 1,
          view_mode: "timeline",
          on_view_version: "custom_view",
          on_diff_versions: "custom_diff"
        })

      # Should pass the custom event names to child components
      assert html =~ "custom_view"
      assert html =~ "custom_diff"
      assert html =~ "phx-value-version=\"1\""
    end

    test "handles missing event handlers gracefully" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Test",
          changes: []
        }
      ]

      html =
        ChangeHistoryViewer.render(%{
          resource: %{__change_history__: change_history},
          selected_version: nil,
          view_mode: "timeline",
          on_view_version: nil,
          on_diff_versions: nil
        })

      # Should render without errors even with nil handlers
      assert html =~ "timeline-view"
      assert html =~ "Version 1"
    end
  end
end
