defmodule SpacecastWeb.Components.UI.TimelineViewTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.UI.TimelineView

  describe "TimelineView component" do
    test "renders timeline with change history" do
      change_history = [
        %{
          version: 3,
          timestamp: "2024-01-15T10:30:00Z",
          description: "Updated user profile"
        },
        %{
          version: 2,
          timestamp: "2024-01-15T09:15:00Z",
          description: "Added email validation"
        },
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation"
        }
      ]

      html = TimelineView.render_timeline_view(%{
        change_history: change_history,
        selected_version: 2,
        on_view_version: "view_version"
      })

      assert html =~ "Version 3"
      assert html =~ "Version 2"
      assert html =~ "Version 1"
      assert html =~ "Updated user profile"
      assert html =~ "Added email validation"
      assert html =~ "Initial user creation"
      assert html =~ "view_version"
    end

    test "renders timeline without selected version" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation"
        }
      ]

      html = TimelineView.render_timeline_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version"
      })

      assert html =~ "Version 1"
      refute html =~ "Currently Selected"
    end

    test "renders timeline with selected version highlighted" do
      change_history = [
        %{
          version: 2,
          timestamp: "2024-01-15T09:15:00Z",
          description: "Added email validation"
        },
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation"
        }
      ]

      html = TimelineView.render_timeline_view(%{
        change_history: change_history,
        selected_version: 2,
        on_view_version: "view_version"
      })

      assert html =~ "Currently Selected"
      assert html =~ "phx-value-version=\"2\""
    end

    test "renders timeline without view version handler" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation"
        }
      ]

      html = TimelineView.render_timeline_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: nil
      })

      assert html =~ "disabled"
    end

    test "handles empty change history" do
      html = TimelineView.render_timeline_view(%{
        change_history: [],
        selected_version: nil,
        on_view_version: "view_version"
      })

      refute html =~ "Version"
      refute html =~ "timeline-item"
    end

    test "formats different timestamp types" do
      # Test with string timestamp
      change_history_string = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Test"
        }
      ]

      html_string = TimelineView.render_timeline_view(%{
        change_history: change_history_string,
        selected_version: nil,
        on_view_version: "view_version"
      })

      assert html_string =~ "2024-01-15 08:00:00"

      # Test with DateTime timestamp
      datetime = DateTime.new!(~D[2024-01-15], ~T[08:00:00], "Etc/UTC")
      change_history_datetime = [
        %{
          version: 1,
          timestamp: datetime,
          description: "Test"
        }
      ]

      html_datetime = TimelineView.render_timeline_view(%{
        change_history: change_history_datetime,
        selected_version: nil,
        on_view_version: "view_version"
      })

      assert html_datetime =~ "2024-01-15 08:00:00"
    end

    test "handles missing description gracefully" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: nil
        }
      ]

      html = TimelineView.render_timeline_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version"
      })

      assert html =~ "Change made to resource"
    end

    test "handles invalid timestamp gracefully" do
      change_history = [
        %{
          version: 1,
          timestamp: "invalid-timestamp",
          description: "Test"
        }
      ]

      html = TimelineView.render_timeline_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version"
      })

      assert html =~ "invalid-timestamp"
    end
  end
end
