defmodule SpacecastWeb.Components.UI.ListViewTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.LiveViewTest
  alias SpacecastWeb.Components.UI.ListView

  describe "ListView component" do
    test "renders list with change history" do
      change_history = [
        %{
          version: 3,
          timestamp: "2024-01-15T10:30:00Z",
          description: "Updated user profile",
          changes: [%{field: :name, action: "updated"}]
        },
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

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: 2,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html =~ "v3"
      assert html =~ "v2"
      assert html =~ "v1"
      assert html =~ "Updated user profile"
      assert html =~ "Added email validation"
      assert html =~ "Initial user creation"
      assert html =~ "view_version"
      assert html =~ "diff_versions"
    end

    test "renders list without selected version" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation",
          changes: []
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html =~ "v1"
      refute html =~ "Selected"
    end

    test "renders list with selected version highlighted" do
      change_history = [
        %{
          version: 2,
          timestamp: "2024-01-15T09:15:00Z",
          description: "Added email validation",
          changes: []
        },
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation",
          changes: []
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: 2,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html =~ "Selected"
      assert html =~ "phx-value-version=\"2\""
    end

    test "renders list without view version handler" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation",
          changes: []
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: nil,
        on_diff_versions: "diff_versions"
      })

      assert html =~ "disabled"
    end

    test "renders list without diff versions handler" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Initial user creation",
          changes: []
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: nil
      })

      assert html =~ "disabled"
    end

    test "handles empty change history" do
      html = ListView.render_list_view(%{
        change_history: [],
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      refute html =~ "v1"
      refute html =~ "list-row"
    end

    test "formats different timestamp types" do
      # Test with string timestamp
      change_history_string = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Test",
          changes: []
        }
      ]

      html_string = ListView.render_list_view(%{
        change_history: change_history_string,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html_string =~ "2024-01-15 08:00"

      # Test with DateTime timestamp
      datetime = DateTime.new!(~D[2024-01-15], ~T[08:00:00], "Etc/UTC")
      change_history_datetime = [
        %{
          version: 1,
          timestamp: datetime,
          description: "Test",
          changes: []
        }
      ]

      html_datetime = ListView.render_list_view(%{
        change_history: change_history_datetime,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html_datetime =~ "2024-01-15 08:00"
    end

    test "handles missing description gracefully" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: nil,
          changes: []
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html =~ "Change made to resource"
    end

    test "formats changes correctly" do
      change_history = [
        %{
          version: 1,
          timestamp: "2024-01-15T08:00:00Z",
          description: "Test",
          changes: [
            %{field: :name, action: "updated"},
            %{field: :email, action: "added"},
            %{field: :age}
          ]
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html =~ "updated name"
      assert html =~ "added email"
      assert html =~ "Modified age"
    end

    test "handles invalid timestamp gracefully" do
      change_history = [
        %{
          version: 1,
          timestamp: "invalid-timestamp",
          description: "Test",
          changes: []
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html =~ "invalid-timestamp"
    end

    test "handles unknown timestamp type gracefully" do
      change_history = [
        %{
          version: 1,
          timestamp: :unknown,
          description: "Test",
          changes: []
        }
      ]

      html = ListView.render_list_view(%{
        change_history: change_history,
        selected_version: nil,
        on_view_version: "view_version",
        on_diff_versions: "diff_versions"
      })

      assert html =~ "Unknown"
    end
  end
end
