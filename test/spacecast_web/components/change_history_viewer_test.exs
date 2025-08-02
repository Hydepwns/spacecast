defmodule SpacecastWeb.Components.ChangeHistoryViewerTest do
  use SpacecastWeb.ConnCase, async: true
  import Phoenix.ConnTest
  import Phoenix.LiveViewTest
  import SpacecastWeb.Components.ChangeHistoryViewer

  @moduletag :component

  @base_metadata_v1 %{
    timestamp: ~N[2024-01-01 00:00:00],
    actor: "user1-audit",
    reason: "Initial creation for audit",
    source: "audit-test-v1"
  }

  @base_metadata_v2 %{
    timestamp: ~N[2024-01-02 00:00:00],
    actor: "user2-audit",
    reason: "Update for audit",
    source: "audit-test-v2"
  }

  describe "change_history_viewer/1" do
    test "renders empty state when no history" do
      html =
        render_component(&change_history_viewer/1, %{resource: %{__change_history__: []}})

      assert html =~ "No changes tracked yet"
    end

    test "renders timeline view with history" do
      history = [
        %{
          version: 1,
          changes: %{name: "A"},
          metadata: %{timestamp: ~N[2024-01-01 00:00:00], actor: "user1", reason: "init"}
        },
        %{
          version: 2,
          changes: %{name: "B"},
          metadata: %{timestamp: ~N[2024-01-02 00:00:00], actor: "user2", reason: "update"}
        }
      ]

      resource = %{__change_history__: history, id: "test-id"}

      html =
        render_component(&change_history_viewer/1, %{resource: resource, view_mode: "timeline"})

      assert html =~ "Change History"
      assert html =~ "Timeline"
      assert html =~ "Version 1"
      assert html =~ "Version 2"
    end

    test "renders list view with history" do
      history = [
        %{
          version: 1,
          changes: %{name: "A"},
          metadata: %{timestamp: ~N[2024-01-01 00:00:00], actor: "user1", reason: "init"}
        }
      ]

      resource = %{__change_history__: history, id: "test-id"}

      html =
        render_component(&change_history_viewer/1, %{resource: resource, view_mode: "list"})

      assert html =~ "List"
      assert html =~ "Version"
      assert html =~ "user1"
    end

    test "renders audit log view with history" do
      history = [
        %{
          version: 1,
          changes: %{name: "A"},
          before: %{},
          metadata: %{
            timestamp: ~N[2024-01-01 00:00:00],
            actor: "user1",
            reason: "init",
            source: "test"
          }
        }
      ]

      resource = %{__change_history__: history, id: "test-id"}

      html =
        render_component(&change_history_viewer/1, %{resource: resource, view_mode: "audit"})

      assert html =~ "Audit Log"
      assert html =~ "Actor: user1"
      assert html =~ "Source: test"
    end

    test "renders correct phx-click and phx-value for view mode buttons" do
      html =
        render_component(&change_history_viewer/1, %{
          resource: %{
            __change_history__: [
              %{
                version: 1,
                changes: %{},
                metadata: %{timestamp: ~N[2024-01-01 00:00:00], actor: "a", reason: "r"}
              }
            ]
          }
        })

      assert html =~ ~s(phx-click="set_view_mode" phx-value-mode="timeline")
      assert html =~ ~s(phx-click="set_view_mode" phx-value-mode="list")
      assert html =~ ~s(phx-click="set_view_mode" phx-value-mode="audit")
    end

    test "renders correct phx-click and phx-value for view and diff buttons in list view" do
      history = [
        %{
          version: 1,
          changes: %{name: "A"},
          metadata: %{timestamp: ~N[2024-01-01 00:00:00], actor: "user1", reason: "init"}
        },
        %{
          version: 2,
          changes: %{name: "B"},
          metadata: %{timestamp: ~N[2024-01-02 00:00:00], actor: "user2", reason: "update"}
        }
      ]

      resource = %{__change_history__: history, id: "test-id"}

      html =
        render_component(&change_history_viewer/1, %{
          resource: resource,
          view_mode: "list",
          selected_version: 1,
          on_view_version: "view_version",
          on_diff_versions: "diff_versions"
        })

      # View button for version 2
      assert html =~ ~s(phx-click="view_version" phx-value-version="2")
      # Diff button for version 2
      assert html =~ ~s(phx-click="diff_versions" phx-value-version1="1" phx-value-version2="2")
    end

    test "renders correct phx-click and phx-value for view and compare buttons in audit view" do
      # Construct resource directly using defined module attributes
      resource = %{
        __change_history__: [
          # Note: prepare_versions_with_diffs reverses history, so v1 is last in display
          %{version: 1, changes: %{name: "A"}, before: %{}, metadata: @base_metadata_v1},
          %{version: 2, changes: %{name: "B"}, before: %{name: "A"}, metadata: @base_metadata_v2}
        ],
        id: "test-id"
      }

      html =
        render_component(&change_history_viewer/1, %{
          resource: resource,
          view_mode: "audit",
          # Custom event name for clarity
          on_view_version: "view_details_event",
          # Passed but not used by audit view
          on_diff_versions: "diff_event",
          # V1 is selected
          selected_version: 1
        })

      # Check for version 2 entry (displayed first)
      assert html =~ "Version 2"
      assert html =~ ~s/phx-click=\"view_details_event\" phx-value-version=\"2\"/
      assert html =~ ~s/Actor: #{Map.get(@base_metadata_v2, :actor)}/
      assert html =~ ~s/<strong>Reason:<\/strong> #{Map.get(@base_metadata_v2, :reason)}/
      # Robust error diff assertion (HTML-encoded)
      assert html =~ "Diff error: &quot;Version 2 is greater than current version 1&quot;"

      # Check for version 1 entry (displayed second, selected)
      assert html =~ "Version 1"
      assert html =~ ~s/phx-click=\"view_details_event\" phx-value-version=\"1\"/
      assert html =~ ~s/Actor: #{Map.get(@base_metadata_v1, :actor)}/
      assert html =~ ~s/<strong>Reason:<\/strong> #{Map.get(@base_metadata_v1, :reason)}/

      assert html =~
               ~s(<span class="text-red-500 line-through">nil</span> &rarr; <span class="text-green-500">&quot;A&quot;</span>)

      # Ensure the diff button is NOT present in audit view
      refute html =~ ~s/phx-click=\"diff_event\"/
    end
  end

  describe "integration: event propagation" do
    defmodule TestLive do
      use Phoenix.LiveView
      import SpacecastWeb.Components.ChangeHistoryViewer

      def render(assigns) do
        ~H"""
        <.change_history_viewer resource={@resource} selected_version={@selected_version} on_view_version="view_version" on_diff_versions="diff_versions" view_mode={@view_mode} />
        <div id="event-log">{@event_log}</div>
        """
      end

      def mount(_params, _session, socket) do
        {:ok,
         assign(socket,
           resource: %{
             __change_history__: [
               %{
                 version: 1,
                 changes: %{name: "A"},
                 metadata: %{timestamp: ~N[2024-01-01 00:00:00], actor: "user1", reason: "init"}
               },
               %{
                 version: 2,
                 changes: %{name: "B"},
                 metadata: %{timestamp: ~N[2024-01-02 00:00:00], actor: "user2", reason: "update"}
               }
             ]
           },
           selected_version: 1,
           view_mode: "list",
           event_log: ""
         )}
      end

      def handle_event("view_version", %{"version" => version}, socket) do
        {:noreply, assign(socket, event_log: "view_version:#{version}")}
      end

      def handle_event("diff_versions", %{"version1" => v1, "version2" => v2}, socket) do
        {:noreply, assign(socket, event_log: "diff_versions:#{v1}-#{v2}")}
      end
    end

    test "clicking view and diff buttons emits events to parent LiveView" do
      {:ok, view, _html} = live_isolated(build_conn(), TestLive)
      # Click the view button for version 2
      view
      |> element("button[phx-click=\"view_version\"][phx-value-version=\"2\"]")
      |> render_click()

      assert render(view) =~ "view_version:2"
      # Click the diff button for version 2
      view
      |> element("button[phx-click=\"diff_versions\"][phx-value-version1=\"1\"][phx-value-version2=\"2\"]")
      |> render_click()

      assert render(view) =~ "diff_versions:1-2"
    end
  end
end
