defmodule SpacecastWeb.Components.ChangeHistoryViewer do
  @moduledoc """
  A component for displaying change history for tracked resources.

  This component provides a visual interface for viewing resource change history,
  comparing different versions, and visualizing changes over time.

  ## Examples

  ```heex
  <.change_history_viewer
    resource={@user}
    selected_version={@selected_version}
    on_view_version="view_version"
    on_diff_versions="diff_versions"
  />
  ```
  """

  use Phoenix.Component

  alias Spacecast.Utils.ChangeTracker
  alias SpacecastWeb.Components.UI.TimelineView
  alias SpacecastWeb.Components.UI.ListView

  @doc """
  Renders a change history viewer for a resource.

  ## Attributes

  - `resource` - The resource with change history.
  - `selected_version` - Currently selected version (optional).
  - `on_view_version` - Event name to emit when a version is selected (optional).
  - `on_diff_versions` - Event name to emit when comparing versions (optional).
  - `view_mode` - The view mode to display: "timeline", "list", or "audit" (optional, default: "timeline").
  - `diff` - Map of diffs between two versions, if applicable (optional).
  - `versioned_resource` - The resource state at a specific version (optional).
  - `rest` - Additional HTML attributes to add to the container element.
  """
  attr :resource, :map, required: true
  attr :selected_version, :integer, default: nil
  attr :on_view_version, :string, default: nil
  attr :on_diff_versions, :string, default: nil
  attr :view_mode, :string, default: "timeline"
  attr :diff, :map, default: nil
  attr :versioned_resource, :map, default: nil
  attr :rest, :global

  # Added :versions_with_diffs to assigns for clarity during updates
  # No, this is not how attr works. It's for external attributes.
  # The mount/update will put :versions_with_diffs into the socket.

  def change_history_viewer(assigns) do
    # Ensure resource and its history are properly structured
    resource = assigns.resource
    history = get_history(resource)
    has_history = Enum.any?(history)

    assigns =
      assigns
      |> assign_new(:versions_with_diffs, fn -> prepare_versions_with_diffs(resource, history) end)
      # change_history is used by timeline/list views
      |> assign(:change_history, history)
      |> assign(:has_history, has_history)

    ~H"""
    <div class="change-history-viewer" {@rest}>
      <div class="change-history-header">
        <h3 class="text-xl font-bold mb-3">Change History</h3>
        <div class="view-mode-selector mb-4 flex space-x-2">
          <button phx-click="set_view_mode" phx-value-mode="timeline" class={"view-mode-button #{if @view_mode == "timeline", do: "active"}"}>
            Timeline
          </button>
          <button phx-click="set_view_mode" phx-value-mode="list" class={"view-mode-button #{if @view_mode == "list", do: "active"}"}>
            List
          </button>
          <button phx-click="set_view_mode" phx-value-mode="audit" class={"view-mode-button #{if @view_mode == "audit", do: "active"}"}>
            Audit Log
          </button>
        </div>
      </div>

      <p :if={!@has_history} class="text-gray-500 italic">No changes tracked yet.</p>
      <div :if={@has_history}>
        <div :if={@view_mode == "timeline"}>
          <TimelineView.render_timeline_view change_history={@change_history} selected_version={@selected_version} on_view_version={@on_view_version} />
        </div>

        <div :if={@view_mode == "list"}>
          <ListView.render_list_view change_history={@change_history} selected_version={@selected_version} on_view_version={@on_view_version} on_diff_versions={@on_diff_versions} />
        </div>

        <div :if={@view_mode == "audit"}>
          <div class="audit-log mb-6">
            <div class="flex justify-between mb-4">
              <h4 class="text-lg font-semibold">Audit Log</h4>
            </div>
            <div class="audit-entries space-y-4">
              <div :for={entry <- @versions_with_diffs}>
                <.render_audit_row entry={entry} selected_version={@selected_version} on_view_version={@on_view_version} />
              </div>
            </div>
          </div>
        </div>
      </div>

      <div :if={@versioned_resource} class="version-details mb-6">
        <h4 class="text-lg font-semibold mb-2">Version {@selected_version}</h4>
        <pre class="bg-gray-100 p-3 rounded text-sm overflow-auto">{inspect(@versioned_resource, pretty: true)}</pre>
      </div>

      <div :if={@diff} class="diff-view mb-6">
        <h4 class="text-lg font-semibold mb-2">Showing diff</h4>
        <div class="diff-details bg-gray-100 p-3 rounded">
          <div :for={{key, values} <- @diff.changes} class="diff-item mb-4 border-b pb-2">
            <div class="diff-key font-bold mb-1">{Atom.to_string(key)}</div>
            <div class="diff-values grid grid-cols-2 gap-4">
              <div class="diff-old">
                <span class="text-red-500">- {inspect(values.before)}</span>
              </div>
              <div class="diff-new">
                <span class="text-green-500">+ {inspect(values.after)}</span>
              </div>
            </div>
            <div :if={values[:nested_diff] && map_size(values.nested_diff) > 0} class="mt-2 pl-4 border-l-2 border-gray-300">
              <div class="text-sm font-medium mb-1">Nested Changes:</div>
              <div :for={{nested_key, nested_values} <- values.nested_diff} class="diff-item mb-2">
                <div class="diff-key font-medium text-sm">{Atom.to_string(nested_key)}</div>
                <div class="diff-values grid grid-cols-2 gap-4 text-sm">
                  <div class="diff-old">
                    <span class="text-red-500">- {inspect(nested_values.before)}</span>
                  </div>
                  <div class="diff-new">
                    <span class="text-green-500">+ {inspect(nested_values.after)}</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  defp prepare_versions_with_diffs(resource, history) do
    Enum.map(Enum.reverse(history), fn version_data ->
      vsn = version_data.version
      prev_vsn = vsn - 1

      diff_to_this_version =
        cond do
          prev_vsn <= 0 ->
            changes = version_data.changes

            formatted_changes =
              Enum.into(changes, %{}, fn {field, val} -> {field, %{before: nil, after: val}} end)

            %{changes: formatted_changes, version: vsn}

          true ->
            case ChangeTracker.diff(resource, version1: prev_vsn, version2: vsn) do
              {:ok, diff_map} ->
                diff_map

              {:error, reason} ->
                %{changes: %{error: "Diff error: #{inspect(reason)}"}, version: vsn}
            end
        end

      %{
        version: vsn,
        # Handle both old and new metadata structures
        timestamp: Map.get(version_data, :timestamp, Map.get(version_data.metadata, :timestamp)),
        actor: Map.get(version_data, :actor, Map.get(version_data.metadata, :actor)),
        # Keep full metadata
        metadata: version_data.metadata,
        raw_changes: version_data.changes,
        diff: diff_to_this_version
      }
    end)
  end

  # Helper function to get history, robust to missing :__change_history__
  defp get_history(resource) do
    Map.get(resource, :__change_history__, [])
  end

  # This is the new, isolated audit row component function
  attr :entry, :map, required: true
  # Passed down for styling active version
  attr :selected_version, :integer, default: nil
  # Passed down for button actions
  attr :on_view_version, :string, default: nil

  def render_audit_row(assigns) do
    # assigns here will contain :entry, :selected_version, :on_view_version from the call site <.render_audit_row ... />
    # @entry is the specific item from @versions_with_diffs

    ~H"""
    <div class={
      "audit-entry p-4 border rounded-lg " <>
        if @selected_version && @selected_version == @entry.version,
          do: "border-blue-500 bg-blue-50",
          else: "border-gray-200"
    }>
      <div class="flex justify-between mb-2">
        <div class="text-sm font-semibold text-gray-700">Version {@entry.version}</div>
        <div class="text-sm text-gray-500">{format_timestamp(Map.get(@entry.metadata, :timestamp))}</div>
      </div>
      <div class="mb-2">
        <span class="inline-block bg-blue-100 text-blue-800 text-xs px-2 py-1 rounded mr-2">
          Actor: {Map.get(@entry.metadata, :actor) || "Unknown"}
        </span>
        <span class="inline-block bg-green-100 text-green-800 text-xs px-2 py-1 rounded mr-2">
          Source: {Map.get(@entry.metadata, :source) || "Unknown"}
        </span>
      </div>
      <div class="mb-3 text-sm text-gray-600">
        <p><strong>Reason:</strong> {Map.get(@entry.metadata, :reason) || "No reason provided"}</p>
      </div>
      <div class="details">
        <%= for {field, diff_val} <- Map.to_list(@entry.diff.changes) do %>
          <div class="field-change">
            <strong>{field}:</strong>
            <%= if is_map(diff_val) && Map.has_key?(diff_val, :before) && Map.has_key?(diff_val, :after) do %>
              <span class="text-red-500 line-through">{inspect(Map.get(diff_val, :before))}</span> &rarr; <span class="text-green-500">{inspect(Map.get(diff_val, :after))}</span>
            <% else %>
              <span>Value: {diff_val}</span>
            <% end %>
          </div>
        <% end %>
      </div>
      <div class="metadata-details mt-2 pt-2 border-t border-gray-200">
        <div class="text-sm font-medium mb-1">Full Metadata:</div>
        <div :for={{key, value} <- @entry.metadata} class="text-xs text-gray-500">
          <span class="font-semibold">{Atom.to_string(key)}:</span> {inspect(value)}
        </div>
      </div>
      <div :if={@on_view_version} class="actions mt-3">
        <button phx-click={@on_view_version} phx-value-version={@entry.version} class="button-primary-sm">
          View Full Version
        </button>
      </div>
    </div>
    """
  end

  # Utility functions for formatting
  defp format_timestamp(nil), do: "N/A"

  defp format_timestamp(timestamp) do
    # Assuming it's already a DateTime
    Calendar.strftime(timestamp, "%Y-%m-%d %H:%M:%S %Z")
  end
end
