defmodule SpacecastWeb.Components.UI.ListView do
  @moduledoc """
  List view component for displaying change history in a tabular format.

  This component provides a structured list representation of resource changes,
  with options for viewing versions and comparing changes between versions.
  """

  use Phoenix.Component

  @doc """
  Renders a list view for change history.

  ## Attributes

  - `change_history` - List of change history entries
  - `selected_version` - Currently selected version (optional)
  - `on_view_version` - Event name to emit when a version is selected (optional)
  - `on_diff_versions` - Event name to emit when comparing versions (optional)
  """
  attr :change_history, :list, required: true
  attr :selected_version, :integer, default: nil
  attr :on_view_version, :string, default: nil
  attr :on_diff_versions, :string, default: nil

  def render_list_view(assigns) do
    ~H"""
    <div class="list-view">
      <div class="list-header">
        <h4 class="list-title">Change History List</h4>
        <div class="list-controls">
          <button
            phx-click={@on_diff_versions && "#{@on_diff_versions}"}
            class="list-button list-button--secondary"
            disabled={!@on_diff_versions}
          >
            Compare Versions
          </button>
        </div>
      </div>

      <div class="list-container">
        <table class="list-table">
          <thead>
            <tr>
              <th>Version</th>
              <th>Timestamp</th>
              <th>Description</th>
              <th>Changes</th>
              <th>Actions</th>
            </tr>
          </thead>
          <tbody>
            <%= for entry <- @change_history do %>
              <tr class={"list-row #{if @selected_version == entry.version, do: "selected"}"}>
                <td class="list-cell list-cell--version">
                  <strong>v<%= entry.version %></strong>
                </td>
                <td class="list-cell list-cell--timestamp">
                  <%= format_timestamp(entry.timestamp) %>
                </td>
                <td class="list-cell list-cell--description">
                  <%= entry.description || "Change made to resource" %>
                </td>
                <td class="list-cell list-cell--changes">
                  <%= format_changes(entry.changes) %>
                </td>
                <td class="list-cell list-cell--actions">
                  <div class="list-actions">
                    <button
                      phx-click={@on_view_version && "#{@on_view_version}"}
                      phx-value-version={entry.version}
                      class="list-button list-button--small"
                      disabled={!@on_view_version}
                    >
                      View
                    </button>

                    <%= if @selected_version == entry.version do %>
                      <span class="list-selected">Selected</span>
                    <% end %>
                  </div>
                </td>
              </tr>
            <% end %>
          </tbody>
        </table>
      </div>
    </div>
    """
  end

  defp format_timestamp(timestamp) when is_binary(timestamp) do
    case DateTime.from_iso8601(timestamp) do
      {:ok, datetime, _} -> format_datetime(datetime)
      _ -> timestamp
    end
  end

  defp format_timestamp(%DateTime{} = datetime) do
    format_datetime(datetime)
  end

  defp format_timestamp(_), do: "Unknown"

  defp format_datetime(datetime) do
    Calendar.strftime(datetime, "%Y-%m-%d %H:%M")
  end

  defp format_changes(changes) when is_list(changes) do
    changes
    |> Enum.map(fn change ->
      case change do
        %{field: field, action: action} -> "#{action} #{field}"
        %{field: field} -> "Modified #{field}"
        _ -> "Change"
      end
    end)
    |> Enum.join(", ")
  end

  defp format_changes(_), do: "Changes made"
end
