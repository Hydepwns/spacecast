defmodule SpacecastWeb.Components.UI.TimelineView do
  @moduledoc """
  Timeline view component for displaying change history in a chronological format.

  This component provides a visual timeline representation of resource changes,
  with interactive elements for viewing specific versions and comparing changes.
  """

  use Phoenix.Component

  @doc """
  Renders a timeline view for change history.

  ## Attributes

  - `change_history` - List of change history entries
  - `selected_version` - Currently selected version (optional)
  - `on_view_version` - Event name to emit when a version is selected (optional)
  """
  attr :change_history, :list, required: true
  attr :selected_version, :integer, default: nil
  attr :on_view_version, :string, default: nil

  def render_timeline_view(assigns) do
    ~H"""
    <div class="timeline-view">
      <div class="timeline-container">
        <div class="timeline-line"></div>

        <%= for {entry, _index} <- Enum.with_index(@change_history) do %>
          <div class="timeline-item">
            <div class="timeline-marker">
              <div class="timeline-dot"></div>
              <div class="timeline-connector"></div>
            </div>

            <div class="timeline-content">
              <div class="timeline-header">
                <h4 class="timeline-title">Version <%= entry.version %></h4>
                <span class="timeline-date">
                  <%= format_timestamp(entry.timestamp) %>
                </span>
              </div>

              <div class="timeline-details">
                <p class="timeline-description">
                  <%= entry.description || "Change made to resource" %>
                </p>

                <div class="timeline-actions">
                  <button
                    phx-click={@on_view_version && "#{@on_view_version}"}
                    phx-value-version={entry.version}
                    class="timeline-button"
                    disabled={!@on_view_version}
                  >
                    View Version
                  </button>

                  <%= if @selected_version == entry.version do %>
                    <span class="timeline-selected">Currently Selected</span>
                  <% end %>
                </div>
              </div>
            </div>
          </div>
        <% end %>
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
    Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
  end
end
