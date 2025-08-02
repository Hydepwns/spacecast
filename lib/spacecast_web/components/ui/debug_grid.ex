defmodule SpacecastWeb.Components.UI.DebugGrid do
  use Phoenix.Component

  @moduledoc """
  Debug grid component for visualizing layout alignment.

  Provides a visual grid overlay that can be toggled on/off to help with
  debugging and aligning UI elements during development.
  """

  @doc """
  Renders a debug grid toggle for visualizing the monospace grid alignment.

  ## Example

      <.debug_grid />
  """
  attr :class, :string, default: nil
  attr :rest, :global
  attr :id, :string, default: "debug-grid-toggle"

  def debug_grid(assigns) do
    ~H"""
    <div class="debug-grid-container">
      <div class="debug-grid" style="display: none;"></div>
      <div class="debug-toggle">
        <label class="debug-toggle-label" for={@id}>
          <span>Debug</span>
          <input type="checkbox" id={@id} phx-hook="DebugGridToggle" />
        </label>
      </div>
    </div>
    """
  end
end
