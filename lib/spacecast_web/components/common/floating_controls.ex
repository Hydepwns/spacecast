defmodule SpacecastWeb.Components.Common.FloatingControls do
  @moduledoc """
  Floating controls component that combines theme toggle and debug grid functionality.

  This component creates a floating panel at the bottom left of the screen that contains
  both the theme toggle and debug grid controls in a compact, accessible format.
  """
  use Phoenix.Component
  alias Phoenix.LiveView.JS

  @doc """
  Renders a floating controls panel with theme toggle and debug grid.

  ## Examples
      <.floating_controls />
      <.floating_controls id="main-floating-controls" current_theme="dark" />
  """
  attr :id, :string, default: "floating-controls"
  attr :current_theme, :string, default: "light"
  attr :class, :string, default: nil
  attr :rest, :global

  def floating_controls(assigns) do
    ~H"""
    <div id={@id} class={["floating-controls", @class]} phx-hook="FloatingControls" {@rest}>
      <div id={"#{@id}-theme-wrapper"} class="theme-toggle-wrapper" phx-hook="ThemeHooks">
        <div class="floating-controls-panel">
          <div class="floating-controls-header">
            <span class="floating-controls-title">Controls</span>
            <button class="floating-controls-toggle" aria-label="Toggle controls">
              <span class="toggle-icon">⚙️</span>
            </button>
          </div>

          <div id={"#{@id}-content"} class="floating-controls-content">
            <div class="floating-controls-section">
              <h4 class="section-title">Theme</h4>
              <div class="theme-controls">
                <div class="theme-toggle-compact">
                  <span class="theme-toggle-label">Theme</span>
                  <div class="theme-toggle-current" phx-click={JS.push("toggle_theme_dropdown")} title="Click to change theme">
                    <span class="current-theme-icon">
                      <%= case @current_theme do %>
                        <% "light" -> %> ☀️
                        <% "dark" -> %> 🌙
                        <% "dim" -> %> 🟪
                        <% "synthwave" -> %> 🌆
                        <% "high-contrast" -> %> 🟨
                        <% _ -> %> ☀️
                      <% end %>
                    </span>
                    <span>▼</span>
                  </div>
                  <div class="theme-toggle-dropdown" id={"#{@id}-theme-dropdown"}>
                    <button class="theme-option" data-theme="light" phx-click={JS.push("change_theme", value: %{theme: "light"})} aria-label="Light theme">
                      <span class="theme-option-icon">☀️</span>
                      <span>Light</span>
                    </button>
                    <button class="theme-option" data-theme="dark" phx-click={JS.push("change_theme", value: %{theme: "dark"})} aria-label="Dark theme">
                      <span class="theme-option-icon">🌙</span>
                      <span>Dark</span>
                    </button>
                    <button class="theme-option" data-theme="dim" phx-click={JS.push("change_theme", value: %{theme: "dim"})} aria-label="Dim theme">
                      <span class="theme-option-icon">🟪</span>
                      <span>Dim</span>
                    </button>
                    <button class="theme-option" data-theme="synthwave" phx-click={JS.push("change_theme", value: %{theme: "synthwave"})} aria-label="Synthwave theme">
                      <span class="theme-option-icon">🌆</span>
                      <span>Synthwave</span>
                    </button>
                    <button class="theme-option" data-theme="high-contrast" phx-click={JS.push("change_theme", value: %{theme: "high-contrast"})} aria-label="High contrast theme">
                      <span class="theme-option-icon">🟨</span>
                      <span>High Contrast</span>
                    </button>
                  </div>
                </div>
              </div>
            </div>

            <div class="floating-controls-section">
              <h4 class="section-title">Debug</h4>
              <div class="debug-controls">
                <label class="debug-toggle-label" for={"#{@id}-debug-grid"}>
                  <span>Grid</span>
                  <input type="checkbox" id={"#{@id}-debug-grid"} phx-hook="DebugGridToggle" />
                </label>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end
end
