defmodule SpacecastWeb.Components.Common.ThemeToggle do
  @moduledoc """
  Provides a theme toggle component for switching between light, dark, dim, and high contrast themes.

  This component renders a set of buttons that send theme change events to the parent LiveView
  via phx-click events using JS.push. The parent LiveView should implement a "change_theme" 
  event handler to process these events.

  The component is also connected to a JavaScript hook ("ThemeToggle") that handles
  theme persistence in localStorage and applies theme changes to the DOM.

  Keyboard shortcuts available:
  - Shift+Up/Right: Next theme
  - Shift+Down/Left: Previous theme
  """
  use Phoenix.Component
  alias Phoenix.LiveView.JS

  @doc """
  Renders a theme toggle component with buttons for different themes.

  Each button is assigned a data-theme attribute and triggers a "change_theme" event
  with the appropriate theme value when clicked.

  ## Examples
      <.theme_toggle />
      <.theme_toggle id="style-guide-theme-toggle" />
  """
  def theme_toggle(assigns) do
    assigns = assign_new(assigns, :id, fn -> "theme-toggle" end)

    ~H"""
    <div id={@id} class="theme-toggle" phx-hook="ThemeToggle" role="group" aria-label="Theme selection">
      <button id={"#{@id}-light-button"} class="theme-button" data-theme="light" phx-click={JS.push("change_theme", value: %{theme: "light"})} aria-label="Switch to light theme" title="Light theme (⌘+L)">
        <span class="theme-icon">☀️</span>
      </button>
      <button id={"#{@id}-dark-button"} class="theme-button" data-theme="dark" phx-click={JS.push("change_theme", value: %{theme: "dark"})} aria-label="Switch to dark theme" title="Dark theme (⌘+D)">
        <span class="theme-icon">🌙</span>
      </button>
      <button id={"#{@id}-dim-button"} class="theme-button" data-theme="dim" phx-click={JS.push("change_theme", value: %{theme: "dim"})} aria-label="Switch to dim theme" title="Dim theme (⌘+M)">
        <span class="theme-icon">🟪</span>
      </button>
      <button id={"#{@id}-high-contrast-button"} class="theme-button" data-theme="high-contrast" phx-click={JS.push("change_theme", value: %{theme: "high-contrast"})} aria-label="Switch to high contrast theme" title="High contrast theme (⌘+H)">
        <span class="theme-icon">🟨</span>
      </button>
    </div>
    """
  end

  @doc """
  Renders a theme toggle component that displays the current theme.

  This version of the toggle is more compact and responsive-friendly.

  ## Examples
      <ThemeToggle.render current_theme="light-theme" />
      <ThemeToggle.render id="style-guide-theme-toggle" current_theme="light-theme" />
  """
  def render(assigns) do
    # Extract the base theme name without the -theme suffix
    base_theme =
      case assigns[:current_theme] do
        nil -> "light"
        theme -> String.replace(theme, "-theme", "")
      end

    assigns = assign(assigns, :base_theme, base_theme)
    assigns = assign_new(assigns, :id, fn -> "theme-toggle" end)

    ~H"""
    <div id={@id} class="theme-toggle" phx-hook="ThemeToggle" role="group" aria-label="Theme selection">
      <div class="theme-toggle-current">
        <span>Theme: </span>
        <span class="current-theme-label">
          {String.capitalize(@base_theme)}
          <%= case @base_theme do %>
            <% "light" -> %>
              ⬜️
            <% "dim" -> %>
              🟪
            <% "dark" -> %>
              ⬛️
            <% "high-contrast" -> %>
              🟨
            <% _ -> %>
              ⬜️
          <% end %>
        </span>
      </div>
      <div class="theme-toggle-buttons">
        <button id={"#{@id}-light-theme"} data-theme="light" phx-click={JS.push("change_theme", value: %{theme: "light"})} aria-label="Switch to light theme" title="Light (Shift+Up/Right)" class={if @base_theme == "light", do: "active"} aria-pressed={if @base_theme == "light", do: "true", else: "false"} tabindex="0">
          ⬜️
        </button>
        <button id={"#{@id}-dim-theme"} data-theme="dim" phx-click={JS.push("change_theme", value: %{theme: "dim"})} aria-label="Switch to dim theme" title="Dim (Shift+Up/Right)" class={if @base_theme == "dim", do: "active"} aria-pressed={if @base_theme == "dim", do: "true", else: "false"} tabindex="0">
          🟪
        </button>
        <button id={"#{@id}-dark-theme"} data-theme="dark" phx-click={JS.push("change_theme", value: %{theme: "dark"})} aria-label="Switch to dark theme" title="Dark (Shift+Up/Right)" class={if @base_theme == "dark", do: "active"} aria-pressed={if @base_theme == "dark", do: "true", else: "false"} tabindex="0">
          ⬛️
        </button>
        <button
          id={"#{@id}-high-contrast-theme"}
          data-theme="high-contrast"
          phx-click={JS.push("change_theme", value: %{theme: "high-contrast"})}
          aria-label="Switch to high contrast theme"
          title="High Contrast (Accessibility)"
          class={if @base_theme == "high-contrast", do: "active"}
          aria-pressed={if @base_theme == "high-contrast", do: "true", else: "false"}
          tabindex="0"
        >
          🟨
        </button>
      </div>
    </div>
    """
  end
end
