defmodule SpacecastWeb.Components.UI.NavigationMenu do
  @moduledoc """
  Navigation Menu component that provides responsive navigation with enhanced JavaScript functionality.

  This component provides a responsive navigation menu with the following features:
  - Mobile responsiveness with collapsible menu
  - Keyboard navigation with arrow keys and focus management
  - Active item highlighting based on current path
  - Accessibility features including ARIA attributes

  It uses the NavigationMenu JavaScript hook for enhanced functionality.
  """
  use Phoenix.Component

  @doc """
  Renders a navigation menu with enhanced JavaScript functionality.

  ## Examples

      <.navigation_menu current_path={@current_path}>
        <:item href="/">Home</:item>
        <:item href="/about">About</:item>
        <:item href="/contact">Contact</:item>
      </.navigation_menu>

  ## Attributes

  * `current_path` - The current path to highlight the active navigation item
  * `class` - Additional CSS classes to apply to the navigation container
  """
  attr :id, :string, default: "main-navigation"
  attr :current_path, :string, required: true
  attr :class, :string, default: ""

  slot :item, required: true do
    attr :href, :string, required: true
  end

  def navigation_menu(assigns) do
    ~H"""
    <div id={@id} class={"site-navigation #{@class}"} phx-hook="NavigationMenu" data-current-path={@current_path}>
      <button class="mobile-nav-toggle" aria-label="Toggle menu" aria-expanded="false">
        <span class="hamburger-icon">
          <span class="hamburger-bar"></span>
          <span class="hamburger-bar"></span>
          <span class="hamburger-bar"></span>
        </span>
      </button>

      <nav class="site-nav" aria-label="Main navigation">
        <ul class="nav-list">
          <%= for item <- @item do %>
            <li class="nav-item">
              <a href={item.href} class={"nav-link #{if @current_path == item.href, do: "active"}"} aria-current={if @current_path == item.href, do: "page", else: "false"}>
                {render_slot(item)}
              </a>
            </li>
          <% end %>
        </ul>
      </nav>
    </div>
    """
  end
end
