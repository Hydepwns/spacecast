defmodule SpacecastWeb.Components.Layouts.MobileNav do
  @moduledoc """
  Mobile navigation component with touch-friendly navigation controls.

  This component provides a fixed bottom navigation menu that appears only on mobile
  devices, following mobile app design patterns for easier navigation.
  """
  use Phoenix.Component
  import SpacecastWeb.Helpers.PathHelper

  @doc """
  Renders a mobile navigation bar that appears at the bottom of the screen on mobile devices.

  ## Examples

      <.mobile_nav current_path={@current_path} />

  ## Attributes

  * `current_path` - The current path to highlight the active navigation item
  """
  attr :current_path, :string, required: true
  attr :class, :string, default: ""

  def mobile_nav(assigns) do
    ~H"""
    <nav class={"mobile-nav-menu show-on-mobile #{@class}"} aria-label="Mobile navigation">
      <a href={path_to(:home)} class={"mobile-nav-item #{if @current_path == path_to(:home), do: "active"}"} aria-current={if @current_path == path_to(:home), do: "page", else: "false"}>
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" class="w-6 h-6">
          <path d="M11.47 3.84a.75.75 0 011.06 0l8.69 8.69a.75.75 0 101.06-1.06l-8.689-8.69a2.25 2.25 0 00-3.182 0l-8.69 8.69a.75.75 0 001.061 1.06l8.69-8.69z" />
          <path d="M12 5.432l8.159 8.159c.03.03.06.058.091.086v6.198c0 1.035-.84 1.875-1.875 1.875H15a.75.75 0 01-.75-.75v-4.5a.75.75 0 00-.75-.75h-3a.75.75 0 00-.75.75V21a.75.75 0 01-.75.75H5.625a1.875 1.875 0 01-1.875-1.875v-6.198a2.29 2.29 0 00.091-.086L12 5.43z" />
        </svg>
        <span>Home</span>
      </a>

      <a href={path_to(:projects)} class={"mobile-nav-item #{if @current_path == path_to(:projects), do: "active"}"} aria-current={if @current_path == path_to(:projects), do: "page", else: "false"}>
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" class="w-6 h-6">
          <path
            fill-rule="evenodd"
            d="M3 6a3 3 0 013-3h2.25a3 3 0 013 3v2.25a3 3 0 01-3 3H6a3 3 0 01-3-3V6zm9.75 0a3 3 0 013-3H18a3 3 0 013 3v2.25a3 3 0 01-3 3h-2.25a3 3 0 01-3-3V6zM3 15.75a3 3 0 013-3h2.25a3 3 0 013 3V18a3 3 0 01-3 3H6a3 3 0 01-3-3v-2.25zm9.75 0a3 3 0 013-3H18a3 3 0 013 3V18a3 3 0 01-3 3h-2.25a3 3 0 01-3-3v-2.25z"
            clip-rule="evenodd"
          />
        </svg>
        <span>Projects</span>
      </a>

      <a href="/admin/event-dashboard" data-test-id="nav-link-events" class={"mobile-nav-item #{if @current_path == "/admin/event-dashboard", do: "active"}"} aria-current={if @current_path == "/admin/event-dashboard", do: "page", else: "false"}>
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" class="w-6 h-6">
          <path d="M12 2a10 10 0 100 20 10 10 0 000-20zm1 15h-2v-2h2v2zm0-4h-2V7h2v6z" />
        </svg>
        <span>Events</span>
      </a>

      <a href={path_to(:style_guide)} class={"mobile-nav-item #{if @current_path == path_to(:style_guide), do: "active"}"} aria-current={if @current_path == path_to(:style_guide), do: "page", else: "false"}>
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" class="w-6 h-6">
          <path d="M21.731 2.269a2.625 2.625 0 00-3.712 0l-1.157 1.157 3.712 3.712 1.157-1.157a2.625 2.625 0 000-3.712zM19.513 8.199l-3.712-3.712-12.15 12.15a5.25 5.25 0 00-1.32 2.214l-.8 2.685a.75.75 0 00.933.933l2.685-.8a5.25 5.25 0 002.214-1.32L19.513 8.2z" />
        </svg>
        <span>Style</span>
      </a>

      <a href={path_to(:about)} class={"mobile-nav-item #{if @current_path == path_to(:about), do: "active"}"} aria-current={if @current_path == path_to(:about), do: "page", else: "false"}>
        <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="currentColor" class="w-6 h-6">
          <path
            fill-rule="evenodd"
            d="M2.25 12c0-5.385 4.365-9.75 9.75-9.75s9.75 4.365 9.75 9.75-4.365 9.75-9.75 9.75S2.25 17.385 2.25 12zm8.706-1.442c1.146-.573 2.437.463 2.126 1.706l-.709 2.836.042-.02a.75.75 0 01.67 1.34l-.04.022c-1.147.573-2.438-.463-2.127-1.706l.71-2.836-.042.02a.75.75 0 11-.671-1.34l.041-.022zM12 9a.75.75 0 100-1.5.75.75 0 000 1.5z"
            clip-rule="evenodd"
          />
        </svg>
        <span>About</span>
      </a>
    </nav>
    """
  end
end
