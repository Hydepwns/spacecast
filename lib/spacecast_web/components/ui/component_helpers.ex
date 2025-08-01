defmodule SpacecastWeb.Components.UI.ComponentHelpers do
  @moduledoc """
  Helper functions for UI components.
  """

  import Phoenix.Component
  # import Phoenix.LiveView.Helpers

  @doc """
  Returns the appropriate color classes for action buttons based on the style.
  """
  def action_button_color(style) do
    case style do
      :primary -> "bg-indigo-600 hover:bg-indigo-700 text-white"
      :secondary -> "bg-gray-200 hover:bg-gray-300 text-gray-700"
      :danger -> "bg-red-600 hover:bg-red-700 text-white"
      :success -> "bg-green-600 hover:bg-green-700 text-white"
      :warning -> "bg-yellow-500 hover:bg-yellow-600 text-white"
      _ -> "bg-gray-200 hover:bg-gray-300 text-gray-700"
    end
  end

  @doc """
  Returns the appropriate color classes for progress bars based on the level.
  """
  def progress_color(level) do
    case level do
      :critical -> "bg-red-500"
      :warning -> "bg-yellow-500"
      :info -> "bg-blue-500"
      :success -> "bg-green-500"
      _ -> "bg-gray-500"
    end
  end

  @doc """
  Returns the appropriate color classes for notifications based on the level.
  """
  def notification_color(level) do
    case level do
      :critical -> "bg-red-100 dark:bg-red-900 dark:bg-opacity-30"
      :warning -> "bg-yellow-100 dark:bg-yellow-900 dark:bg-opacity-30"
      :info -> "bg-blue-100 dark:bg-blue-900 dark:bg-opacity-30"
      :success -> "bg-green-100 dark:bg-green-900 dark:bg-opacity-30"
      _ -> "bg-gray-100 dark:bg-gray-800"
    end
  end

  @doc """
  Renders an icon based on the level.
  """
  def render_icon(assigns) do
    ~H"""
    <div class={"icon icon-#{@level}"}>
      <%= case @level do %>
        <% :info -> %>
          <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        <% :success -> %>
          <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7" />
          </svg>
        <% :warning -> %>
          <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
          </svg>
        <% :error -> %>
          <svg class="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
      <% end %>
    </div>
    """
  end

  @doc """
  Renders a notification with the given content and level.
  """
  def render_notification(assigns) do
    ~H"""
    <div class="notification">
      <div class="notification-content">
        {@content}
      </div>
    </div>
    """
  end
end
