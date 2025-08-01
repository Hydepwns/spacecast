defmodule SpacecastWeb.Components.Common.HeaderComponent do
  use Phoenix.Component

  attr :title, :string, required: true
  attr :subtitle, :string, default: nil
  attr :actions, :list, default: []
  attr :class, :string, default: nil

  def header(assigns) do
    ~H"""
    <header class={["flex items-center justify-between", @class]}>
      <div>
        <h1 class="text-2xl font-semibold text-gray-900 dark:text-white">{@title}</h1>
        <%= if @subtitle do %>
          <p class="mt-1 text-sm text-gray-500 dark:text-gray-400">{@subtitle}</p>
        <% end %>
      </div>
      <%= if @actions != [] do %>
        <div class="flex items-center gap-4">
          <%= for action <- @actions do %>
            {action}
          <% end %>
        </div>
      <% end %>
    </header>
    """
  end
end
