defmodule SpacecastWeb.Components.Common.LinkComponent do
  use Phoenix.Component

  attr :href, :string, default: nil
  attr :patch, :string, default: nil
  attr :navigate, :string, default: nil
  attr :phx_click, :any, default: nil
  attr :phx_value, :any, default: nil
  attr :class, :string, default: nil
  slot :inner_block, required: false

  def link_component(assigns) do
    ~H"""
    <%= if @phx_click || @phx_value do %>
      <button class={["link", @class]} phx-click={@phx_click} phx-value={@phx_value}>
        {render_slot(@inner_block)}
      </button>
    <% else %>
      <.link class={["link", @class]} href={@href} patch={@patch} navigate={@navigate}>
        {render_slot(@inner_block)}
      </.link>
    <% end %>
    """
  end
end
