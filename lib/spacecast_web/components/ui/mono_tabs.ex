defmodule SpacecastWeb.Components.UI.MonoTabs do
  @moduledoc """
  Monospace tabbed interface component that maintains grid alignment.

  This component provides a tabbed interface with consistent monospace styling,
  ensuring tabs and content maintain proper grid alignment. Tabs can be rendered
  in different styles (bordered, underlined, or boxed) while preserving the
  monospace aesthetic.
  """
  use Phoenix.Component
  alias Phoenix.LiveView.JS
  import SpacecastWeb.Components.Common.CoreComponents, only: [icon_component: 1]

  @doc """
  Renders a monospace tabbed interface.

  ## Examples

      <.mono_tabs id="demo-tabs">
        <:tab id="tab1" title="First Tab">
          Content for first tab goes here
        </:tab>
        <:tab id="tab2" title="Second Tab">
          Content for second tab goes here
        </:tab>
      </.mono_tabs>

  ## Attributes

  * `id` - Required unique identifier for the tabs component
  * `class` - Additional CSS classes to add to the tabs container
  * `style` - Tab styling variant: :bordered, :underlined, :boxed (default: :bordered)
  * `active_tab` - ID of the initially active tab (defaults to first tab)
  * `vertical` - Whether tabs should be arranged vertically (default: false)
  """
  attr :id, :string, required: true
  attr :class, :string, default: nil
  attr :style, :atom, default: :bordered, values: [:bordered, :underlined, :boxed]
  attr :active_tab, :string, default: nil
  attr :vertical, :boolean, default: false
  attr :rest, :global

  slot :tab, required: true do
    attr :id, :string, required: true
    attr :title, :string, required: true
    attr :icon, :string
  end

  def mono_tabs(assigns) do
    style_class =
      case assigns.style do
        :bordered -> "mono-tabs-bordered"
        :underlined -> "mono-tabs-underlined"
        :boxed -> "mono-tabs-boxed"
      end

    vertical_class = if assigns.vertical, do: "mono-tabs-vertical", else: ""

    assigns = assign(assigns, :style_class, style_class)
    assigns = assign(assigns, :vertical_class, vertical_class)

    ~H"""
    <div id={@id} data-test="mono-tabs" data-test-style={@style} class={["mono-tabs", @style_class, @vertical_class, @class]} {@rest}>
      <div class="mono-tabs-list" role="tablist" data-test="mono-tabs-list">
        <%= for {tab, index} <- Enum.with_index(@tab) do %>
          <button type="button" role="tab" id={"#{@id}-tab-#{tab.id}"} data-test={"mono-tab-#{index + 1}"} data-tab-id={tab.id} aria-selected={tab.id == @active_tab} aria-controls={"#{@id}-panel-#{tab.id}"} class={["mono-tab", tab.id == @active_tab && "active"]} phx-click={JS.push("tab_click", value: %{tab: tab.id})}>
            <%= if tab.icon do %>
              <.icon_component name={tab.icon} class="mono-tab-icon" />
            <% end %>
            <span class="mono-tab-title">{tab.title}</span>
          </button>
        <% end %>
      </div>

      <div class="mono-tabs-content" data-test="mono-tabs-content">
        <%= for tab <- @tab do %>
          <div id={"#{@id}-panel-#{tab.id}"} role="tabpanel" aria-labelledby={"#{@id}-tab-#{tab.id}"} data-test={"mono-tab-panel-#{tab.id}"} class={["mono-tab-panel", tab.id == @active_tab && "active"]} hidden={tab.id != @active_tab}>
            {render_slot(tab)}
          </div>
        <% end %>
      </div>
    </div>
    """
  end
end
