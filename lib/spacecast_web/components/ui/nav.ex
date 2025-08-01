defmodule SpacecastWeb.Components.UI.Nav do
  @moduledoc """
  Navigation components for the application.

  Provides navigation elements like navbar, breadcrumbs, and navigation links
  with consistent styling and behavior.
  """
  use Phoenix.Component

  @doc """
  Renders a navigation link for the monospace theme.

  ## Examples

      <.nav_link navigate={~p"/"} active={@current_section == :home}>Home</.nav_link>
  """
  attr :navigate, :any, required: true
  attr :active, :boolean, default: false
  attr :class, :string, default: nil
  attr :rest, :global
  slot :inner_block, required: true

  def nav_link(assigns) do
    ~H"""
    <.link
      navigate={@navigate}
      class={[
        "nav-link",
        @active && "active",
        @class
      ]}
      {@rest}
    >
      {render_slot(@inner_block)}
    </.link>
    """
  end

  @doc """
  Renders a horizontal navigation bar in the monospace style.

  ## Examples

      <.monospace_nav current_path={@current_path}>
        <:item path={~p"/"} label="Home" />
        <:item path={~p"/style-guide"} label="Style Guide" />
      </.monospace_nav>
  """
  attr :current_path, :string, required: true
  attr :class, :string, default: nil

  slot :item, required: true do
    attr :path, :string, required: true
    attr :label, :string, required: true
  end

  def monospace_nav(assigns) do
    ~H"""
    <nav class={["monospace-nav", @class]}>
      <ul class="monospace-nav-list">
        <%= for item <- @item do %>
          <li class="monospace-nav-item">
            <.nav_link navigate={item.path} active={@current_path == item.path} class="monospace-nav-link">
              {item.label}
            </.nav_link>
          </li>
        <% end %>
      </ul>
    </nav>
    """
  end

  @doc """
  Renders a table of contents (TOC) navigation in monospace style.

  ## Examples

      <.toc_nav>
        <:item id="introduction" label="Introduction" />
        <:item id="getting-started" label="Getting Started" />
      </.toc_nav>
  """
  attr :class, :string, default: nil

  slot :item, required: true do
    attr :id, :string, required: true
    attr :label, :string, required: true
  end

  def toc_nav(assigns) do
    ~H"""
    <nav id="TOC" role="doc-toc" class={["toc-nav", @class]}>
      <h2 id="toc-title">Contents</h2>
      <ul class="toc-list">
        <%= for item <- @item do %>
          <li class="toc-item">
            <a href={"##{item.id}"} id={"toc-#{item.id}"} class="toc-link">
              {item.label}
            </a>
          </li>
        <% end %>
      </ul>
    </nav>
    """
  end

  @doc """
  Renders a hierarchical table of contents (TOC) navigation in monospace style.

  This component supports multiple levels of nesting and works with the TocHelper
  to automatically generate TOC from content.

  ## Examples

      # With manual structure
      <.hierarchical_toc_nav toc_data={[
        %{id: "intro", label: "Introduction", level: 2, children: [
          %{id: "background", label: "Background", level: 3, children: []}
        ]},
        %{id: "features", label: "Features", level: 2, children: []}
      ]} />
      
      # With automatically generated TOC from TocHelper
      toc_data = TocHelper.generate_toc(html_content)
      <.hierarchical_toc_nav toc_data={toc_data} />
  """
  attr :toc_data, :list, required: true, doc: "The hierarchical TOC data from TocHelper"
  attr :class, :string, default: nil
  attr :current_section, :string, default: nil, doc: "ID of the current active section"
  attr :collapsible, :boolean, default: true, doc: "Whether nested sections can be collapsed"
  attr :initially_expanded, :boolean, default: true, doc: "Whether nested sections start expanded"

  def hierarchical_toc_nav(assigns) do
    ~H"""
    <nav id="TOC" role="doc-toc" class={["hierarchical-toc-nav", @class]}>
      <h2 id="toc-title" class="visually-hidden">Contents</h2>
      <ul class="toc-list">
        <%= for item <- @toc_data do %>
          <.toc_item item={item} current_section={@current_section} collapsible={@collapsible} initially_expanded={@initially_expanded} />
        <% end %>
      </ul>
    </nav>
    """
  end

  @doc """
  Renders a single TOC item with support for nested children.
  """
  attr :item, :map, required: true
  attr :current_section, :string, default: nil
  attr :collapsible, :boolean, default: true
  attr :initially_expanded, :boolean, default: true

  def toc_item(assigns) do
    # Create class based on heading level and active state
    has_children = Map.get(assigns.item, :children, []) |> length() > 0
    is_active = assigns.current_section == assigns.item.id
    level_class = "level-#{assigns.item.level}"

    assigns =
      assign(assigns,
        has_children: has_children,
        is_active: is_active,
        level_class: level_class
      )

    ~H"""
    <li class={[
      "toc-item",
      @level_class,
      @has_children && "has-children",
      @is_active && "active"
    ]}>
      <a href={"##{@item.id}"} id={"toc-#{@item.id}"} class="toc-link">
        <%= if @has_children && @collapsible do %>
          <span class="toggle-indicator" aria-hidden="true">
            <%= if @initially_expanded do %>
              ▼
            <% else %>
              ▶
            <% end %>
          </span>
        <% end %>
        <span class="toc-label">{@item.label}</span>
      </a>

      <%= if @has_children do %>
        <ul class={["toc-sublist", !@initially_expanded && "collapsed"]}>
          <%= for child <- @item.children do %>
            <.toc_item item={child} current_section={@current_section} collapsible={@collapsible} initially_expanded={@initially_expanded} />
          <% end %>
        </ul>
      <% end %>
    </li>
    """
  end
end
