defmodule SpacecastWeb.Components.UI.LayoutComponents do
  @moduledoc """
  Provides layout UI components.
  """
  use Phoenix.Component
  use Gettext, backend: SpacecastWeb.Gettext

  # Import base components
  import SpacecastWeb.Components.BaseComponents

  # Remove import of CoreComponents (if present)
  # import SpacecastWeb.CoreComponents

  @doc """
  Renders a header with title.
  """
  attr :class, :string, default: nil
  attr :title, :string, default: nil
  slot :inner_block, required: true
  slot :subtitle
  slot :actions
  slot :actions_header

  def header(assigns) do
    ~H"""
    <header class={[@class]} data-test="header-with-title">
      <div class="flex items-center justify-between gap-6">
        <div>
          <h1 class="text-lg font-semibold leading-8 text-zinc-800">
            {@title || render_slot(@inner_block)}
          </h1>
          <p :if={@subtitle != []} class="mt-2 text-sm leading-6 text-zinc-600" data-test="header-with-subtitle">
            {render_slot(@subtitle)}
          </p>
        </div>
        <div :if={@actions != [] || @actions_header != []} class="flex-none" data-test="header-with-actions">
          {render_slot(@actions_header)}
          {render_slot(@actions)}
        </div>
      </div>
    </header>
    """
  end

  @doc """
  Renders a back navigation link.

  ## Examples

      <.back navigate={~p"/posts"}>Back to posts</.back>
  """
  attr :navigate, :any, required: true
  slot :inner_block, required: true

  def back(assigns) do
    ~H"""
    <div class="mt-16">
      <.link navigate={@navigate} data-test="back-nav" class="text-sm font-semibold leading-6 text-zinc-900 hover:text-zinc-700">
        <.icon name="hero-arrow-left-solid" class="h-3 w-3" /> {render_slot(@inner_block)}
      </.link>
    </div>
    """
  end

  @doc """
  Renders a header with a table layout, similar to The Monospace Web.
  """
  attr :class, :string, default: nil
  slot :left, required: true
  slot :right
  slot :metadata

  def header_table(assigns) do
    assigns = assign_new(assigns, :class, fn -> "" end)
    assigns = assign_new(assigns, :metadata, fn -> [] end)

    ~H"""
    <div class={["header-wrapper", @class]}>
      <table class="header">
        <tr>
          <td class="content-cell">
            {render_slot(@left)}
          </td>
          <td class="width-min metadata-cell">
            <table :if={@right != []} class="metadata-table">
              {render_slot(@right)}
            </table>
          </td>
        </tr>
        <tr>
          <td class="author-cell">
            <div :if={@metadata != []} class="author-row">
              {render_slot(@metadata)}
            </div>
          </td>
          <td class="width-min"></td>
        </tr>
      </table>
    </div>
    """
  end

  @doc """
  Renders a navigation component.
  """
  attr :class, :string, default: nil
  slot :inner_block

  slot :item, required: false do
    attr :link, :any
    attr :active, :boolean
  end

  def nav(assigns) do
    ~H"""
    <nav class={["site-nav", @class]}>
      <div :if={@inner_block != []}>
        {render_slot(@inner_block)}
      </div>
      <div :if={@inner_block == []}>
        <a :for={item <- @item} href={item.link} class={if Map.get(item, :active, false), do: "active"}>
          {render_slot(item)}
        </a>
      </div>
    </nav>
    """
  end

  @doc """
  Renders a table header layout.
  """
  attr :class, :string, default: nil
  slot :left, required: true
  slot :right
  slot :metadata

  def table_header(assigns) do
    ~H"""
    <div class={["table-header", @class]} data-test="table-header">
      <div class="flex items-center justify-between">
        <div class="min-w-0 flex-1">
          {render_slot(@left)}
        </div>
        <div :if={@right != []} class="flex flex-none items-center gap-4">
          {render_slot(@right)}
        </div>
      </div>
      <div :if={@metadata != []} class="mt-2" data-test="table-header-with-metadata">
        {render_slot(@metadata)}
      </div>
    </div>
    """
  end

  @doc """
  Renders a layout container.
  """
  attr :class, :string, default: nil
  attr :type, :string, default: "content"
  slot :inner_block, required: true

  def layout_container(assigns) do
    ~H"""
    <div class={[@class]} data-test={"layout-#{@type}"} data-test-content-type={@type}>
      {render_slot(@inner_block)}
    </div>
    """
  end

  @doc """
  Renders a grid layout.
  """
  attr :class, :string, default: nil
  attr :type, :string, default: "basic"
  slot :inner_block, required: true

  def grid_layout(assigns) do
    ~H"""
    <div class={["grid-layout", @class]} data-test={"grid-layout-#{@type}"}>
      {render_slot(@inner_block)}
    </div>
    """
  end
end
