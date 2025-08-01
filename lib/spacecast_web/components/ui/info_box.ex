defmodule SpacecastWeb.Components.UI.InfoBox do
  @moduledoc """
  Information box component for tips and contextual information.

  This component provides a styled box for displaying helpful information,
  tips, warnings, or context to users while maintaining the monospace grid alignment.
  It supports different types (info, tip, warning, error) with appropriate styling.
  """
  use Phoenix.Component
  import SpacecastWeb.Components.MonoGrid

  @doc """
  Renders an information box.

  ## Examples

      <.info_box type={:info}>
        This is important information.
      </.info_box>
      
      <.info_box type={:warning} title="Warning">
        Be careful with this operation.
      </.info_box>
      
      <.info_box type={:tip} title="Pro Tip" dismissible={true}>
        You can use keyboard shortcut Alt+R to replay animations.
      </.info_box>
      
  ## Attributes

  * `id` - Optional unique identifier
  * `class` - Additional CSS classes to add
  * `type` - Type of info box: :info, :tip, :warning, :error (default: :info)
  * `title` - Optional title for the info box
  * `icon` - Whether to show an icon (default: true)
  * `dismissible` - Whether the box can be dismissed (default: false)
  * `bordered` - Whether the box has a border (default: true)
  """
  attr :id, :string, default: nil
  attr :class, :string, default: nil
  attr :type, :atom, default: :info, values: [:info, :tip, :warning, :error]
  attr :title, :string, default: nil
  attr :icon, :boolean, default: true
  attr :dismissible, :boolean, default: false
  attr :bordered, :boolean, default: true
  attr :rest, :global

  slot :inner_block, required: true

  def info_box(assigns) do
    # Map icon characters for each type
    icon_char =
      case assigns.type do
        :info -> "ℹ"
        :tip -> "✓"
        :warning -> "⚠"
        :error -> "✗"
      end

    # Set default title based on type if not provided
    assigns =
      if is_nil(assigns.title) do
        title =
          case assigns.type do
            :info -> "Information"
            :tip -> "Tip"
            :warning -> "Warning"
            :error -> "Error"
          end

        assign(assigns, :title, title)
      else
        assigns
      end

    # Add icon_char to assigns
    assigns = assign(assigns, :icon_char, icon_char)

    ~H"""
    <div
      id={@id}
      class={[
        "info-box",
        "info-box--#{@type}",
        @bordered && "info-box--bordered",
        @class
      ]}
      {@rest}
      phx-hook={@dismissible && "DismissibleInfoBox"}
    >
      <div class="info-box__header">
        <%= if @icon do %>
          <span class="info-box__icon">{@icon_char}</span>
        <% end %>
        <h4 class="info-box__title">{@title}</h4>
        <%= if @dismissible do %>
          <button type="button" class="info-box__close" aria-label="Close" phx-click="dismiss_info_box">
            ×
          </button>
        <% end %>
      </div>
      <div class="info-box__content">
        <.mono_grid cols={76}>
          <.mono_grid_row>
            <.mono_grid_cell cols={76}>
              {render_slot(@inner_block)}
            </.mono_grid_cell>
          </.mono_grid_row>
        </.mono_grid>
      </div>
    </div>
    """
  end

  @doc """
  Renders a keyboard shortcut within an info box or elsewhere.

  ## Examples

      <.kbd>Alt+R</.kbd>
      
  """
  attr :class, :string, default: nil
  attr :rest, :global

  slot :inner_block, required: true

  def kbd(assigns) do
    ~H"""
    <kbd class={["mono-kbd", @class]} {@rest}>
      {render_slot(@inner_block)}
    </kbd>
    """
  end
end
