defmodule SpacecastWeb.Components.BaseComponents do
  @moduledoc """
  Base components and utilities shared between other component modules.

  This module breaks the circular dependency between CoreComponents
  and other UI component modules.
  """
  use Phoenix.Component
  alias Phoenix.LiveView.JS

  # Common attribute definitions
  attr :id, :string, default: nil
  attr :class, :string, default: nil
  attr :rest, :global

  # Common layout/structure patterns
  @doc """
  Renders a container div with optional id, class, and additional attributes.
  The inner content is rendered via the inner_block slot.
  """
  @spec container(map()) :: Phoenix.LiveView.Rendered.t()
  def container(assigns) do
    ~H"""
    <div id={@id} class={[@class]} {@rest}>
      {render_slot(@inner_block)}
    </div>
    """
  end

  # Common icon/button patterns
  @doc """
  Renders an icon span with optional class and additional attributes.
  The inner content is rendered via the inner_block slot.
  """
  @spec icon(map()) :: Phoenix.LiveView.Rendered.t()
  def icon(assigns) do
    assigns = assign_new(assigns, :class, fn -> "" end)

    ~H"""
    <span class={["icon", @class]} {@rest}>
      {render_slot(@inner_block)}
    </span>
    """
  end

  # Common modal-related helpers
  @doc """
  Renders a modal container div with fade-out transition on removal.
  The inner content is rendered via the inner_block slot.
  """
  @spec modal_container(map()) :: Phoenix.LiveView.Rendered.t()
  def modal_container(assigns) do
    ~H"""
    <div id={@id} class={["modal-container", @class]} phx-remove={JS.transition("fade-out")} {@rest}>
      {render_slot(@inner_block)}
    </div>
    """
  end

  # Common utility functions
  @doc """
  Hides a modal by id using JS transitions.
  Returns a JS command.
  """
  @spec hide_modal(String.t()) :: JS.t()
  def hide_modal(id) when is_binary(id) do
    %JS{}
    |> JS.hide(to: "##{id}")
    |> JS.hide(to: "##{id}-bg", transition: "fade-out")
    |> JS.hide(to: "##{id}-container", transition: "fade-out-scale")
  end

  # Overload to support passing in an existing JS command
  @doc """
  Hides a modal by id using an existing JS command, chaining transitions.
  Returns the updated JS command.
  """
  @spec hide_modal(JS.t(), String.t()) :: JS.t()
  def hide_modal(js, id) when is_binary(id) do
    js
    |> JS.hide(to: "##{id}")
    |> JS.hide(to: "##{id}-bg", transition: "fade-out")
    |> JS.hide(to: "##{id}-container", transition: "fade-out-scale")
  end

  # Shared utility functions
  @doc """
  Generates a unique DOM id with the given prefix using a UUID.
  """
  @spec generate_id(String.t()) :: String.t()
  def generate_id(prefix), do: "#{prefix}-#{Ecto.UUID.generate()}"
end
