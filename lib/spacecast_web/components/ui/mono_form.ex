defmodule SpacecastWeb.Components.UI.MonoForm do
  @moduledoc """
  Provides monospace form components for the application.
  These components are designed to work with the monospace theme.
  """
  use Phoenix.Component

  import SpacecastWeb.Components.UI.FormComponents,
    only: [simple_form: 1, input: 1, button: 1, label_tag: 1, error: 1]

  attr :for, :any, required: true, doc: "the data structure for the form"
  attr :as, :any, default: nil, doc: "the server side parameter to collect all input under"
  attr :rest, :global, doc: "arbitrary HTML attributes to apply to the form tag"
  slot :inner_block, required: true
  slot :actions, doc: "the slot for form actions"

  @doc """
  Renders a monospace form with slots for fields and actions.
  """
  def mono_form(assigns) do
    ~H"""
    <.simple_form :let={f} for={@for} as={@as} {@rest}>
      <div class="mt-10 space-y-8 bg-white">
        {render_slot(@inner_block, f)}
        <div :for={action <- @actions} class="mt-2 flex items-center justify-between gap-6">
          {render_slot(action, f)}
        </div>
      </div>
    </.simple_form>
    """
  end

  @doc """
  Renders a monospace input field.
  """
  def mono_input(assigns) do
    ~H"""
    <.input type={@type} field={@field} label={@label} value={@value} errors={@errors} class="mono-input" {@rest} />
    """
  end

  @doc """
  Renders a monospace button.
  """
  def mono_button(assigns) do
    ~H"""
    <.button type={@type} class="mono-button" {@rest}>
      {render_slot(@inner_block)}
    </.button>
    """
  end

  @doc """
  Renders a monospace label.
  """
  def mono_label(assigns) do
    ~H"""
    <.label_tag for={@for} class="mono-label">
      {render_slot(@inner_block)}
    </.label_tag>
    """
  end

  @doc """
  Renders a monospace error message.
  """
  def mono_error(assigns) do
    ~H"""
    <.error class="mono-error">
      {render_slot(@inner_block)}
    </.error>
    """
  end
end
