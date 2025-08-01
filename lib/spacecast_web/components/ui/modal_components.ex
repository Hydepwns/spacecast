defmodule SpacecastWeb.Components.UI.ModalComponents do
  @moduledoc """
  Provides modal UI components.
  """
  use Phoenix.Component
  use Gettext, backend: SpacecastWeb.Gettext

  alias Phoenix.LiveView.JS
  import SpacecastWeb.Components.BaseComponents

  @doc """
  Renders a modal.

  ## Examples

      <.modal id="confirm-modal">
        This is a modal.
      </.modal>

  JS commands may be passed to the `:on_cancel` to configure
  the closing/cancel event, for example:

      <.modal id="confirm" on_cancel={JS.navigate(~p"/posts")}>
        This is another modal.
      </.modal>

  """
  attr :id, :string, required: true
  attr :show, :boolean, default: false
  attr :on_cancel, JS, default: %JS{}
  slot :inner_block, required: true

  def modal(assigns) do
    ~H"""
    <.modal_container id={@id} class={@class}>
      <div class="modal-content">
        <div class="modal-header">
          {render_slot(@header)}
        </div>
        <div class="modal-body">
          {render_slot(@inner_block)}
        </div>
        <div class="modal-footer">
          {render_slot(@footer)}
        </div>
      </div>
    </.modal_container>
    """
  end

  @doc """
  Renders flash notices.

  ## Examples

      <.flash kind={:info} flash={@flash} />
      <.flash kind={:info} phx-mounted={show("#flash")}>Welcome Back!</.flash>
  """
  attr :id, :string, doc: "the optional id of flash container"
  attr :flash, :map, default: %{}, doc: "the map of flash messages to display"
  attr :title, :string, default: nil
  attr :kind, :atom, values: [:info, :error], doc: "used for styling and flash lookup"
  attr :rest, :global, doc: "the arbitrary HTML attributes to add to the flash container"
  attr :autoshow, :boolean, default: true, doc: "whether to auto show the flash"
  attr :close, :boolean, default: true, doc: "whether the flash can be closed"
  attr :myself, :any, default: nil
  slot :inner_block, doc: "the optional inner block that renders the flash message"

  def flash(assigns) do
    ~H"""
    <div
      :if={msg = render_slot(@inner_block) || Phoenix.Flash.get(@flash, @kind)}
      id={@id || "flash-#{@kind}"}
      phx-mounted={@autoshow && show("##{@id || "flash-#{@kind}"}")}
      phx-click={JS.push("lv:clear-flash", value: %{key: @kind}) |> hide("##{@id || "flash-#{@kind}"}")}
      role="alert"
      class={[
        "fixed top-2 right-2 w-80 sm:w-96 z-50 rounded-lg p-3 ring-1",
        @kind == :info && "bg-emerald-50 text-emerald-800 ring-emerald-500 fill-cyan-900",
        @kind == :error && "bg-rose-50 text-rose-900 shadow-md ring-rose-500 fill-rose-900"
      ]}
      {@rest}
    >
      <p :if={@title} class="flex items-center gap-1.5 text-sm font-semibold leading-6">
        <.icon :if={@kind == :info} name="hero-information-circle-mini" class="h-4 w-4" />
        <.icon :if={@kind == :error} name="hero-exclamation-circle-mini" class="h-4 w-4" /> {@title}
      </p>
      <p class="mt-2 text-sm leading-5">{msg}</p>
      <button :if={@close} type="button" class="group absolute top-1 right-1 p-2" aria-label={gettext("close")}>
        <.icon name="hero-x-mark-solid" class="h-5 w-5 opacity-40 group-hover:opacity-70" />
      </button>
    </div>
    """
  end

  @doc """
  Shows the flash group with standard titles and content.

  ## Examples

      <.flash_group flash={@flash} flash_group_id="modal-flash-group" />
  """
  attr :flash, :map, required: true, doc: "the map of flash messages"
  attr :flash_group_id, :string, default: "flash-group", doc: "the optional id of flash container"

  def flash_group(assigns) do
    ~H"""
    <div id={@flash_group_id}>
      <.flash kind={:info} title={gettext("Success!")} flash={@flash} />
      <.flash kind={:error} title={gettext("Error!")} flash={@flash} />
      <.flash id="client-error" kind={:error} title={gettext("We can't find the internet")} phx-disconnected={show(".phx-client-error #client-error")} phx-connected={hide("#client-error")} hidden>
        {gettext("Attempting to reconnect")} <.icon name="hero-arrow-path" class="ml-1 h-3 w-3 animate-spin" />
      </.flash>

      <.flash id="server-error" kind={:error} title={gettext("Something went wrong!")} phx-disconnected={show(".phx-server-error #server-error")} phx-connected={hide("#server-error")} hidden>
        {gettext("Hang in there while we get back on track")} <.icon name="hero-arrow-path" class="ml-1 h-3 w-3 animate-spin" />
      </.flash>
    </div>
    """
  end

  ## JS Commands

  def show(js \\ %JS{}, selector) do
    JS.show(js,
      to: selector,
      transition:
        {"transition-all transform ease-out duration-300",
         "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95",
         "opacity-100 translate-y-0 sm:scale-100"}
    )
  end

  def hide(js \\ %JS{}, selector) do
    JS.hide(js,
      to: selector,
      time: 200,
      transition:
        {"transition-all transform ease-in duration-200",
         "opacity-100 translate-y-0 sm:scale-100",
         "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"}
    )
  end

  def show_modal(js \\ %JS{}, id) when is_binary(id) do
    js
    |> JS.show(to: "##{id}")
    |> JS.show(
      to: "##{id}-bg",
      transition: {"transition-all transform ease-out duration-300", "opacity-0", "opacity-100"}
    )
    |> show("##{id}-container")
    |> JS.add_class("overflow-hidden", to: "body")
    |> JS.focus_first(to: "##{id}-content")
  end

  def hide_modal(js \\ %JS{}, id) do
    js
    |> JS.hide(
      to: "##{id}-bg",
      transition: {"transition-all transform ease-in duration-200", "opacity-100", "opacity-0"}
    )
    |> hide("##{id}-container")
    |> JS.hide(to: "##{id}", transition: {"block", "block", "hidden"})
    |> JS.remove_class("overflow-hidden", to: "body")
    |> JS.pop_focus()
  end

  @doc """
  Wraps the content with a form tag that handles phx-submitted automatically.

  For a more dynamic form, use `Phoenix.Component.form/1` instead.
  """
  attr :for, :any, required: true, doc: "the datastructure for the form"
  attr :as, :any, default: nil, doc: "the server side parameter to collect all input under"

  attr :rest, :global,
    include: ~w(autocomplete name rel action enctype method novalidate target multipart)

  slot :inner_block, required: true

  def focus_wrap(assigns) do
    ~H"""
    <div id={@id} phx-hook="Phoenix.FocusWrap" class={@class}>
      {render_slot(@inner_block)}
    </div>
    """
  end
end
