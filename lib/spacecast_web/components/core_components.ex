defmodule SpacecastWeb.CoreComponents do
  @moduledoc """
  Provides core UI components.

  At first glance, this module may seem daunting, but its goal is to provide
  core building blocks for your application, such as modals, tables, and
  forms. The components consist mostly of markup and are well-documented
  with doc strings and declarative assigns. You may customize and style
  them in any way you want, based on your application growth and needs.

  The default components use Tailwind CSS, a utility-first CSS framework.
  See the [Tailwind CSS documentation](https://tailwindcss.com) to learn
  how to customize them or feel free to swap in another framework altogether.

  Icons are provided by [heroicons](https://heroicons.com). See `icon/1` for usage.

  This module imports and re-exports components from specialized modules:
  - ModalComponents - modal and flash related components
  - FormComponents - form, input, and validation related components
  - TableComponents - table and list components
  - LayoutComponents - layout and navigation components
  - Nav - navigation links
  - DebugGrid - debug grid for monospace layout
  """
  use Phoenix.Component
  use Gettext, backend: SpacecastWeb.Gettext

  alias Phoenix.LiveView.JS

  attr :id, :string, required: true
  attr :show, :boolean, default: false
  attr :on_cancel, JS, default: %JS{}
  attr :type, :string, default: nil
  attr :class, :string, default: nil
  attr :rest, :global
  slot :inner_block, required: true
  attr :flash, :map, default: %{}, doc: "the map of flash messages to display"
  attr :title, :string, default: nil
  attr :kind, :atom, values: [:info, :error], doc: "used for styling and flash lookup"
  attr :id_flash, :string, doc: "the optional id of flash container"
  attr :flash_group_id, :string, default: "flash-group", doc: "the optional id of flash container"
  attr :for, :any, required: true, doc: "the data structure for the form"
  attr :as, :any, default: nil, doc: "the server side parameter to collect all input under"
  attr :row_id, :any, default: nil, doc: "the function for generating the row id"
  attr :row_click, :any, default: nil, doc: "the function for handling phx-click on each row"

  attr :row_item, :any,
    default: &Function.identity/1,
    doc: "the function for mapping each row before calling the :col and :action slots"

  slot :action, doc: "the slot for showing user actions in the last table column"
  attr :checked, :boolean, doc: "the checked flag for checkbox inputs"
  attr :prompt, :string, default: nil, doc: "the prompt for select inputs"
  attr :options, :list, doc: "the options to pass to Phoenix.HTML.Form.options_for_select/2"
  attr :multiple, :boolean, default: false, doc: "the multiple flag for select inputs"
  attr :errors, :list, default: []
  attr :disabled, :boolean, default: false
  attr :name, :any
  attr :label, :string, default: nil
  attr :value, :any

  attr :type_input, :string,
    default: "text",
    values: ~w(checkbox color date datetime-local email file month number password
               range search select tel text textarea time url week)

  attr :field, Phoenix.HTML.FormField, doc: "a form field struct retrieved from the form, for example: @form[:email]"

  attr :disabled_button, :boolean, default: false
  slot :inner_block_button, required: true
  slot :inner_block_simple_form, required: true
  slot :actions, doc: "the slot for form actions, such as a submit button"
  attr :navigate, :any, required: true
  slot :subtitle
  slot :actions_header
  attr :count, :integer
  attr :line_height, :string, required: true
  slot :item

  @doc """
  Renders a modal dialog with optional show/hide transitions and cancel handling.
  """
  @spec modal(map()) :: Phoenix.LiveView.Rendered.t()
  def modal(assigns) do
    ~H"""
    <div id={@id} phx-mounted={@show && show_modal(@id)} phx-remove={hide_modal(@id)} data-cancel={JS.exec(@on_cancel, "phx-remove")} class="relative z-50 hidden">
      <div id={"#{@id}-bg"} class="bg-gray-500/75 transition-opacity fixed inset-0" aria-hidden="true" />
      <div class="fixed inset-0 overflow-y-auto" aria-labelledby={"#{@id}-title"} aria-describedby={"#{@id}-description"} role="dialog" aria-modal="true" tabindex="0">
        <div class="flex min-h-full items-center justify-center">
          <div class="w-full max-w-3xl overflow-hidden bg-white shadow-2xl sm:rounded-lg">
            <div class="bg-white px-4 pb-4 pt-5 sm:p-6 sm:pb-4">
              <div class="sm:flex sm:items-start">
                <div class="mt-3 text-center sm:ml-4 sm:mt-0 sm:text-left">
                  <h3 class="text-lg font-semibold leading-6 text-zinc-800" id={"#{@id}-title"}>
                    {@title}
                  </h3>
                  <div class="mt-2" id={"#{@id}-description"}>
                    {render_slot(@inner_block)}
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Renders a flash message for info or error notifications.
  """
  @spec flash(map()) :: Phoenix.LiveView.Rendered.t()
  def flash(assigns) do
    assigns = assign_new(assigns, :id_flash, fn -> "flash-#{assigns.kind}" end)
    assigns = assign_new(assigns, :inner_block, fn -> nil end)
    assigns = assign_new(assigns, :flash, fn -> %{} end)

    ~H"""
    <div
      :if={msg = Phoenix.Flash.get(@flash, @kind)}
      id={@id_flash}
      phx-click={JS.push("lv:clear-flash", value: %{key: @kind}) |> hide("##{@id_flash}")}
      role="alert"
      class={[
        "fixed top-2 right-2 mr-2 w-80 sm:w-96 z-50 rounded-lg p-3 ring-1",
        (@kind == :info or @kind == "info") && "alert-success bg-emerald-50 text-emerald-800 ring-emerald-500 fill-cyan-900",
        (@kind == :error or @kind == "error") && "alert-error bg-rose-50 text-rose-900 shadow-md ring-rose-500 fill-rose-900"
      ]}
      data-test-id={((@kind == :info or @kind == "info") && "flash-success") || ((@kind == :error or @kind == "error") && "flash-error")}
    >
      <p :if={@title} class="flex items-center gap-1.5 text-sm font-semibold leading-6">
        <.icon :if={@kind == :info or @kind == "info"} name="hero-information-circle-mini" class="h-4 w-4" />
        <.icon :if={@kind == :error or @kind == "error"} name="hero-exclamation-circle-mini" class="h-4 w-4" /> {@title}
      </p>
      <p class="mt-2 text-sm leading-5">{msg}</p>
      <button type="button" class="group absolute top-1 right-1 p-2" aria-label={gettext("close")}>
        <.icon name="hero-x-mark-solid" class="h-5 w-5 opacity-40 group-hover:opacity-70" />
      </button>
    </div>
    """
  end

  @doc """
  Renders a group of flash messages for info and error notifications.
  """
  @spec flash_group(map()) :: Phoenix.LiveView.Rendered.t()
  def flash_group(assigns) do
    assigns = assign_new(assigns, :flash_group_id, fn -> "flash-group" end)

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

  @doc """
  Renders a header with optional subtitle and actions.
  """
  @spec header(map()) :: Phoenix.LiveView.Rendered.t()
  def header(assigns) do
    ~H"""
    <header class={[@actions_header != [] && "flex items-center justify-between gap-6", @class]}>
      <div>
        <h1 class="text-lg font-semibold leading-8 text-zinc-800">
          {render_slot(@inner_block)}
        </h1>
        <p :if={@subtitle != []} class="mt-2 text-sm leading-6 text-zinc-600">
          {render_slot(@subtitle)}
        </p>
      </div>
      <div class="flex-none">{render_slot(@actions_header)}</div>
    </header>
    """
  end

  @doc """
  Renders a table with columns, rows, and optional actions.
  """
  @spec table(map()) :: Phoenix.LiveView.Rendered.t()
  def table(assigns) do
    assigns =
      case assigns do
        %{rows: %Phoenix.LiveView.LiveStream{}} ->
          assign(assigns, :row_id, assigns[:row_id] || fn {id, _item} -> id end)

        _ ->
          assigns
      end

    assigns = Map.put_new(assigns, :row_item, &Function.identity/1)

    ~H"""
    <div class="overflow-y-auto px-4 sm:overflow-visible sm:px-0">
      <table class="w-[40rem] mt-11 sm:w-full">
        <thead class="text-sm text-left leading-6 text-zinc-500">
          <tr>
            <th :for={col <- @col} class="p-0 pb-4 pr-6 font-normal">{col[:label]}</th>
            <th :if={@action != []} class="relative p-0 pb-4">
              <span class="sr-only">{gettext("Actions")}</span>
            </th>
          </tr>
        </thead>
        <tbody id={@id} phx-update={match?(%Phoenix.LiveView.LiveStream{}, @rows) && "stream"} class="relative divide-y divide-zinc-100 border-t border-zinc-200 text-sm leading-6 text-zinc-700">
          <tr :for={row <- @rows} id={@row_id && @row_id.(row)} class="group hover:bg-zinc-50">
            <td :for={{col, i} <- Enum.with_index(@col)} phx-click={@row_click && @row_click.(row)} class={["relative p-0", @row_click && "hover:cursor-pointer"]}>
              <div class="block py-4 pr-6">
                <span class="absolute -inset-y-px right-0 -left-4 group-hover:bg-zinc-50 sm:rounded-l-xl" />
                <span class={["relative", i == 0 && "font-semibold text-zinc-900"]}>
                  {render_slot(col, @row_item.(row))}
                </span>
              </div>
            </td>
            <td :if={@action != []} class="relative w-14 p-0">
              <div class="relative whitespace-nowrap py-4 text-right text-sm font-medium">
                <span class="absolute -inset-y-px -right-4 left-0 group-hover:bg-zinc-50 sm:rounded-r-xl" />
                <span :for={action <- @action} class="relative ml-4 font-semibold leading-6 text-zinc-900 hover:text-zinc-700">
                  {render_slot(action, @row_item.(row))}
                </span>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
    </div>
    """
  end

  @doc """
  Renders a list of items in a description list format.
  """
  @spec list(map()) :: Phoenix.LiveView.Rendered.t()
  def list(assigns) do
    ~H"""
    <div class="mt-14">
      <dl class="-my-4 divide-y divide-zinc-100">
        <div :for={item <- @item} class="flex gap-4 py-4 text-sm leading-6 sm:gap-8">
          <dt class="w-1/4 flex-none text-zinc-500">{item.title}</dt>
          <dd class="text-zinc-700">{render_slot(item)}</dd>
        </div>
      </dl>
    </div>
    """
  end

  @doc """
  Renders a back link.
  """
  @spec back(map()) :: Phoenix.LiveView.Rendered.t()
  def back(assigns) do
    ~H"""
    <div class="mt-16">
      <a href={@navigate} data-phx-link="redirect" data-phx-link-state="push" class="text-sm font-semibold leading-6 text-zinc-900 hover:text-zinc-700">
        <span class="hero-arrow-left-solid h-3 w-3"></span>
        {render_slot(@inner_block)}
      </a>
    </div>
    """
  end

  @doc """
  Renders a heroicon or generic icon span.
  """
  def icon(%{name: "hero-" <> _} = assigns) do
    ~H"""
    <span class={[@name, @class]} />
    """
  end

  @doc false
  def icon(assigns) do
    ~H"""
    <span class={[@name, @class]} />
    """
  end

  @doc """
  Shows a DOM element with a transition using JS commands.
  """
  @spec show(Phoenix.LiveView.JS.t(), String.t()) :: Phoenix.LiveView.JS.t()
  @spec show(String.t()) :: Phoenix.LiveView.JS.t()
  def show(js \\ %JS{}, selector) do
    JS.show(js,
      to: selector,
      time: 300,
      transition:
        {"transition-all transform ease-out duration-300", "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95",
         "opacity-100 translate-y-0 sm:scale-100"}
    )
  end

  @doc """
  Hides a DOM element with a transition using JS commands.
  """
  @spec hide(Phoenix.LiveView.JS.t(), String.t()) :: Phoenix.LiveView.JS.t()
  @spec hide(String.t()) :: Phoenix.LiveView.JS.t()
  def hide(js \\ %JS{}, selector) do
    JS.hide(js,
      to: selector,
      time: 200,
      transition:
        {"transition-all transform ease-in duration-200", "opacity-100 translate-y-0 sm:scale-100",
         "opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"}
    )
  end

  @doc """
  Shows a modal by id using JS transitions.
  """
  @spec show_modal(Phoenix.LiveView.JS.t(), String.t()) :: Phoenix.LiveView.JS.t()
  @spec show_modal(String.t()) :: Phoenix.LiveView.JS.t()
  def show_modal(js \\ %JS{}, id) when is_binary(id) do
    js
    |> JS.show(to: "##{id}")
    |> JS.show(
      to: "##{id}-bg",
      time: 300,
      transition: {"transition-all transform ease-out duration-300", "opacity-0", "opacity-100"}
    )
    |> show("##{id}-container")
    |> JS.add_class("overflow-hidden", to: "body")
    |> JS.focus_first(to: "##{id}-content")
  end

  @doc """
  Hides a modal by id using JS transitions.
  """
  @spec hide_modal(Phoenix.LiveView.JS.t(), String.t()) :: Phoenix.LiveView.JS.t()
  @spec hide_modal(String.t()) :: Phoenix.LiveView.JS.t()
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
  Translates a single error tuple using Gettext.
  """
  @spec translate_error({String.t(), keyword()}) :: String.t()
  def translate_error({msg, opts}) do
    if count = opts[:count] do
      Gettext.dngettext(SpacecastWeb.Gettext, "errors", msg, msg, count, opts)
    else
      Gettext.dgettext(SpacecastWeb.Gettext, "errors", msg, opts)
    end
  end

  @doc """
  Translates all errors for a given field from a list of error tuples.
  """
  @spec translate_errors(list(), atom()) :: list(String.t())
  def translate_errors(errors, field) when is_list(errors) do
    for {^field, {msg, opts}} <- errors, do: translate_error({msg, opts})
  end

  @doc """
  Renders a button.
  """
  attr :type, :string, default: "button"
  attr :class, :string, default: nil
  attr :rest, :global
  slot :inner_block, required: true

  def button(assigns) do
    ~H"""
    <button
      type={@type}
      class={[
        "phx-submit-loading:opacity-75 rounded-lg bg-zinc-900 py-2 px-3 text-sm font-semibold leading-6 text-white active:text-white/80 hover:bg-zinc-700",
        @class
      ]}
      {@rest}
    >
      {render_slot(@inner_block)}
    </button>
    """
  end

  @doc """
  Renders a theme toggle button group for switching themes.
  """
  @spec theme_toggle(map()) :: Phoenix.LiveView.Rendered.t()
  def theme_toggle(assigns) do
    ~H"""
    <div id="theme-toggle" class="theme-toggle" phx-hook="ThemeToggle">
      <button id="light-theme" data-theme="light" phx-click={JS.dispatch("theme-set", detail: %{theme: "light-theme"})}>
        ⬜️
      </button>
      <button id="dim-theme" data-theme="dim" phx-click={JS.dispatch("theme-set", detail: %{theme: "dim-theme"})}>
        🟪
      </button>
      <button id="dark-theme" data-theme="dark" phx-click={JS.dispatch("theme-set", detail: %{theme: "dark-theme"})}>
        ⬛️
      </button>
    </div>
    """
  end

  @doc """
  Renders a navigation bar with links.
  """
  @spec nav(map()) :: Phoenix.LiveView.Rendered.t()
  def nav(assigns) do
    ~H"""
    <nav class="site-nav">
      <a href="/">Home</a>
      <a href="/style-guide">Style Guide</a>
    </nav>
    """
  end

  @doc """
  Renders a header table with metadata.
  """
  @spec header_table(map()) :: Phoenix.LiveView.Rendered.t()
  def header_table(assigns) do
    ~H"""
    <header class="site-header">
      <h1>{@title}</h1>
      <table class="metadata">
        <tr>
          <td>Version:</td>
          <td>{@version}</td>
        </tr>
        <tr>
          <td>Updated:</td>
          <td>{@updated}</td>
        </tr>
        <tr>
          <td>Author:</td>
          <td>{@author}</td>
        </tr>
        <tr>
          <td>License:</td>
          <td>{@license}</td>
        </tr>
        <tr>
          <td>Line height:</td>
          <td>{@line_height}</td>
        </tr>
      </table>
    </header>
    """
  end

  @doc """
  Renders an error message.
  """
  @spec error(map()) :: Phoenix.LiveView.Rendered.t()
  def error(assigns) do
    ~H"""
    <p class="mt-2 text-sm text-red-600">
      {render_slot(@inner_block)}
    </p>
    """
  end

  @doc """
  Renders an input with label and error messages.
  """
  @spec input(map()) :: Phoenix.LiveView.Rendered.t()
  def input(assigns) do
    ~H"""
    <div class="form-group">
      <label :if={@label} for={@id} class="block text-sm font-medium text-gray-700">
        {@label}
      </label>
      <div class="mt-1">
        <input
          type={@type_input}
          name={@name}
          id={@id}
          value={@value}
          class={[
            "block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm",
            @errors != [] && "border-red-300"
          ]}
          {@rest}
        />
      </div>
      <.error :for={msg <- @errors}>
        {msg}
      </.error>
    </div>
    """
  end
end
