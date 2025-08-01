defmodule SpacecastWeb.Components.Common.CoreComponents do
  @moduledoc """
  Provides core UI components.
  """

  use Phoenix.Component
  use Gettext, backend: SpacecastWeb.Gettext

  alias Phoenix.LiveView.JS

  # Common attributes for most components
  attr :id, :any, default: nil
  attr :name, :any, default: nil
  attr :label, :string, default: nil
  attr :value, :any, default: nil
  attr :error, :string, default: nil
  attr :required, :boolean, default: false
  attr :disabled, :boolean, default: false
  attr :class, :string, default: nil

  # Common slots
  slot :inner_block, required: false
  slot :subtitle
  slot :actions
  slot :actions_header

  # Modal component
  attr :show, :boolean, default: false
  attr :on_cancel, JS, default: %JS{}
  attr :title, :string, default: nil

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

  # Label component
  attr :for, :any, required: true
  slot :inner_block, required: false

  def label(assigns) do
    ~H"""
    <label for={@for} class="block text-sm font-medium text-gray-700">
      {render_slot(@inner_block)}
    </label>
    """
  end

  # Error component
  def error(assigns) do
    ~H"""
    <p class="mt-2 text-sm text-red-600">
      <.icon name="hero-exclamation-circle-mini" class="h-4 w-4" />
      {render_slot(@inner_block)}
    </p>
    """
  end

  # Header component
  attr :title, :string, default: nil

  def header(assigns) do
    ~H"""
    <header class="flex items-center justify-between gap-6">
      <div>
        <h1 class="text-lg font-semibold leading-8 text-zinc-800">
          {@title || render_slot(@inner_block)}
        </h1>
        <p :if={@subtitle != []} class="mt-2 text-sm leading-6 text-zinc-600">
          {render_slot(@subtitle)}
        </p>
      </div>
      <div :if={@actions_header != []} class="flex-none">
        {render_slot(@actions_header)}
      </div>
    </header>
    """
  end

  # Icon component
  attr :name, :string, required: true
  attr :class, :string, default: nil

  def icon(assigns) do
    ~H"""
    <span class={[@name, @class]} />
    """
  end

  # Harmonize slot keys to use :inner_block for consistency
  defp harmonize_slots(assigns) do
    harmonized =
      assigns
      |> Map.keys()
      |> Enum.filter(&slot_key?/1)
      |> Enum.reduce(assigns, fn key, acc ->
        harmonize_slot_key(key, acc)
      end)

    harmonized
  end

  defp harmonize_slot_key(key, acc) do
    case key do
      :inner_block ->
        acc

      :inner_block_button ->
        Map.put(acc, :inner_block, Map.get(acc, key))

      :inner_block_simple_form ->
        Map.put(acc, :inner_block, Map.get(acc, key))

      key when is_atom(key) ->
        harmonize_atom_key(key, acc)

      _ ->
        Map.put(acc, :inner_block, Map.get(acc, key))
    end
  end

  defp harmonize_atom_key(key, acc) do
    key_str = Atom.to_string(key)

    if String.ends_with?(key_str, "_item") do
      acc
    else
      Map.put(acc, :inner_block, Map.get(acc, key))
    end
  end

  # Check if a key is a slot key
  defp slot_key?(:inner_block), do: true
  defp slot_key?(:inner_block_button), do: true
  defp slot_key?(:inner_block_simple_form), do: true

  defp slot_key?(key) when is_atom(key) do
    key_str = Atom.to_string(key)

    String.ends_with?(key_str, "_item") or
      String.starts_with?(key_str, "inner_block_")
  end

  defp slot_key?(_), do: false

  defp call_slot_function(slot_fun, assigns) do
    try do
      slot_fun.(assigns)
    rescue
      _e -> ""
    end
  end

  defp render_list_item(item, assigns) do
    case item do
      %{inner_block: block, title: title} when is_function(block) ->
        content = render_slot_content(block, assigns)
        "#{title}: #{content}"

      %{inner_block: block} when is_function(block) ->
        render_slot_content(block, assigns)

      _other ->
        inspect(item)
    end
  end

  defp render_slot_content(slot_value, assigns \\ %{}, _slot_key) do
    case slot_value do
      nil ->
        ""

      slot_value when is_function(slot_value) ->
        call_slot_function(slot_value, assigns)

      slot_value when is_list(slot_value) ->
        Enum.map_join(slot_value, "", &render_list_item(&1, assigns))

      slot_value when is_binary(slot_value) ->
        slot_value

      _ ->
        inspect(slot_value)
    end
  end

  defp get_list_slot_content(assigns) do
    case assigns[:item] do
      nil ->
        render_slot_content(assigns[:inner_block], assigns, :inner_block)

      items when is_list(items) ->
        Enum.map_join(items, "", fn item -> render_list_item(item, assigns) end)

      val ->
        render_slot_content(val, assigns, :inner_block)
    end
  end

  def list(assigns) do
    assigns = harmonize_slots(assigns)
    assigns = if Map.has_key?(assigns, :class), do: assigns, else: Map.put(assigns, :class, "")
    slot_content = get_list_slot_content(assigns)
    assigns = assign(assigns, :inner_block, slot_content)

    ~H"""
    <ul class={["list", @class]}>
      {@inner_block}
    </ul>
    """
  end

  # Back component
  attr :href, :string, default: nil
  attr :patch, :string, default: nil
  attr :navigate, :string, default: nil
  attr :class, :string, default: nil
  slot :inner_block, required: false

  def back(assigns) do
    assigns = harmonize_slots(assigns)
    slot_content = render_slot_content(assigns[:inner_block], assigns)
    assigns = assign(assigns, :inner_block, slot_content)

    ~H"""
    <a href={@navigate} class={["back", @class]}>
      {@inner_block}
    </a>
    """
  end

  # Icon component
  attr :name, :string, required: true
  attr :class, :string, default: nil
  slot :inner_block, required: false

  def icon_component(assigns) do
    assigns = harmonize_slots(assigns)

    ~H"""
    <span class={["icon", @class]}>
      {render_slot(@inner_block)}
    </span>
    """
  end

  # Show div component
  attr :class, :string, default: nil
  slot :inner_block, required: false

  def show_div(assigns) do
    assigns = harmonize_slots(assigns)

    ~H"""
    <div class={["show", @class]}>
      {render_slot(@inner_block)}
    </div>
    """
  end

  # Hide div component
  attr :class, :string, default: nil
  slot :inner_block, required: false

  def hide_div(assigns) do
    assigns = harmonize_slots(assigns)

    ~H"""
    <div class={["hide", @class]}>
      {render_slot(@inner_block)}
    </div>
    """
  end

  def table(assigns) do
    assigns = harmonize_slots(assigns)
    assigns = if Map.has_key?(assigns, :class), do: assigns, else: Map.put(assigns, :class, "")
    slot_content = get_table_slot_content(assigns)
    assigns = assign(assigns, :inner_block, slot_content)

    ~H"""
    <div class="table-responsive">
      <table id={@id} class={["table", @class]}>
        {@inner_block}
      </table>
    </div>
    """
  end

  defp get_table_slot_content(assigns) do
    case {assigns[:row_item], assigns[:rows]} do
      {row_item_fn, rows} when is_function(row_item_fn) and is_list(rows) ->
        render_table_rows(rows, row_item_fn, assigns)

      {nil, _} ->
        render_slot_content(assigns[:inner_block], assigns, :inner_block)

      {val, _} ->
        render_table_fallback(val, assigns)
    end
  end

  defp render_table_rows(rows, row_item_fn, assigns) do
    Enum.map_join(rows, "", fn row -> render_table_row(row, row_item_fn, assigns) end)
  end

  defp render_table_row(row, row_item_fn, assigns) do
    arity = Function.info(row_item_fn, :arity)

    try do
      result =
        case arity do
          {:arity, 2} -> row_item_fn.(row, assigns)
          {:arity, 1} -> row_item_fn.(row)
          _ -> inspect(row)
        end

      format_table_result(result)
    rescue
      _ -> inspect(row)
    end
  end

  defp format_table_result(result) do
    cond do
      is_binary(result) -> result
      is_map(result) -> "<td>" <> inspect(result) <> "</td>"
      true -> to_string(result)
    end
  end

  defp render_table_fallback(val, assigns) do
    content = render_slot_content(val, assigns, :inner_block)

    if is_map(content) and not is_binary(content) do
      inspect(content)
    else
      content
    end
  end

  # Link component
  attr :href, :string, default: nil
  attr :patch, :string, default: nil
  attr :navigate, :string, default: nil
  attr :phx_click, :any, default: nil
  attr :phx_value, :any, default: nil
  # Add the hyphenated versions for template compatibility
  attr :"phx-click", :any, default: nil
  attr :"phx-value", :any, default: nil
  slot :inner_block, required: false

  def link_component(assigns) do
    assigns = harmonize_slots(assigns)

    # Handle both underscore and hyphenated attribute names
    phx_click = assigns[:"phx-click"] || assigns[:phx_click]
    phx_value = assigns[:"phx-value"] || assigns[:phx_value]

    assigns =
      assigns
      |> assign(:phx_click, phx_click)
      |> assign(:phx_value, phx_value)

    ~H"""
    <.link class={["link", @class]} href={@href} patch={@patch} navigate={@navigate} phx-click={@phx_click} phx-value={@phx_value}>
      {render_slot(@inner_block)}
    </.link>
    """
  end

  # Button component
  attr :type, :string, default: "button"
  attr :variant, :string, default: nil
  attr :size, :string, default: nil
  attr :disabled, :boolean, default: false
  attr :phx_click, :string, default: nil
  attr :phx_submit, :string, default: nil
  attr :phx_value, :any, default: nil
  attr :phx_value_role, :string, default: nil
  attr :phx_value_theme, :string, default: nil
  attr :phx_disable_with, :string, default: nil
  attr :data_test_id, :string, default: nil
  attr :"phx-click", :string, default: nil
  attr :"phx-submit", :string, default: nil
  attr :"phx-value", :any, default: nil
  attr :"phx-value-role", :string, default: nil
  attr :"phx-value-theme", :string, default: nil
  attr :"phx-disable-with", :string, default: nil
  attr :"data-test-id", :string, default: nil
  slot :inner_block, required: false

  def button(assigns) do
    assigns = harmonize_slots(assigns)
    assigns = prepare_button_assigns(assigns)

    ~H"""
    <button type={@type} class={@class} phx-click={@phx_click} phx-submit={@phx_submit} phx-value={@phx_value} phx-value-role={@phx_value_role} phx-value-theme={@phx_value_theme} phx-disable-with={@phx_disable_with} data-test-id={@data_test_id} disabled={@disabled}>
      {@inner_block}
    </button>
    """
  end

  defp prepare_button_assigns(assigns) do
    # Handle both type and type_input attributes
    type = assigns[:type] || assigns[:type_input] || "button"

    # Get slot content
    slot_content = render_slot_content(assigns[:inner_block], assigns)

    # Map attributes
    attrs = map_button_attributes(assigns)

    assigns
    |> assign(:type, type)
    |> assign(:inner_block, slot_content)
    |> assign_attributes(attrs)
  end

  defp map_button_attributes(assigns) do
    %{
      phx_click: assigns[:"phx-click"] || assigns[:phx_click],
      phx_submit: assigns[:"phx-submit"] || assigns[:phx_submit],
      phx_value: assigns[:"phx-value"] || assigns[:phx_value],
      phx_value_role: assigns[:"phx-value-role"] || assigns[:phx_value_role],
      phx_value_theme: assigns[:"phx-value-theme"] || assigns[:phx_value_theme],
      phx_disable_with: assigns[:"phx-disable-with"] || assigns[:phx_disable_with],
      data_test_id: assigns[:"data-test-id"] || assigns[:data_test_id]
    }
  end

  defp assign_attributes(assigns, attrs) do
    Enum.reduce(attrs, assigns, fn {key, value}, acc ->
      assign(acc, key, value)
    end)
  end

  # Flash component
  attr :flash, :map, default: %{}
  attr :kind, :atom, values: [:info, :error]
  attr :title, :string, default: nil
  attr :id_flash, :string, default: nil
  attr :on_cancel, JS, default: %JS{}
  attr :id, :string, default: nil
  attr :hidden, :boolean, default: false
  attr :phx_connected, :any, default: nil
  attr :phx_disconnected, :any, default: nil
  attr :"phx-connected", :any, default: nil
  attr :"phx-disconnected", :any, default: nil
  slot :inner_block, required: false

  def flash(assigns) do
    assigns = harmonize_slots(assigns)

    # Handle both underscore and hyphenated attribute names
    phx_connected = assigns[:"phx-connected"] || assigns[:phx_connected]
    phx_disconnected = assigns[:"phx-disconnected"] || assigns[:phx_disconnected]

    assigns =
      assigns
      |> assign(:phx_connected, phx_connected)
      |> assign(:phx_disconnected, phx_disconnected)

    ~H"""
    <div
      :if={msg = Phoenix.Flash.get(@flash, @kind)}
      id={@id || @id_flash}
      phx-mounted={show("##{@id || @id_flash}")}
      phx-click={JS.push("lv:clear-flash", value: %{key: @kind}) |> hide("##{@id || @id_flash}")}
      phx-hook="Flash"
      data-cancel={@on_cancel}
      phx-connected={@phx_connected}
      phx-disconnected={@phx_disconnected}
      hidden={@hidden}
      role="alert"
      class={[
        "fixed top-2 right-2 w-80 sm:w-96 z-50 rounded-lg p-3 ring-1 alert-success",
        @kind == :info && "bg-emerald-50 text-emerald-800 ring-emerald-500 fill-cyan-900",
        @kind == :error && "bg-rose-50 text-rose-900 shadow-md ring-rose-500 fill-rose-900"
      ]}
    >
      <p :if={@title} class="font-semibold leading-tight">{@title}</p>
      <p class="mt-2 leading-tight">{msg}</p>
      <button type="button" class="group absolute top-2 right-1 p-2" aria-label="close">
        <.icon_component name="hero-x-mark-solid" class="h-5 w-5 opacity-40 group-hover:opacity-70" />
      </button>
      {render_slot(@inner_block)}
    </div>
    """
  end

  attr :flash_group_id, :string, default: "flash-group"
  attr :flash, :map, default: %{}

  def flash_group(assigns) do
    ~H"""
    <div id={@flash_group_id}>
      <.flash kind={:info} title="Success!" flash={@flash} />
      <.flash kind={:error} title="Error!" flash={@flash} />
      <.flash id="client-error" kind={:error} title="We can't find the internet" phx-disconnected={show(".phx-client-error #client-error")} phx-connected={hide("#client-error")} hidden>
        Attempting to reconnect <.icon_component name="hero-arrow-path" class="ml-1 h-3 w-3 animate-spin" />
      </.flash>

      <.flash id="server-error" kind={:error} title="Something went wrong!" phx-disconnected={show(".phx-server-error #server-error")} phx-connected={hide("#server-error")} hidden>
        Hang in there while we get back on track. <.icon_component name="hero-arrow-path" class="ml-1 h-3 w-3 animate-spin" />
      </.flash>
    </div>
    """
  end

  # JS helper functions
  def show(js \\ %JS{}, selector) when is_binary(selector) do
    JS.show(js, to: selector)
  end

  def hide(js \\ %JS{}, selector) when is_binary(selector) do
    JS.hide(js, to: selector)
  end

  # Modal helper functions
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

  def nav(assigns) do
    IO.puts("DEBUG: nav component called with assigns: #{inspect(Map.keys(assigns))}")
    assigns = harmonize_slots(assigns)
    assigns = if Map.has_key?(assigns, :class), do: assigns, else: Map.put(assigns, :class, "")
    slot_content = get_nav_slot_content(assigns)
    IO.puts("DEBUG: nav slot content: #{inspect(slot_content)}")
    assigns = assign(assigns, :inner_block, slot_content)

    ~H"""
    <nav class={["nav", @class]}>
      {@inner_block}
    </nav>
    """
  end

  defp get_nav_slot_content(assigns) do
    cond do
      is_list(assigns[:item]) ->
        render_list_items(assigns[:item], assigns)

      is_function(assigns[:item]) ->
        render_slot_content(assigns[:item], assigns, :inner_block)

      is_list(assigns[:inner_block]) ->
        render_list_items(assigns[:inner_block], assigns)

      is_function(assigns[:inner_block]) ->
        render_slot_content(assigns[:inner_block], assigns, :inner_block)

      true ->
        render_inner_block_fallback(assigns[:inner_block], assigns)
    end
  end

  defp render_list_items(items, assigns) do
    Enum.map_join(items, "", fn item -> render_list_item(item, assigns) end)
  end

  defp render_inner_block_fallback(inner_block, assigns) do
    case inner_block do
      nil -> ""
      val when is_list(val) -> render_list_items(val, assigns)
      val when is_function(val) -> render_slot_content(val, assigns, :inner_block)
      _ -> ""
    end
  end

  defp render_theme_toggle_slot(assigns) do
    cond do
      is_list(assigns[:inner_block]) ->
        render_list_items(assigns[:inner_block], assigns)

      is_function(assigns[:inner_block]) ->
        render_slot_content(assigns[:inner_block], assigns, :inner_block)

      true ->
        render_inner_block_fallback(assigns[:inner_block], assigns)
    end
  end

  def theme_toggle(assigns) do
    assigns = harmonize_slots(assigns)
    assigns = if Map.has_key?(assigns, :class), do: assigns, else: Map.put(assigns, :class, "")
    slot_content = render_theme_toggle_slot(assigns)
    assigns = assign(assigns, :inner_block, slot_content)

    ~H"""
    <div class={["theme-toggle", @class]}>
      <button type="button" class="theme-toggle-btn">
        <span class="theme-toggle-icon">🌙</span>
      </button>
      {@inner_block}
    </div>
    """
  end

  # Header table component
  attr :title, :string, required: true
  attr :version, :string, default: "1.0.0"
  attr :updated, :string, default: "Today"
  attr :author, :string, default: "Author"
  attr :license, :string, default: "MIT"
  attr :line_height, :string, default: "normal"

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

  def simple_form(assigns) do
    assigns = harmonize_slots(assigns)
    # Add default for :rest to avoid KeyError
    assigns = if Map.has_key?(assigns, :rest), do: assigns, else: Map.put(assigns, :rest, [])
    # Add default class to prevent KeyError
    assigns = if Map.has_key?(assigns, :class), do: assigns, else: Map.put(assigns, :class, "")
    slot_content = render_slot_content(assigns[:inner_block], assigns)
    actions_content = render_slot_content(assigns[:actions], assigns)
    assigns = assign(assigns, :inner_block, slot_content)
    assigns = assign(assigns, :actions, actions_content)

    ~H"""
    <.form :let={_f} for={@for} as={@as} {@rest}>
      {@inner_block}
      <div class="flex justify-end gap-3">
        {@actions}
      </div>
    </.form>
    """
  end

  # Input component
  attr :type_input, :string, default: "text"
  attr :type, :string, default: nil
  attr :name, :string, required: true
  attr :id, :string, required: true
  attr :value, :any, default: nil
  attr :label, :string, default: nil
  attr :errors, :list, default: []
  attr :options, :list, default: []
  attr :rest, :global

  def input(assigns) do
    # Harmonize input type for test compatibility
    input_type =
      cond do
        assigns[:type_input] && assigns[:type_input] != "text" -> assigns[:type_input]
        assigns[:type] in ["select", "textarea", "checkbox"] -> assigns[:type]
        true -> assigns[:type_input] || assigns[:type] || "text"
      end

    assigns = Map.put(assigns, :input_type, input_type)

    ~H"""
    <div class="form-group">
      <.label :if={@label} for={@id}>{@label}</.label>
      <div class="mt-1">
        <%= case @input_type do %>
          <% "textarea" -> %>
            <textarea
              name={@name}
              id={@id}
              class={[
                "block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm",
                @errors != [] && "border-red-300"
              ]}
              {@rest}
            ><%= @value %></textarea>
          <% "select" -> %>
            <select
              name={@name}
              id={@id}
              class={[
                "block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm",
                @errors != [] && "border-red-300"
              ]}
              {@rest}
            >
              <%= for {value, label} <- @options do %>
                <option value={value} selected={@value == value}>{label}</option>
              <% end %>
            </select>
          <% "checkbox" -> %>
            <input
              type="checkbox"
              name={@name}
              id={@id}
              value={@value}
              class={[
                "block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm",
                @errors != [] && "border-red-300"
              ]}
              {@rest}
            />
          <% _ -> %>
            <input
              type={@input_type}
              name={@name}
              id={@id}
              value={@value}
              class={[
                "block w-full rounded-md border-gray-300 shadow-sm focus:border-indigo-500 focus:ring-indigo-500 sm:text-sm",
                @errors != [] && "border-red-300"
              ]}
              {@rest}
            />
        <% end %>
      </div>
      <.error :for={msg <- @errors}>
        {msg}
      </.error>
    </div>
    """
  end
end
