defmodule SpacecastWeb.Components.UI.FormComponents do
  @moduledoc """
  Form-related components and utilities.
  """
  use Phoenix.Component
  use Gettext, backend: SpacecastWeb.Gettext
  import Phoenix.HTML.Form

  alias Phoenix.LiveView.JS

  # Common attributes
  attr :id, :string, required: true
  attr :show, :boolean, default: false
  attr :on_cancel, JS, default: %JS{}
  attr :type, :string, default: nil
  attr :class, :string, default: nil
  attr :rest, :global
  slot :inner_block, required: true

  # Flash-related attributes
  attr :flash, :map, default: %{}, doc: "the map of flash messages to display"
  attr :title, :string, default: nil
  attr :kind, :atom, values: [:info, :error], doc: "used for styling and flash lookup"
  attr :id_flash, :string, doc: "the optional id of flash container"
  attr :flash_group_id, :string, default: "flash-group", doc: "the optional id of flash container"

  # Form-related attributes
  attr :for, :any, required: true, doc: "the data structure for the form"
  attr :as, :any, default: nil, doc: "the server side parameter to collect all input under"
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

  attr :field, Phoenix.HTML.FormField,
    doc: "a form field struct retrieved from the form, for example: @form[:email]"

  # Slots
  slot :actions, doc: "the slot for form actions, such as a submit button"

  def simple_form(assigns) do
    ~H"""
    <.form :let={f} for={@for} as={@as} id={@id} class={@class}>
      {render_slot(@inner_block, f)}
    </.form>
    """
  end

  def input(%{field: %Phoenix.HTML.FormField{} = field} = assigns) do
    errors = field.errors

    # Filter out non-attribute values from rest
    rest = assigns[:rest] || %{}

    rest =
      if is_map(rest) do
        # Only keep valid HTML attribute keys, including LiveView attributes
        rest
        |> Map.drop([:value, :field, :form, :content, :metadata, :settings])
        |> Map.filter(fn {k, v} ->
          is_atom(k) or
            (is_binary(k) and
               (is_binary(v) or is_number(v) or is_boolean(v) or is_nil(v) or
                  (is_atom(k) and Atom.to_string(k) |> String.starts_with?("phx-"))))
        end)
      else
        %{}
      end

    # Add all phx-* attributes from assigns to rest
    rest =
      assigns
      |> Enum.filter(fn {k, _v} ->
        k = if is_atom(k), do: Atom.to_string(k), else: k
        String.starts_with?(k, "phx-")
      end)
      |> Enum.reduce(rest, fn {k, v}, acc -> Map.put(acc, k, v) end)

    # Ensure required attribute is included if specified
    rest = if assigns[:required], do: Map.put(rest, :required, true), else: rest

    assigns
    |> assign(field: nil, id: assigns[:id] || field.id)
    |> assign(:errors, Enum.map(errors, &translate_error(&1)))
    |> assign_new(:name, fn -> if assigns[:multiple], do: field.name <> "[]", else: field.name end)
    |> assign_new(:value, fn -> field.value end)
    |> assign_new(:type_input, fn -> assigns[:type] || "text" end)
    |> assign(:rest, rest)
    |> assign_new(:label, fn -> nil end)
    |> assign_new(:multiple, fn -> assigns[:multiple] || false end)
    |> assign_new(:prompt, fn -> assigns[:prompt] end)
    |> assign_new(:required, fn -> false end)
    |> input()
  end

  def input(%{type: "checkbox"} = assigns) do
    assigns =
      assign_new(assigns, :checked, fn ->
        input_value(assigns[:form], assigns[:name])
      end)

    ~H"""
    <div>
      <label class="flex items-center gap-4 text-sm leading-6 text-zinc-600">
        <input type="hidden" name={@name} value="false" />
        <input type="checkbox" id={@id} name={@name} value="true" checked={@checked} class="rounded border-zinc-300 text-zinc-900 focus:ring-0" /> {@label}
      </label>
      <.error :for={msg <- @errors}>{msg}</.error>
    </div>
    """
  end

  def input(%{type: "select"} = assigns) do
    assigns =
      assigns
      |> assign_new(:multiple, fn -> assigns[:multiple] || false end)
      |> assign_new(:prompt, fn -> assigns[:prompt] end)

    ~H"""
    <div data-test-id={"#{@id}-container"}>
      <.label_tag for={@id}>{@label}</.label_tag>
      <select id={@id} name={@name} class="mt-2 block w-full rounded-md border border-gray-300 bg-white shadow-sm focus:border-zinc-400 focus:ring-0 sm:text-sm" multiple={@multiple} data-test-id={@id}>
        <option :if={@prompt} value="">{@prompt}</option>
        {options_for_select(@options, @value)}
      </select>
      <.error :for={msg <- @errors} data-test-id={"#{@id}-error"}>{msg}</.error>
    </div>
    """
  end

  def input(%{type: "textarea"} = assigns) do
    value =
      cond do
        assigns[:form] && is_map(input_value(assigns[:form], assigns[:name])) ->
          Jason.encode!(input_value(assigns[:form], assigns[:name]))

        assigns[:form] ->
          input_value(assigns[:form], assigns[:name])

        is_map(assigns[:value]) ->
          Jason.encode!(assigns[:value])

        true ->
          assigns[:value] || ""
      end

    assigns = assign(assigns, :textarea_value, value)

    ~H"""
    <div data-test-id={"#{@id}-container"}>
      <.label_tag for={@id}>{@label}</.label_tag>
      <textarea
        id={@id}
        name={@name}
        data-test-id={@id}
        class={[
          "mt-2 block w-full rounded-lg text-zinc-900 focus:ring-0 sm:text-sm sm:leading-6 min-h-[6rem]",
          @errors == [] && "border-zinc-300 focus:border-zinc-400",
          @errors != [] && "border-rose-400 focus:border-rose-400"
        ]}
      ><%= @textarea_value %></textarea>
      <.error :for={msg <- @errors} data-test-id={"#{@id}-error"}>{msg}</.error>
    </div>
    """
  end

  def input(assigns) do
    assigns = assign_new(assigns, :required, fn -> false end)

    ~H"""
    <div class="form-group">
      <.label_tag for={@id}>{@label}</.label_tag>
      <input type={@type_input} id={@id} name={@name} value={@value} class={["form-control", @errors != [] && "is-invalid", @errors != [] && "error"]} required={@rest[:required] || @required} {@rest} />
      <.error :for={msg <- @errors} class="error">{msg}</.error>
    </div>
    """
  end

  def label_tag(assigns) do
    ~H"""
    <label for={@for} class="block text-sm font-semibold leading-6 text-zinc-800" data-test-id={"#{@for}-label"}>
      {render_slot(@inner_block)}
    </label>
    """
  end

  def label(assigns) do
    ~H"""
    <label>{assigns[:for] || "Label"}</label>
    """
  end

  attr :rest, :global
  slot :inner_block, required: true

  def error(assigns) do
    ~H"""
    <p class="error mt-3 flex gap-3 text-sm leading-6 text-rose-600" {@rest}>
      <.icon name="hero-exclamation-circle-mini" class="mt-0.5 h-5 w-5 flex-none" /> {render_slot(@inner_block)}
    </p>
    """
  end

  def icon(%{name: "hero-" <> _} = assigns) do
    ~H"""
    <span class={[@name, @class]} />
    """
  end

  def icon(assigns) do
    ~H"""
    <span class={[@name, @class]} />
    """
  end

  # Button-specific attributes
  attr :type, :string, default: "button"
  attr :class, :string, default: nil
  attr :rest, :global
  slot :inner_block, required: true

  def button(assigns) do
    ~H"""
    <button
      type={@type}
      class={[
        "phx-submit-loading:opacity-75 rounded-lg bg-zinc-900 hover:bg-zinc-700 py-2 px-3 text-sm font-semibold leading-6 text-white active:text-white/80",
        @class
      ]}
    >
      {render_slot(@inner_block)}
    </button>
    """
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
end
