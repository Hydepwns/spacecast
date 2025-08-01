defmodule SpacecastWeb.Examples.TransformationExampleLive do
  @moduledoc """
  Example LiveView that demonstrates how to use the transformation system.

  This LiveView provides a comprehensive demonstration of the transformation system,
  including:

  - Registering and using transformations
  - Visualizing transformations and their effects
  - Working with transformation contexts
  - Handling transformation errors
  """

  use SpacecastWeb, :live_view

  alias Spacecast.Utils.LiveViewResource
  alias Spacecast.Transformations.TransformationRegistry
  alias Spacecast.Transformations.TransformationPipeline
  alias Spacecast.Transformations.TransformationContext
  alias Spacecast.Transformations.StandardTransformers

  @adapter_info nil

  @example_resource %{
    username: "JOHN.DOE",
    email: "JOHN.DOE@EXAMPLE.COM",
    phone: "(555) 123-4567",
    age: "30",
    active: "yes",
    bio: "<script>alert('XSS')</script><p>Hello world</p>",
    notes: "  This is a note with extra spaces  ",
    created_at: nil,
    last_login: nil
  }

  @type_schema %{
    username: :string,
    email: :string,
    phone: :string,
    age: :integer,
    active: :boolean,
    bio: :string,
    notes: :string,
    created_at: :datetime,
    last_login: :datetime
  }

  # Define resource using the LiveViewResource behavior
  use LiveViewResource
  import Spacecast.Utils.ResourceAssigns, only: [attributes: 1]

  attributes do
    attribute(:resource, :map, default: @example_resource)
    attribute(:original_resource, :map, default: @example_resource)
    attribute(:transformation_result, :any, default: nil)
    attribute(:transformation_context, :any, default: nil)
    attribute(:form_data, :map, default: %{})
    attribute(:available_transformers, :list, default: [])
    attribute(:selected_transformers, :list, default: [])
    attribute(:execution_error, :string, default: nil)
  end

  @impl Spacecast.Utils.LiveViewResource
  def __resource_schema__ do
    @type_schema
  end

  @impl Spacecast.Utils.LiveViewResource
  def relationships do
    []
  end

  @impl Spacecast.Utils.LiveViewResource
  def validations do
    []
  end

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    case Process.whereis(TransformationRegistry) do
      nil ->
        {:ok, _pid} = TransformationRegistry.start_link()

      _pid ->
        Spacecast.Utils.TransformationRegistry.clear()
    end

    StandardTransformers.register_defaults()

    available_transformers =
      TransformationRegistry.list_transformations()
      |> Enum.map(& &1.name)
      |> Enum.sort()

    selected_transformers = [
      "format_email",
      "format_phone",
      "trim_strings",
      "convert_types",
      "sanitize_html",
      "add_timestamp"
    ]

    socket =
      socket
      |> assign(:available_transformers, available_transformers)
      |> assign(:selected_transformers, selected_transformers)
      |> assign(:form_data, %{
        "conversions" => %{
          "age" => "integer",
          "active" => "boolean"
        },
        "html_fields" => ["bio"],
        "trim_fields" => ["notes", "username"],
        "timestamp_field" => "created_at",
        "field_to_remove" => ""
      })

    socket
  end

  @impl Phoenix.LiveView
  def handle_event("execute_transformations", _params, socket) do
    result =
      execute_transformation_pipeline(
        socket.assigns.resource,
        socket.assigns.selected_transformers,
        socket.assigns.form_data
      )

    case result do
      {:ok, transformed_resource, context} ->
        socket =
          socket
          |> assign(:resource, transformed_resource)
          |> assign(:transformation_result, :success)
          |> assign(:transformation_context, context)
          |> assign(:execution_error, nil)

        {:noreply, socket}

      {:error, resource, context} ->
        socket =
          socket
          |> assign(:resource, resource)
          |> assign(:transformation_result, :error)
          |> assign(:transformation_context, context)
          |> assign(:execution_error, nil)

        {:noreply, socket}

      {:error, message} ->
        socket =
          socket
          |> assign(:transformation_result, :execution_error)
          |> assign(:execution_error, message)

        {:noreply, socket}
    end
  end

  @impl Phoenix.LiveView
  def handle_event("reset_resource", _params, socket) do
    socket =
      socket
      |> assign(:resource, @example_resource)
      |> assign(:original_resource, @example_resource)
      |> assign(:transformation_result, nil)
      |> assign(:transformation_context, nil)
      |> assign(:execution_error, nil)

    {:noreply, socket}
  end

  @impl Phoenix.LiveView
  def handle_event("update_transformers", %{"transformers" => transformers}, socket) do
    selected =
      if is_list(transformers) do
        transformers
      else
        Map.keys(transformers)
      end

    {:noreply, assign(socket, :selected_transformers, selected)}
  end

  @impl Phoenix.LiveView
  def handle_event("update_form", %{"form_data" => form_data}, socket) do
    updated_form_data = Map.merge(socket.assigns.form_data, form_data)
    {:noreply, assign(socket, :form_data, updated_form_data)}
  end

  @impl Phoenix.LiveView
  def handle_event("transform", _params, socket) do
    context = %{
      current_user: socket.assigns.current_user
    }

    {:noreply,
     socket
     |> assign(:transformation_context, context)}
  end

  @impl Phoenix.LiveView
  def handle_event("transform_with_context", _params, socket) do
    context = %{
      current_user: socket.assigns.current_user
    }

    socket
    |> assign(:transformation_context, context)

    {:noreply, socket}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div class="transformation-example">
      <h1 class="page-title">Resource Transformation System Demo</h1>

      <div class="metrics-dashboard-link">
        <.link navigate={~p"/examples/transformation-metrics"} class="button button--primary">
          <span>📊 View Transformation Performance Metrics</span>
        </.link>
      </div>

      <div class="example-container">
        <div class="control-panel">
          <div class="section-header">
            <h2>Transformation Controls</h2>
          </div>

          <div class="transformer-selection">
            <h3>Select Transformers</h3>
            <form phx-change="update_transformers">
              <div class="transformer-checkboxes">
                <%= for transformer <- @available_transformers do %>
                  <label class="transformer-option">
                    <input type="checkbox" name={"transformers[#{transformer}]"} value="true" checked={transformer in @selected_transformers} />
                    <span class="transformer-name">{transformer}</span>
                  </label>
                <% end %>
              </div>
            </form>
          </div>

          <div class="transformer-config">
            <h3>Configure Transformers</h3>
            <form phx-change="update_form">
              <div class="config-section">
                <h4>Type Conversions</h4>
                <div class="field-pair">
                  <label for="age_type">age:</label>
                  <select name="form_data[conversions][age]" id="age_type">
                    <option value="string" selected={@form_data["conversions"]["age"] == "string"}>string</option>
                    <option value="integer" selected={@form_data["conversions"]["age"] == "integer"}>integer</option>
                    <option value="float" selected={@form_data["conversions"]["age"] == "float"}>float</option>
                  </select>
                </div>
                <div class="field-pair">
                  <label for="active_type">active:</label>
                  <select name="form_data[conversions][active]" id="active_type">
                    <option value="string" selected={@form_data["conversions"]["active"] == "string"}>string</option>
                    <option value="boolean" selected={@form_data["conversions"]["active"] == "boolean"}>boolean</option>
                  </select>
                </div>
              </div>

              <div class="config-section">
                <h4>HTML Sanitization</h4>
                <div class="field-checkbox">
                  <label>
                    <input type="checkbox" name="form_data[html_fields][]" value="bio" checked={"bio" in (@form_data["html_fields"] || [])} /> Sanitize bio field
                  </label>
                </div>
              </div>

              <div class="config-section">
                <h4>String Trimming</h4>
                <div class="field-checkbox">
                  <label>
                    <input type="checkbox" name="form_data[trim_fields][]" value="notes" checked={"notes" in (@form_data["trim_fields"] || [])} /> Trim notes field
                  </label>
                </div>
                <div class="field-checkbox">
                  <label>
                    <input type="checkbox" name="form_data[trim_fields][]" value="username" checked={"username" in (@form_data["trim_fields"] || [])} /> Trim username field
                  </label>
                </div>
              </div>

              <div class="config-section">
                <h4>Add Timestamp</h4>
                <div class="field-pair">
                  <label for="timestamp_field">Field:</label>
                  <select name="form_data[timestamp_field]" id="timestamp_field">
                    <option value="created_at" selected={@form_data["timestamp_field"] == "created_at"}>created_at</option>
                    <option value="last_login" selected={@form_data["timestamp_field"] == "last_login"}>last_login</option>
                  </select>
                </div>
              </div>

              <div class="config-section">
                <h4>Remove Field</h4>
                <div class="field-pair">
                  <label for="field_to_remove">Field to remove:</label>
                  <select name="form_data[field_to_remove]" id="field_to_remove">
                    <option value="" selected={@form_data["field_to_remove"] == ""}>None</option>
                    <%= for field <- Map.keys(@resource) do %>
                      <option value={field} selected={@form_data["field_to_remove"] == field}>{field}</option>
                    <% end %>
                  </select>
                </div>
              </div>
            </form>
          </div>

          <div class="action-buttons">
            <button phx-click="execute_transformations" class="primary-button">
              Execute Transformations
            </button>
            <button phx-click="reset_resource" class="secondary-button">
              Reset Resource
            </button>
          </div>
        </div>

        <div class="result-panel">
          <div class="section-header">
            <h2>Transformation Results</h2>
          </div>

          <%= if @execution_error do %>
            <div class="execution-error">
              <div class="error-message">
                <strong>Error executing transformations:</strong> {@execution_error}
              </div>
            </div>
          <% end %>

          <%= if @transformation_result do %>
            <.live_component module={SpacecastWeb.Components.TransformationVisualizerComponent} id="transformation_visualizer" context={@transformation_context} original_resource={@original_resource} transformed_resource={@resource} show_unchanged={false} collapsible={true} />
          <% else %>
            <div class="no-results">
              <p>Execute transformations to see results</p>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end

  # Private functions

  defp execute_transformation_pipeline(resource, selected_transformers, form_data) do
    try do
      register_selected_transformations(selected_transformers, form_data)

      _context =
        TransformationContext.new(
          Map.keys(resource),
          :update,
          original_resource: resource
        )

      case TransformationPipeline.apply_transformations(
             resource,
             :example,
             :update,
             phase: :before_validation,
             original_resource: resource
           ) do
        {:ok, transformed_resource, updated_context} ->
          {:ok, transformed_resource, updated_context}

        {:error, resource, context} ->
          {:error, "Transformation pipeline failed", resource, context}

        other ->
          {:error, "Unexpected result from transformation pipeline: #{inspect(other)}"}
      end
    rescue
      exception ->
        {:error, "Error executing transformation pipeline: #{Exception.message(exception)}"}
    end
  end

  defp register_selected_transformations(selected_transformers, form_data) do
    clear_example_transformations()

    register_email_transformer(selected_transformers)
    register_phone_transformer(selected_transformers, form_data)
    register_trim_transformer(selected_transformers, form_data)
    register_type_converter(selected_transformers, form_data)
    register_html_sanitizer(selected_transformers, form_data)
    register_field_remover(selected_transformers, form_data)
    register_uuid_adder(selected_transformers)
    register_defaults_setter(selected_transformers)
    register_custom_transformer(form_data)
  end

  defp register_email_transformer(selected_transformers) do
    if "format_email" in selected_transformers do
      TransformationRegistry.register(
        "format_email",
        &StandardTransformers.format_email/2,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: 10
      )
    end
  end

  defp register_phone_transformer(selected_transformers, form_data) do
    if "format_phone" in selected_transformers do
      format_option =
        case Map.get(form_data, "phone_format", "digits_only") do
          "formatted" -> :formatted
          _ -> :digits_only
        end

      TransformationRegistry.register(
        "format_phone",
        fn resource, context ->
          StandardTransformers.format_phone(resource, context, format: format_option)
        end,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: 20
      )
    end
  end

  defp register_trim_transformer(selected_transformers, form_data) do
    if "trim_strings" in selected_transformers do
      fields = Map.get(form_data, "trim_fields", [])
      fields_atoms = fields |> Enum.map(&String.to_existing_atom/1)

      TransformationRegistry.register(
        "trim_strings",
        fn resource, context ->
          StandardTransformers.trim_strings(resource, context, fields: fields_atoms)
        end,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: 5
      )
    end
  end

  defp register_type_converter(selected_transformers, form_data) do
    if "convert_types" in selected_transformers do
      type_conversions =
        @type_schema
        |> Enum.filter(fn {key, _type} ->
          Map.get(form_data, "convert_#{key}", false) in [true, "true"]
        end)
        |> Map.new()

      TransformationRegistry.register(
        "convert_types",
        fn resource, context ->
          StandardTransformers.convert_types(resource, context, conversions: type_conversions)
        end,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: 30,
        dependencies: ["format_email", "format_phone", "trim_strings"]
      )
    end
  end

  defp register_html_sanitizer(selected_transformers, form_data) do
    if "sanitize_html" in selected_transformers do
      fields = Map.get(form_data, "html_fields", [])
      fields_atoms = fields |> Enum.map(&String.to_existing_atom/1)

      TransformationRegistry.register(
        "sanitize_html",
        fn resource, context ->
          StandardTransformers.sanitize_html(resource, context, fields: fields_atoms)
        end,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: 40
      )
    end
  end

  defp register_field_remover(selected_transformers, form_data) do
    unless "remove_fields" in selected_transformers, do: :ok

    field_to_remove = Map.get(form_data, "field_to_remove")
    if field_to_remove == "", do: :ok

    fields = [String.to_existing_atom(field_to_remove)]

    TransformationRegistry.register(
      "remove_fields",
      fn resource, context ->
        StandardTransformers.remove_fields(resource, context, fields: fields)
      end,
      resource_type: :example,
      operation: :update,
      phase: :before_validation,
      priority: 90
    )
  end

  defp register_uuid_adder(selected_transformers) do
    if "add_uuid" in selected_transformers do
      TransformationRegistry.register(
        "add_uuid",
        fn resource, context ->
          StandardTransformers.add_uuid(resource, context, field: :uuid)
        end,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: 50
      )
    end
  end

  defp register_defaults_setter(selected_transformers) do
    if "set_defaults" in selected_transformers do
      defaults = %{last_login: DateTime.utc_now()}

      TransformationRegistry.register(
        "set_defaults",
        fn resource, context ->
          StandardTransformers.set_defaults(resource, context, defaults: defaults)
        end,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: 60
      )
    end
  end

  defp register_custom_transformer(form_data) do
    custom_name = Map.get(form_data, "custom_name", "")
    custom_code = Map.get(form_data, "custom_code", "")

    if custom_name != "" && custom_code != "" do
      {custom_transformer, _} = Code.eval_string(custom_code)

      TransformationRegistry.register(
        custom_name,
        custom_transformer,
        resource_type: :example,
        operation: :update,
        phase: :before_validation,
        priority: Map.get(form_data, "custom_priority", 50) |> parse_integer(50)
      )
    end
  end

  defp clear_example_transformations do
    all_transformations = TransformationRegistry.list_transformations()

    example_transformations =
      Enum.filter(all_transformations, fn t ->
        t.resource_type == :example
      end)

    # Unregister each one (this would be a real feature in a full implementation)
    # Since we don't have unregister, we just re-register with empty registry
    if length(example_transformations) > 0 do
      # In a real implementation, you'd have an unregister function
      # We're simulating this by starting from an empty state
      # In a production environment, you'd use a proper unregister mechanism
    end
  end

  # Helper to parse integers safely
  defp parse_integer(value, _default) when is_integer(value), do: value
  defp parse_integer(_value, default), do: default
end
