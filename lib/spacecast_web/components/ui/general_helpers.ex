defmodule SpacecastWeb.Components.UI.GeneralHelpers do
  @moduledoc """
  General helper functions for UI components.
  """

  import Phoenix.Component
  # import Phoenix.LiveView.Helpers

  @doc """
  Displays a field value with proper formatting.
  """
  def display_field_value(field, value) do
    case field do
      :datetime -> SpacecastWeb.Components.UI.FormatHelpers.format_datetime(value)
      :date -> SpacecastWeb.Components.UI.FormatHelpers.format_datetime(value)
      :time -> SpacecastWeb.Components.UI.FormatHelpers.format_time(value)
      :percentage -> SpacecastWeb.Components.UI.FormatHelpers.format_percentage(value)
      :size -> SpacecastWeb.Components.UI.FormatHelpers.format_size_change(value)
      :status -> SpacecastWeb.Components.UI.FormatHelpers.format_status(value)
      _ -> SpacecastWeb.Components.UI.FormatHelpers.format_value(value)
    end
  end

  @doc """
  Ensures a resource module exists and is loaded.
  """
  def ensure_resource_module(module_name) do
    case Code.ensure_loaded(module_name) do
      {:module, _} -> {:ok, module_name}
      {:error, :nofile} -> {:error, "Module #{module_name} does not exist"}
    end
  end

  @doc """
  Counts the number of errors in a list.
  """
  def error_count(errors) when is_list(errors) do
    Enum.count(errors)
  end

  def error_count(_), do: 0

  @doc """
  Returns the appropriate icon for an error level.
  """
  def error_icon(level) do
    case level do
      :critical -> "✗"
      :warning -> "!"
      :info -> "i"
      "success" -> "✓"
      "error" -> "✗"
      "warning" -> "!"
      "info" -> "i"
      _ -> "•"
    end
  end

  @doc """
  Gets the error ID from an error struct.
  """
  def get_error_id(error) do
    case error do
      %{id: id} -> id
      %{error_id: id} -> id
      _ -> nil
    end
  end

  @doc """
  Gets the error message from an error struct.
  """
  def get_error_message(error) do
    case error do
      %{message: message} -> message
      %{error: error} -> get_error_message(error)
      error when is_binary(error) -> error
      _ -> "Unknown error"
    end
  end

  @doc """
  Gets the error module from an error struct.
  """
  def get_error_module(error) do
    case error do
      %{module: module} -> module
      %{error_module: module} -> module
      _ -> nil
    end
  end

  @doc false
  def get_transformation_name(transformation) do
    case transformation do
      %{name: name} -> name
      %{transformation_name: name} -> name
      _ -> "Unknown transformation"
    end
  end

  @doc """
  Finds the most affected view from a list of errors.
  """
  def most_affected_view(errors) do
    errors
    |> Enum.group_by(&get_error_module/1)
    |> Enum.max_by(fn {_module, module_errors} -> length(module_errors) end, fn -> {nil, []} end)
    |> elem(0)
  end

  @doc """
  Finds the most common error type from a list of errors.
  """
  def most_common_error_type(errors) do
    errors
    |> Enum.group_by(&get_error_type/1)
    |> Enum.max_by(fn {_type, type_errors} -> length(type_errors) end, fn -> {nil, []} end)
    |> elem(0)
  end

  @doc """
  Gets the relationship name from a relationship definition.
  """
  def relationship_name_from_def(def) do
    case def do
      %{name: name} -> name
      %{relationship_name: name} -> name
      _ -> "Unknown relationship"
    end
  end

  @doc """
  Renders errors in a formatted way.
  """
  def render_errors(assigns) do
    ~H"""
    <div class={"error-container #{@level}"}>
      <h3 class="error-title">{@title}</h3>
      <ul class="error-list">
        <%= for error <- @errors do %>
          <li class="error-item">{error}</li>
        <% end %>
      </ul>
    </div>
    """
  end

  @doc """
  Renders errors with specific parameters.
  """
  def render_errors(errors, level, title, content) do
    """
    <div class="error-container #{level}">
      <h3 class="error-title">#{title}</h3>
      <div class="error-content">#{content}</div>
      <ul class="error-list">
        #{Enum.map_join(errors, "", fn error -> "<li class=\"error-item\">#{get_error_message(error)}</li>" end)}
      </ul>
    </div>
    """
  end

  @doc """
  Builds a filter component.
  """
  def build_filter(assigns) do
    ~H"""
    <div class="filter-container">
      <div class="filter-header">
        <h4 class="filter-title">{@title}</h4>
      </div>
      <div class="filter-content">
        <%= for filter <- @filters do %>
          <div class="filter-item">
            <label class="filter-label">{filter.label}</label>
            <input type="text" class="filter-input" placeholder={filter.placeholder} />
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  @doc """
  Creates a filter map with field, operator, and value.
  """
  def build_filter(field, operator, value) do
    %{field: field, operator: operator, value: value}
  end

  @doc """
  Renders validation status.
  """
  def render_validation_status(assigns) do
    ~H"""
    <div class="validation-status">
      <%= for status <- @validation.statuses do %>
        <div class="status-item">
          <h4 class="status-title">{status.title}</h4>
          <div class="status-content">
            {status.content}
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @doc """
  Renders validation status with specific parameters.
  """
  def render_validation_status(valid, message, type) do
    bg_class = if valid, do: "bg-green-50", else: "bg-red-50"
    text_class = if valid, do: "text-green-800", else: "text-red-800"

    """
    <div class="#{bg_class} border border-gray-200 rounded-md p-4">
      <div class="flex">
        <div class="flex-shrink-0">
          <span class="#{text_class} text-sm font-medium">
            #{type}: #{message}
          </span>
        </div>
      </div>
    </div>
    """
  end

  @doc """
  Returns a shortened version of a view name.
  """
  def short_view_name(name) do
    name
    |> to_string()
    |> String.split(".")
    |> List.last()
    |> String.replace("View", "")
  end

  @doc """
  Returns the appropriate icon for a status.
  """
  def status_icon(status) do
    case status do
      "success" -> "✓"
      "error" -> "✗"
      "warning" -> "!"
      "info" -> "i"
      _ -> "•"
    end
  end

  @doc """
  Truncates a message to a specified length.
  """
  def truncate_message(message, length) when is_binary(message) do
    if String.length(message) > length do
      String.trim_trailing(String.slice(message, 0, length)) <> "..."
    else
      message
    end
  end

  def truncate_message(message, _), do: message

  @doc """
  Returns a list of unique error types from a list of errors.
  """
  def unique_error_types(errors) do
    errors
    |> Enum.map(&get_error_type/1)
    |> Enum.uniq()
  end

  @doc """
  Unsets default status for all items except the given ID.
  """
  def unset_other_defaults(items, except_id) do
    Enum.map(items, fn item ->
      if item.id == except_id do
        Map.put(item, :is_default, true)
      else
        Map.put(item, :is_default, false)
      end
    end)
  end

  # Private helper functions

  defp get_error_type(error) do
    case error do
      %{type: type} -> type
      %{error_type: type} -> type
      _ -> :unknown
    end
  end
end
