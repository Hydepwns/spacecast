defmodule SpacecastWeb.Components.UI.FormatHelpers do
  @moduledoc """
  Helper functions for formatting various types of data.
  """

  @doc """
  Formats a change with a label.
  """
  def format_change(label, change) do
    "#{label}: #{format_value(change)}"
  end

  @doc """
  Formats a datetime value.
  """
  def format_datetime(datetime) when is_binary(datetime) do
    case DateTime.from_iso8601(datetime) do
      {:ok, datetime, _} -> format_datetime(datetime)
      _ -> datetime
    end
  end

  def format_datetime(%DateTime{} = datetime) do
    Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
  end

  def format_datetime(%NaiveDateTime{} = datetime) do
    Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
  end

  def format_datetime(_), do: "N/A"

  @doc """
  Formats an error count with a label.
  """
  def format_error_count(label, count) do
    "#{label}: #{count}"
  end

  @doc """
  Formats error details.
  """
  def format_error_details(error) do
    case error do
      %{message: message} -> message
      %{error: error} -> error
      error when is_binary(error) -> error
      _ -> "Unknown error"
    end
  end

  @doc """
  Formats an error message with a label.
  """
  def format_error_message(label, message) do
    "#{label}: #{message}"
  end

  @doc """
  Formats an error type.
  """
  def format_error_type(type) do
    type
    |> to_string()
    |> String.split(".")
    |> List.last()
    |> String.replace("_", " ")
    |> String.capitalize()
  end

  @doc """
  Formats an error.
  """
  def format_error(error) do
    case error do
      %{type: type, message: message} -> "#{format_error_type(type)}: #{message}"
      %{error: error} -> format_error(error)
      error when is_binary(error) -> error
      _ -> "Unknown error"
    end
  end

  @doc """
  Formats nested changes.
  """
  def format_nested_changes(label, changes) do
    "#{label}: #{format_value(changes)}"
  end

  @doc """
  Formats an operation.
  """
  def format_operation(operation) do
    operation
    |> to_string()
    |> String.replace("_", " ")
    |> String.capitalize()
  end

  @doc false
  def format_percentage(value) when is_number(value) do
    "#{round(value * 100)}%"
  end

  @doc false
  def format_percentage(_), do: "N/A"

  @doc false
  def format_rate(value) when is_number(value) do
    "#{Float.round(value, 2)}/s"
  end

  @doc false
  def format_rate(_), do: "N/A"

  @doc false
  def format_resource_type(type) do
    type
    |> to_string()
    |> String.split(".")
    |> List.last()
    |> String.replace("_", " ")
    |> String.capitalize()
  end

  @doc false
  def format_resource(resource) do
    case resource do
      %{name: name} -> name
      %{id: id} -> "Resource #{id}"
      _ -> "Unknown resource"
    end
  end

  @doc false
  def format_size_change(change) when is_number(change) do
    cond do
      change > 0 -> format_bytes(change)
      change < 0 -> "-" <> format_bytes(abs(change))
      true -> "0 B"
    end
  end

  @doc false
  def format_size_change(_), do: "N/A"

  @doc false
  def format_status(status) do
    status
    |> to_string()
    |> String.replace("_", " ")
    |> String.capitalize()
  end

  @doc false
  def format_time(time) when is_integer(time) do
    "#{time}ms"
  end

  @doc false
  def format_time(%DateTime{} = datetime) do
    Calendar.strftime(datetime, "%H:%M")
  end

  @doc false
  def format_time(%NaiveDateTime{} = datetime) do
    Calendar.strftime(datetime, "%H:%M")
  end

  @doc false
  def format_time(_), do: "N/A"

  @doc false
  def format_timestamp_short(timestamp) do
    case DateTime.from_iso8601(timestamp) do
      {:ok, datetime, _} -> Calendar.strftime(datetime, "%H:%M:%S")
      _ -> timestamp
    end
  end

  @doc false
  def format_timestamp(timestamp) do
    case DateTime.from_iso8601(timestamp) do
      {:ok, datetime, _} -> Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
      _ -> timestamp
    end
  end

  @doc """
  Formats a value based on its type.
  """
  def format_value(value) do
    case value do
      value when is_binary(value) -> value
      value when is_number(value) -> to_string(value)
      value when is_boolean(value) -> to_string(value)
      value when is_map(value) -> Jason.encode!(value)
      value when is_list(value) -> Enum.map_join(value, ", ", &format_value/1)
      _ -> "N/A"
    end
  end

  # Private helper functions

  @doc false
  def format_bytes(bytes) when is_number(bytes) and bytes < 1024, do: "#{bytes} B"

  def format_bytes(bytes) when is_number(bytes) and bytes < 1024 * 1024,
    do: "#{round(bytes / 1024)} KB"

  def format_bytes(bytes) when is_number(bytes) and bytes < 1024 * 1024 * 1024,
    do: "#{round(bytes / 1024 / 1024)} MB"

  def format_bytes(bytes) when is_number(bytes), do: "#{round(bytes / 1024 / 1024 / 1024)} GB"
  def format_bytes(_), do: "N/A"
end
