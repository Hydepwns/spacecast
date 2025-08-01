defmodule SpacecastWeb.Components.Common.DebugHelpers do
  @moduledoc """
  Shared utility functions for debug and validation components.

  This module provides generic helpers for formatting, summarizing, and inspecting values
  in debug/validation UIs. Use these helpers in debug panels, grids, and other components
  to avoid code duplication and ensure consistent behavior.
  """

  # Helper to determine if a value is simple enough to display directly
  def is_simple_value(value) do
    case value do
      v when is_binary(v) -> String.length(v) < 50
      v when is_number(v) -> true
      v when is_atom(v) -> true
      v when is_boolean(v) -> true
      nil -> true
      _ -> false
    end
  end

  # Helper to create a summary for complex values
  def summarize_value(value) do
    cond do
      is_map(value) -> "Map with #{map_size(value)} keys"
      is_list(value) -> "List with #{length(value)} items"
      is_tuple(value) -> "Tuple with #{tuple_size(value)} elements"
      is_function(value) -> "Function"
      is_pid(value) -> "PID"
      true -> "Complex value"
    end
  end

  # Format a complex value for display
  def format_complex_value(value) do
    inspect(value, pretty: true, width: 60)
  end

  # Helper to get type of a value (human readable)
  def get_type(nil), do: "nil"
  def get_type(value) when is_binary(value), do: "string"
  def get_type(value) when is_integer(value), do: "integer"
  def get_type(value) when is_float(value), do: "float"
  def get_type(value) when is_boolean(value), do: "boolean"
  def get_type(value) when is_atom(value), do: "atom"
  def get_type(value) when is_map(value), do: "map"
  def get_type(value) when is_list(value), do: "list"
  def get_type(value) when is_tuple(value), do: "tuple"
  def get_type(value) when is_function(value), do: "function"
  def get_type(value) when is_pid(value), do: "pid"
  def get_type(_), do: "unknown"

  # Helper to create a preview of a value
  def truncate_preview(nil), do: "nil"

  def truncate_preview(value) when is_binary(value) do
    if String.length(value) <= 50 do
      value
    else
      String.slice(value, 0, 47) <> "..."
    end
  end

  def truncate_preview(value) when is_map(value) do
    "Map with #{map_size(value)} keys"
  end

  def truncate_preview(value) when is_list(value) do
    "List with #{length(value)} items"
  end

  def truncate_preview(value) when is_tuple(value) do
    "Tuple with #{tuple_size(value)} elements"
  end

  def truncate_preview(value) when is_function(value) do
    "Function"
  end

  def truncate_preview(value) when is_pid(value) do
    "PID"
  end

  def truncate_preview(value) do
    inspect(value, pretty: true, width: 50)
  end
end
