defmodule Spacecast.Utils.MapHelpers do
  @moduledoc """
  Utility functions for working with maps, including recursive unwrapping and key stringification.
  """

  @doc """
  Recursively unwraps nested 'data' keys in a map.
  """
  def unwrap_data(%{"data" => data} = map) when is_map(data),
    do: unwrap_data(Map.merge(data, Map.drop(map, ["data"])))

  def unwrap_data(map), do: map

  @doc """
  Recursively stringifies all keys in a map.
  """
  def stringify_keys(map) when is_map(map) do
    map
    |> Enum.map(fn {k, v} ->
      key = if is_atom(k), do: Atom.to_string(k), else: k
      value = stringify_keys(v)
      {key, value}
    end)
    |> Enum.into(%{})
  end

  def stringify_keys(list) when is_list(list) do
    Enum.map(list, &stringify_keys/1)
  end

  def stringify_keys(other), do: other
end
