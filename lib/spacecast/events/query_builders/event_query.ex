defmodule Spacecast.Events.QueryBuilders.EventQuery do
  @moduledoc """
  Builds and manages queries for event retrieval.
  Handles filtering, sorting, and pagination of events.
  """

  import Ecto.Query
  alias Spacecast.Events.Schemas.Event

  @doc """
  Builds a query from the given criteria.

  ## Parameters
  * `criteria` - Map of criteria to filter events by. Supported keys:
    * `:id` - The event ID
    * `:correlation_id` - Events with this correlation ID
    * `:causation_id` - Events with this causation ID
    * `:event_type` - Events of this type (or list of types)
    * `:resource_id` - Events for this resource ID
    * `:timestamp` - Events with timestamp matching this criteria
    * `:metadata` - Events with metadata matching this criteria
    * `:limit` - Maximum number of events to return
    * `:offset` - Number of events to skip
    * `:sort` - List of sort criteria, e.g. [timestamp: :desc]
  """
  @spec build_query(map()) :: Ecto.Query.t()
  def build_query(criteria \\ %{}) do
    base_query = from(e in Event)

    criteria
    |> Enum.reduce(base_query, &apply_criterion/2)
    |> apply_sort(criteria)
    |> apply_pagination(criteria)
  end

  # Private functions

  defp apply_criterion({:id, id}, query) do
    where(query, [e], e.id == ^id)
  end

  defp apply_criterion({:correlation_id, correlation_id}, query) do
    where(query, [e], e.correlation_id == ^correlation_id)
  end

  defp apply_criterion({:causation_id, causation_id}, query) do
    where(query, [e], e.causation_id == ^causation_id)
  end

  defp apply_criterion({:event_type, event_types}, query) when is_list(event_types) do
    where(query, [e], e.type in ^event_types)
  end

  defp apply_criterion({:event_type, event_type}, query) do
    where(query, [e], e.type == ^event_type)
  end

  defp apply_criterion({:resource_id, resource_id}, query) do
    where(query, [e], e.resource_id == ^resource_id)
  end

  defp apply_criterion({:timestamp, timestamp_criteria}, query) when is_map(timestamp_criteria) do
    Enum.reduce(timestamp_criteria, query, fn
      {:after, time}, q -> where(q, [e], e.timestamp >= ^time)
      {:before, time}, q -> where(q, [e], e.timestamp <= ^time)
      _, q -> q
    end)
  end

  defp apply_criterion({:metadata, metadata_criteria}, query) when is_map(metadata_criteria) do
    Enum.reduce(metadata_criteria, query, fn {key, value}, q ->
      where(q, [e], fragment("?->? = ?", e.metadata, ^to_string(key), ^to_string(value)))
    end)
  end

  defp apply_criterion(_, query), do: query

  defp apply_sort(query, %{sort: sort_criteria}) when is_list(sort_criteria) do
    Enum.reduce(sort_criteria, query, fn
      {:timestamp, :asc}, q -> order_by(q, [e], asc: e.timestamp)
      {:timestamp, :desc}, q -> order_by(q, [e], desc: e.timestamp)
      {:id, :asc}, q -> order_by(q, [e], asc: e.id)
      {:id, :desc}, q -> order_by(q, [e], desc: e.id)
      _, q -> q
    end)
  end

  defp apply_sort(query, _), do: query

  defp apply_pagination(query, %{limit: limit}) when is_integer(limit) and limit > 0 do
    query = limit(query, ^limit)

    case Map.get(query, :offset) do
      offset when is_integer(offset) and offset >= 0 ->
        offset(query, ^offset)

      _ ->
        query
    end
  end

  defp apply_pagination(query, %{offset: offset}) when is_integer(offset) and offset >= 0 do
    offset(query, ^offset)
  end

  defp apply_pagination(query, _), do: query
end
