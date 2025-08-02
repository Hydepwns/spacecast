defmodule Spacecast.Events.QueryBuilders.ReplayQuery do
  @moduledoc """
  Builds and manages queries for event replay sessions.
  Handles event constraints and ordering for replay operations.
  """

  import Ecto.Query
  alias Spacecast.Events.Schemas.Event
  alias Spacecast.Events.Schemas.ReplaySession

  @doc """
  Builds a query for events in a replay session.

  ## Parameters
  * `session` - The replay session to build the query for
  """
  @spec build_query(ReplaySession.t()) :: Ecto.Query.t()
  def build_query(session) do
    session
    |> build_base_query()
    |> add_start_event_constraint(session)
    |> add_end_event_constraint(session)
    |> order_by([e], asc: e.inserted_at)
  end

  # Private functions

  defp build_base_query(session) do
    from(e in Event,
      where: e.resource_type == ^session.resource_type and e.resource_id == ^session.resource_id
    )
  end

  defp add_start_event_constraint(query, session) do
    if session.start_event_id do
      case get_event(session.start_event_id) do
        {:ok, start_event} -> from(e in query, where: e.inserted_at >= ^start_event.inserted_at)
        _ -> query
      end
    else
      query
    end
  end

  defp add_end_event_constraint(query, session) do
    if session.end_event_id do
      case get_event(session.end_event_id) do
        {:ok, end_event} -> from(e in query, where: e.inserted_at <= ^end_event.inserted_at)
        _ -> query
      end
    else
      query
    end
  end

  defp get_event(id) do
    case Spacecast.RepoHelper.get(Event, id, timeout: 5000) do
      nil -> {:error, :not_found}
      event -> {:ok, event}
    end
  end
end
