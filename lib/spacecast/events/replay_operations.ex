defmodule Spacecast.Events.ReplayOperations do
  @moduledoc """
  Handles operations related to event replay sessions.
  """

  import Ecto.Query, warn: false
  require Logger
  alias Spacecast.Repo
  alias Spacecast.Events.Schemas.ReplaySession
  alias Spacecast.Events.Core.Event

  @doc """
  Creates a new replay session.

  ## Parameters
  * `name` - The name of the replay session
  * `resource_type` - The type of resource to replay
  * `resource_id` - The ID of the resource to replay
  * `opts` - Additional options for the replay session

  ## Returns
  * `{:ok, session}` - The replay session was created
  * `{:error, reason}` - The session could not be created
  """
  @spec create_session(String.t(), String.t(), String.t(), Keyword.t()) ::
          {:ok, ReplaySession.t()} | {:error, any()}
  def create_session(name, resource_type, resource_id, opts \\ []) do
    %ReplaySession{}
    |> ReplaySession.changeset(%{
      name: name,
      _resource_type: resource_type,
      _resource_id: resource_id,
      status: "pending",
      _metadata: opts
    })
    |> Repo.insert()
  end

  @doc """
  Gets a replay session by ID.

  ## Parameters
  * `id` - The ID of the replay session

  ## Returns
  * `{:ok, session}` - The replay session was found
  * `{:error, :not_found}` - The session was not found
  """
  @spec get_session(String.t()) :: {:ok, ReplaySession.t()} | {:error, :not_found}
  def get_session(id) when is_binary(id) do
    case Spacecast.RepoHelper.get(ReplaySession, id) do
      nil -> {:error, :not_found}
      session -> {:ok, session}
    end
  end

  @doc """
  Completes a replay session by marking it as finished and storing the final state.

  ## Parameters
  * `session_id` - The ID of the replay session
  * `final_state` - The final state after replaying all events

  ## Returns
  * `{:ok, session}` - The replay session was completed successfully
  * `{:error, reason}` - The session could not be completed
  """
  @spec complete_session(String.t(), map()) :: {:ok, ReplaySession.t()} | {:error, any()}
  def complete_session(session_id, final_state)
      when is_binary(session_id) and is_map(final_state) do
    with {:ok, session} <- get_session(session_id) do
      session
      |> ReplaySession.changeset(%{
        status: "completed",
        final_state: final_state,
        completed_at: DateTime.utc_now()
      })
      |> Repo.update()
    end
  end

  @doc """
  Gets all events for a replay session.

  ## Parameters
  * `session_id` - The ID of the replay session

  ## Returns
  * `{:ok, events}` - The events for the session
  * `{:error, :not_found}` - The session was not found
  * `{:error, reason}` - Error retrieving the events
  """
  @spec get_session_events(String.t()) :: {:ok, [Event.t()]} | {:error, any()}
  def get_session_events(session_id) when is_binary(session_id) do
    with {:ok, session} <- get_session(session_id) do
      events =
        Event
        |> where([e], e.resource_type == ^session.resource_type)
        |> where([e], e.resource_id == ^session.resource_id)
        |> order_by([e], asc: e.timestamp)
        |> Repo.all()

      {:ok, events}
    end
  end

  @doc """
  Lists all replay sessions.

  ## Returns
  * `{:ok, sessions}` - List of all replay sessions
  * `{:error, reason}` - Error retrieving sessions
  """
  @spec list_sessions() :: {:ok, [ReplaySession.t()]} | {:error, any()}
  def list_sessions do
    try do
      {:ok, Repo.all(ReplaySession)}
    rescue
      e ->
        Logger.error("Error listing replay sessions: #{inspect(e)}")
        {:error, e}
    end
  end
end
