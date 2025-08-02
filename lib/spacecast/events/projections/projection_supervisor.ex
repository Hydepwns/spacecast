defmodule Spacecast.Events.Projections.ProjectionSupervisor do
  @moduledoc """
  Supervisor for managing projection processes.

  The ProjectionSupervisor is responsible for:
  - Starting and supervising projection processes
  - Registering standard projections
  - Providing an API for interacting with projections
  """

  use Supervisor
  require Logger

  alias Spacecast.Events.ProjectionProcess
  alias Spacecast.Events.UserMetricsProjection

  @doc """
  Starts the projection supervisor.
  """
  def start_link(opts \\ []) do
    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      # Dynamic supervisor for projection processes
      {DynamicSupervisor, strategy: :one_for_one, name: __MODULE__.DynamicSupervisor}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Registers standard projections that should be started with the application.
  """
  def register_standard_projections do
    Logger.info("Registering standard projections")

    # Register the UserMetricsProjection
    {:ok, _pid} = start_projection(UserMetricsProjection)

    :ok
  end

  @doc """
  Starts a projection process for the given projection module.

  ## Options

  * `:name` - Optional name to register the projection process under
  * `:rebuild` - Whether to rebuild the projection from scratch (default: false)
  * `:subscribe` - Whether to subscribe to events (default: true)
  """
  def start_projection(projection_module, opts \\ []) do
    _name = Keyword.get(opts, :name)

    child_spec = %{
      id: {ProjectionProcess, projection_module},
      start: {ProjectionProcess, :start_link, [projection_module, opts]},
      restart: :permanent,
      shutdown: 5000,
      type: :worker
    }

    case DynamicSupervisor.start_child(__MODULE__.DynamicSupervisor, child_spec) do
      {:ok, pid} = result ->
        Logger.info("Started projection #{inspect(projection_module)} with PID #{inspect(pid)}")
        result

      {:error, {:already_started, pid}} ->
        Logger.info("Projection #{inspect(projection_module)} already started with PID #{inspect(pid)}")

        {:ok, pid}

      {:error, reason} = error ->
        Logger.error("Failed to start projection #{inspect(projection_module)}: #{inspect(reason)}")

        error
    end
  end

  @doc """
  Lists all active projections.

  Returns a list of {module, pid} tuples.
  """
  def list_projections do
    projections =
      DynamicSupervisor.which_children(__MODULE__.DynamicSupervisor)
      |> Enum.map(fn {_, pid, _, _} ->
        {:ok, {module, _}} = Spacecast.Events.Projections.ProjectionProcess.get_info(pid)
        {module, pid}
      end)

    {:ok, projections}
  end

  @doc """
  Gets the current state of a projection.
  """
  def get_projection_state(pid) do
    Spacecast.Events.Projections.ProjectionProcess.get_state(pid)
  end

  @doc """
  Rebuilds a projection from scratch.
  """
  def rebuild_projection(pid) do
    Spacecast.Events.Projections.ProjectionProcess.rebuild(pid)
  end

  @doc """
  Stops a projection process.
  """
  def stop_projection(pid) do
    DynamicSupervisor.terminate_child(__MODULE__.DynamicSupervisor, pid)
  end
end
