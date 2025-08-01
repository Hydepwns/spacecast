defmodule Spacecast.Events.ProjectionSupervisor do
  @moduledoc """
  Supervisor for event projections.

  This module uses DynamicSupervisor to manage event projection processes.
  It provides functions to start and stop projections dynamically, and ensures
  they are properly supervised.
  """

  use DynamicSupervisor
  require Logger

  alias Spacecast.Events.Projections.ProjectionSupervisor, as: CoreProjectionSupervisor

  @doc """
  Starts the projection supervisor.

  ## Parameters
  * `opts` - Supervisor options

  ## Returns
  * `{:ok, pid}` - The supervisor was started
  * `{:error, reason}` - The supervisor failed to start
  """
  def start_link(opts \\ []) do
    CoreProjectionSupervisor.start_link(opts)
  end

  @doc """
  Starts a new event projection.

  ## Parameters
  * `projection_module` - The module implementing the projection logic
  * `opts` - Additional options for the projection
    * `:name` - Optional name to register the projection process under
    * `:rebuild` - Whether to rebuild the projection from scratch (default: false)
    * `:subscribe` - Whether to subscribe to events (default: true)

  ## Returns
  * `{:ok, pid}` - The projection was started
  * `{:error, reason}` - The projection failed to start
  """
  def start_projection(projection_module, opts \\ []) do
    CoreProjectionSupervisor.start_projection(projection_module, opts)
  end

  @doc """
  Stops an event projection.

  ## Parameters
  * `pid` - The process ID of the projection to stop

  ## Returns
  * `:ok` - The projection was stopped
  * `{:error, reason}` - The projection failed to stop
  """
  def stop_projection(pid) do
    CoreProjectionSupervisor.stop_projection(pid)
  end

  @doc """
  Lists all running projections.

  ## Returns
  * `{:ok, projections}` - List of {module, pid} tuples
  """
  def list_projections do
    CoreProjectionSupervisor.list_projections()
  end

  @doc """
  Gets the state of a projection.

  ## Parameters
  * `pid` - The process ID of the projection

  ## Returns
  * `{:ok, state}` - The current state of the projection
  * `{:error, reason}` - Failed to get the projection state
  """
  def get_projection_state(pid) do
    CoreProjectionSupervisor.get_projection_state(pid)
  end

  @doc """
  Rebuilds a projection from scratch.

  ## Parameters
  * `pid` - The process ID of the projection to rebuild

  ## Returns
  * `{:ok, new_state}` - The projection was rebuilt successfully
  * `{:error, reason}` - Failed to rebuild the projection
  """
  def rebuild_projection(pid) do
    CoreProjectionSupervisor.rebuild_projection(pid)
  end

  @doc """
  Registers standard projections for the event system.
  """
  def register_standard_projections do
    CoreProjectionSupervisor.register_standard_projections()
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      max_restarts: 10,
      max_seconds: 60
    )
  end
end
