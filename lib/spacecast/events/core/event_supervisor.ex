defmodule Spacecast.Events.Core.EventSupervisor do
  @moduledoc """
  Top-level supervisor for the Resource Event System.

  This supervisor manages all components of the event system:
  - EventBus: Distributes events to subscribers
  - EventStore: Persists events to the database
  - HandlerSupervisor: Manages event handlers
  - ProjectionSupervisor: Manages projections
  """

  use Supervisor
  require Logger

  alias Spacecast.Events.Core.EventBus
  alias Spacecast.Events.Core.EventStore
  alias Spacecast.Events.ProjectionSupervisor
  alias Spacecast.Events.Handlers.HandlerSupervisor
  alias Spacecast.Events.Handlers.StandardHandlers

  @doc """
  Starts the event system supervisor.
  """
  @spec start_link(any()) :: Supervisor.on_start()
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    children = [
      # Registries for handler and projection processes
      {Registry, keys: :unique, name: Spacecast.Events.HandlerRegistry},
      {Registry, keys: :unique, name: Spacecast.Events.ProjectionRegistry},
      # Event bus for distributing events
      {EventBus, []},
      # Event store for persisting events
      {EventStore, []},
      # Projection supervisor for managing projections
      {ProjectionSupervisor, []},
      # Handler supervisor for managing event handlers
      {HandlerSupervisor, []}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  @doc """
  Registers standard projections that should be started with the application.

  This is called after the application has started to ensure all dependencies
  are available.
  """
  @spec register_standard_projections() :: :ok | :error
  def register_standard_projections do
    if function_exported?(ProjectionSupervisor, :register_standard_projections, 0) do
      ProjectionSupervisor.register_standard_projections()
    else
      Logger.warning("ProjectionSupervisor does not export register_standard_projections/0")
      :error
    end
  end

  @doc """
  Registers standard handlers that should be started with the application.

  This is called after the application has started to ensure all dependencies
  are available.
  """
  @spec register_standard_handlers() :: :ok | :error
  def register_standard_handlers do
    if function_exported?(StandardHandlers, :register_handlers, 0) do
      StandardHandlers.register_handlers()
    else
      Logger.warning("StandardHandlers does not export register_handlers/0")
      :error
    end
  end
end
