defmodule Spacecast.Events.EventSupervisor do
  @moduledoc """
  Supervisor for the Resource Event System.

  This module delegates to Spacecast.Events.Core.EventSupervisor,
  which is the actual implementation of the event system supervisor.

  This module exists to maintain backward compatibility with code that
  expects the supervisor to be at this module path.
  """

  # use Supervisor # Removed as this module is a delegate, not a supervisor itself.
  require Logger

  @doc """
  Starts the event system supervisor.
  """
  def start_link(init_arg) do
    Spacecast.Events.Core.EventSupervisor.start_link(init_arg)
  end

  @doc """
  Registers standard projections that should be started with the application.
  """
  def register_standard_projections do
    Spacecast.Events.Core.EventSupervisor.register_standard_projections()
  end

  @doc """
  Registers standard handlers that should be started with the application.
  """
  def register_standard_handlers do
    Spacecast.Events.Core.EventSupervisor.register_standard_handlers()
  end
end
