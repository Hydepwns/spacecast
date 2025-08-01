defmodule Spacecast.Events.ProjectionProcess do
  @moduledoc """
  Bridge module for the Projection Process.

  This module delegates to Spacecast.Events.Projections.ProjectionProcess,
  which is the actual implementation of the projection process.

  This module exists to maintain backward compatibility with code that
  expects the projection process to be at this module path.
  """

  alias Spacecast.Events.Projections.ProjectionProcess, as: CoreProjectionProcess

  defdelegate start_link(projection_module, opts \\ []), to: CoreProjectionProcess
  defdelegate child_spec(opts), to: CoreProjectionProcess

  # Add any other functions that might be called on ProjectionProcess
  # For example:
  # defdelegate some_function(args), to: CoreProjectionProcess
end
