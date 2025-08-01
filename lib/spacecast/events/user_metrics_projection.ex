defmodule Spacecast.Events.UserMetricsProjection do
  @moduledoc """
  Bridge module for the User Metrics Projection.

  This module delegates to Spacecast.Events.Projections.UserMetricsProjection,
  which is the actual implementation of the user metrics projection.

  This module exists to maintain backward compatibility with code that
  expects the user metrics projection to be at this module path.
  """

  alias Spacecast.Events.Projections.UserMetricsProjection, as: CoreUserMetricsProjection

  # Implement the Projection behavior by delegating to the core implementation

  @behaviour Spacecast.Events.Projections.Projection

  @impl true
  def init do
    CoreUserMetricsProjection.init()
  end

  @impl true
  def interested_in do
    CoreUserMetricsProjection.interested_in()
  end

  @impl true
  def apply_event(event, state) do
    CoreUserMetricsProjection.apply_event(event, state)
  end

  @impl true
  def get_state do
    CoreUserMetricsProjection.get_state()
  end

  # Delegate all public functions to the core implementation
  defdelegate total_users(state), to: CoreUserMetricsProjection
  defdelegate total_logins(state), to: CoreUserMetricsProjection
  defdelegate active_users_on_date(state, date), to: CoreUserMetricsProjection
  defdelegate active_users_in_week(state, date), to: CoreUserMetricsProjection
  defdelegate active_users_in_month(state, date), to: CoreUserMetricsProjection
  defdelegate most_active_users(state, limit \\ 10), to: CoreUserMetricsProjection
  defdelegate inactive_users(state, days), to: CoreUserMetricsProjection
end
