defmodule Spacecast.Events.Projections.UserMetricsProjection do
  @moduledoc """
  Projection that maintains user activity metrics.

  This projection tracks:
  - Total number of users
  - Login counts
  - Active users by day/week/month
  - Most active users
  """

  @behaviour Spacecast.Events.Projections.Projection

  require Logger

  # Projection behavior implementation

  @impl true
  def init do
    state = %{
      total_users: 0,
      login_count: 0,
      user_logins: %{},
      active_users: %{
        daily: %{},
        weekly: %{},
        monthly: %{}
      },
      user_last_seen: %{},
      user_first_seen: %{}
    }

    {:ok, state}
  end

  @impl true
  def interested_in do
    [
      "user.created",
      "user.login",
      "user.logout",
      "user.deleted"
    ]
  end

  @impl true
  def apply_event(%{type: "user.created"} = event, state) do
    user_id = event.resource_id

    # Update state with new user
    new_state =
      state
      |> Map.update!(:total_users, &(&1 + 1))
      |> put_in([:user_logins, user_id], 0)
      |> put_in([:user_first_seen, user_id], event.timestamp)
      |> put_in([:user_last_seen, user_id], event.timestamp)

    {:ok, new_state}
  end

  @impl true
  def apply_event(%{type: "user.login"} = event, state) do
    user_id = event.resource_id
    date = DateTime.to_date(event.timestamp)

    # If this is a new user (not seen before), add them to our tracking
    state =
      if not Map.has_key?(state.user_logins, user_id) do
        state
        |> Map.update!(:total_users, &(&1 + 1))
        |> put_in([:user_logins, user_id], 0)
        |> put_in([:user_first_seen, user_id], event.timestamp)
      else
        state
      end

    # Update login count and last seen
    new_state =
      state
      |> Map.update!(:login_count, &(&1 + 1))
      |> update_in([:user_logins, user_id], &((&1 || 0) + 1))
      |> put_in([:user_last_seen, user_id], event.timestamp)
      |> update_active_users(date, user_id)

    {:ok, new_state}
  end

  @impl true
  def apply_event(%{type: "user.logout"} = event, state) do
    user_id = event.resource_id

    # Update last seen timestamp
    new_state =
      if Map.has_key?(state.user_last_seen, user_id) do
        put_in(state, [:user_last_seen, user_id], event.timestamp)
      else
        state
      end

    {:ok, new_state}
  end

  @impl true
  def apply_event(%{type: "user.deleted"} = event, state) do
    user_id = event.resource_id

    # We don't actually remove the user from our metrics,
    # just mark them as deleted by setting login count to -1
    new_state =
      if Map.has_key?(state.user_logins, user_id) do
        put_in(state, [:user_logins, user_id], -1)
      else
        state
      end

    {:ok, new_state}
  end

  @impl true
  def apply_event(_event, state) do
    # Ignore other events
    {:ok, state}
  end

  # Helper functions

  # Updates active users for daily, weekly, and monthly periods
  defp update_active_users(state, date, user_id) do
    # Convert date to string keys
    day_key = Date.to_string(date)

    # Get the start of the week (Monday)
    week_start = Date.beginning_of_week(date)
    week_key = Date.to_string(week_start)

    # Get the start of the month
    month_start = %{date | day: 1}
    month_key = Date.to_string(month_start)

    # Update active users for each time period
    state
    |> update_active_users_for_period([:active_users, :daily], day_key, user_id)
    |> update_active_users_for_period([:active_users, :weekly], week_key, user_id)
    |> update_active_users_for_period([:active_users, :monthly], month_key, user_id)
  end

  # Updates active users for a specific time period
  defp update_active_users_for_period(state, path, period_key, user_id) do
    # Get the current set of active users for this period
    active_users = get_in(state, path ++ [period_key]) || MapSet.new()

    # Add the user to the set
    updated_active_users = MapSet.put(active_users, user_id)

    # Update the state
    put_in(state, path ++ [period_key], updated_active_users)
  end

  # Public API for querying metrics

  @doc """
  Gets the total number of users.
  """
  def total_users(state) do
    state.total_users
  end

  @doc """
  Gets the total number of logins.
  """
  def total_logins(state) do
    state.login_count
  end

  @doc """
  Gets the number of active users for a specific date.
  """
  def active_users_on_date(state, date) do
    date_key = Date.to_string(date)

    case get_in(state, [:active_users, :daily, date_key]) do
      nil -> 0
      users -> MapSet.size(users)
    end
  end

  @doc """
  Gets the number of active users for a specific week.
  """
  def active_users_in_week(state, date) do
    week_start = Date.beginning_of_week(date)
    week_key = Date.to_string(week_start)

    case get_in(state, [:active_users, :weekly, week_key]) do
      nil -> 0
      users -> MapSet.size(users)
    end
  end

  @doc """
  Gets the number of active users for a specific month.
  """
  def active_users_in_month(state, date) do
    month_start = %{date | day: 1}
    month_key = Date.to_string(month_start)

    case get_in(state, [:active_users, :monthly, month_key]) do
      nil -> 0
      users -> MapSet.size(users)
    end
  end

  @doc """
  Gets the most active users, sorted by login count.
  """
  def most_active_users(state, limit \\ 10) do
    state.user_logins
    |> Enum.filter(fn {_user_id, count} -> count > 0 end)
    |> Enum.sort_by(fn {_user_id, count} -> count end, :desc)
    |> Enum.take(limit)
  end

  @doc """
  Gets users who haven't logged in for a specified period.
  """
  def inactive_users(state, days) do
    cutoff = DateTime.utc_now() |> DateTime.add(-days * 24 * 60 * 60, :second)

    state.user_last_seen
    |> Enum.filter(fn {user_id, last_seen} ->
      DateTime.compare(last_seen, cutoff) == :lt &&
        Map.get(state.user_logins, user_id, 0) >= 0
    end)
    |> Enum.map(fn {user_id, _} -> user_id end)
  end

  @doc """
  Returns the current state of the projection. Not implemented for stateless module.
  """
  @impl true
  def get_state, do: :not_implemented
end
