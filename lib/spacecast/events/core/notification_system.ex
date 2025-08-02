defmodule Spacecast.Events.Core.NotificationSystem do
  @moduledoc """
  Notification system for the event processing pipeline.

  This module handles the delivery of alerts and notifications about the
  event processing system, including:

  - Backpressure alerts
  - Event processing errors
  - Performance degradation notifications
  - System status updates

  It supports multiple notification channels such as in-app notifications,
  email, Slack, and PagerDuty.
  """

  require Logger
  alias SpacecastWeb.Presence

  # Default notification settings
  @default_channels [:in_app, :log]

  @doc """
  Sends an alert to the appropriate channels.

  ## Parameters
  * `alert` - Map containing alert information:
    * `:type` - The alert type (:event_system_backpressure, :event_error, etc.)
    * `:level` - The severity level (:normal, :warning, :critical)
    * `:message` - Human-readable message
    * `:details` - Map with additional information
    * `:timestamp` - When the alert was generated
  * `opts` - Options for alert delivery
    * `:channels` - List of channels to notify (defaults to [:in_app, :log])
    * `:recipients` - List of recipient IDs or nil for admins only

  ## Returns
  * `{:ok, sent_notifications}` - The alert was sent
  * `{:error, reason}` - Failed to send alert
  """
  @spec send_alert(map(), Keyword.t()) :: {:ok, map()} | {:error, any()}
  def send_alert(alert, opts \\ []) do
    channels = Keyword.get(opts, :channels, @default_channels)
    recipients = Keyword.get(opts, :recipients, :admins_only)

    # Ensure alert has ID and timestamp
    alert =
      alert
      |> Map.put_new_lazy(:id, &generate_id/0)
      |> Map.put_new_lazy(:timestamp, &DateTime.utc_now/0)

    # Broadcast to admin dashboard for real-time updates
    Phoenix.PubSub.broadcast(
      Spacecast.PubSub,
      "admin_notifications",
      {:new_notification, alert}
    )

    # Track which notifications were sent
    sent_notifications =
      channels
      |> Enum.map(fn channel -> {channel, deliver_to_channel(channel, alert, recipients)} end)
      |> Enum.filter(fn {_channel, result} -> match?({:ok, _}, result) end)
      |> Enum.map(fn {channel, {:ok, details}} -> {channel, details} end)
      |> Enum.into(%{})

    # Log that we sent notifications
    Logger.info("Sent #{map_size(sent_notifications)} alerts for #{alert.type} (level: #{alert.level})")

    {:ok, sent_notifications}
  end

  @doc """
  Subscribes to alerts for specific event types.

  ## Parameters
  * `user` - The user to subscribe
  * `event_types` - List of event types to subscribe to or :all
  * `level` - Minimum alert level to receive (:normal, :warning, :critical)

  ## Returns
  * `{:ok, subscription}` - Successfully subscribed
  * `{:error, reason}` - Failed to subscribe
  """
  @spec subscribe(map(), [atom()] | :all, atom()) :: {:ok, map()} | {:error, any()}
  def subscribe(user, event_types \\ :all, level \\ :warning) do
    # Implementation would typically store this in a database
    # For now, we'll just return a success
    {:ok, %{user_id: user.id, event_types: event_types, level: level}}
  end

  @doc """
  Unsubscribes a user from alerts.

  ## Parameters
  * `user` - The user to unsubscribe
  * `event_types` - List of event types to unsubscribe from or :all

  ## Returns
  * `:ok` - Successfully unsubscribed
  * `{:error, reason}` - Failed to unsubscribe
  """
  @spec unsubscribe(any(), [atom()] | :all) :: :ok | {:error, any()}
  def unsubscribe(_user, _event_types \\ :all) do
    # Implementation would typically update a database
    :ok
  end

  @doc """
  Sets up default system alerts for administrators.

  ## Parameters
  * `channels` - List of channels to use for system alerts

  ## Returns
  * `:ok` - Successfully set up alerts
  * `{:error, reason}` - Failed to set up alerts
  """
  @spec setup_system_alerts([atom()]) :: :ok | {:error, any()}
  def setup_system_alerts(_channels \\ [:in_app, :log, :email]) do
    # This would be used to configure system-wide alert settings
    :ok
  end

  @doc """
  Retrieves notification preferences for a user.

  ## Parameters
  * `user` - The user to get preferences for

  ## Returns
  * `{:ok, preferences}` - User preferences
  * `{:error, reason}` - Failed to get preferences
  """
  @spec get_preferences(any()) :: {:ok, map()} | {:error, any()}
  def get_preferences(_user) do
    # This would typically be loaded from a database
    # For now, we return default preferences
    {:ok,
     %{
       channels: [:in_app, :email],
       event_types: :all,
       level: :warning
     }}
  end

  @doc """
  Updates notification preferences for a user.

  ## Parameters
  * `user` - The user to update preferences for
  * `preferences` - New preferences

  ## Returns
  * `{:ok, updated_preferences}` - Successfully updated
  * `{:error, reason}` - Failed to update
  """
  @spec update_preferences(any(), map()) :: {:ok, map()} | {:error, any()}
  def update_preferences(_user, preferences) do
    # This would typically save to a database
    {:ok, preferences}
  end

  #############################################################################
  # Private Functions - Channel Delivery
  #############################################################################

  # Deliver alert to the in-app notification system
  defp deliver_to_channel(:in_app, alert, recipients) do
    try do
      # Get list of user IDs to notify
      user_ids = get_user_ids_for_recipients(recipients)

      # Create in-app notification
      notification = %{
        type: alert.type,
        level: alert.level,
        message: alert.message,
        summary: get_summary_for_alert(alert),
        timestamp: alert.timestamp,
        read: false
      }

      # Broadcast to active admin users
      Presence.broadcast_to_users("admin", "new_notification", notification, user_ids)

      # Return success with user IDs notified
      {:ok, %{notified_users: length(user_ids), user_ids: user_ids}}
    rescue
      e ->
        Logger.error("Failed to deliver in-app notification: #{inspect(e)}")
        {:error, :in_app_delivery_failed}
    end
  end

  # Deliver to system logs
  defp deliver_to_channel(:log, alert, _recipients) do
    log_level =
      case alert.level do
        :critical -> :error
        :warning -> :warning
        _ -> :info
      end

    Logger.log(
      log_level,
      "EVENT ALERT: [#{alert.type}] #{alert.message} " <>
        "(level: #{alert.level}, time: #{DateTime.to_string(alert.timestamp)})"
    )

    {:ok, %{logged_at: DateTime.utc_now()}}
  end

  # Deliver to email
  defp deliver_to_channel(:email, alert, recipients) do
    # In a real implementation, this would send emails
    # For now, we'll just log it
    Logger.info("Would send email alert to #{inspect(recipients)}: #{alert.message}")

    {:ok, %{email_queued: true}}
  end

  # Deliver to Slack
  defp deliver_to_channel(:slack, alert, _recipients) do
    # In a real implementation, this would post to Slack
    # For now, we'll just log it
    Logger.info("Would send Slack alert: #{alert.message}")

    {:ok, %{slack_queued: true}}
  end

  # Deliver to PagerDuty (for critical alerts)
  defp deliver_to_channel(:pagerduty, alert, _recipients) do
    if alert.level == :critical do
      # In a real implementation, this would create a PagerDuty incident
      # For now, we'll just log it
      Logger.info("Would create PagerDuty incident: #{alert.message}")

      {:ok, %{incident_created: true}}
    else
      # Don't create PagerDuty incidents for non-critical alerts
      {:ok, %{incident_created: false, reason: "Alert level not critical"}}
    end
  end

  # Default handler for unknown channels
  defp deliver_to_channel(channel, _alert, _recipients) do
    Logger.warning("Attempted to deliver alert to unknown channel: #{inspect(channel)}")
    {:error, :unknown_channel}
  end

  #############################################################################
  # Private Functions - Helpers
  #############################################################################

  # Generate a unique ID for notifications
  defp generate_id do
    :crypto.strong_rand_bytes(10) |> Base.encode16(case: :lower)
  end

  # Get a list of user IDs based on the recipient specification
  defp get_user_ids_for_recipients(:admins_only) do
    # In a real implementation, this would query the database for admin users
    # For now, we'll return a placeholder
    ["admin1", "admin2"]
  end

  defp get_user_ids_for_recipients(:all_users) do
    # In a real implementation, this would query the database for all users
    # For now, we'll return a placeholder
    ["admin1", "admin2", "user1", "user2", "user3"]
  end

  defp get_user_ids_for_recipients(user_ids) when is_list(user_ids) do
    user_ids
  end

  # Generate a human-readable summary for each alert type
  defp get_summary_for_alert(alert) do
    case alert.type do
      :event_system_backpressure ->
        get_backpressure_summary(alert.details)

      :event_error ->
        "Error processing event: #{alert.details.error_message}"

      :system_overload ->
        "System overload: #{alert.details.resource_type || "unknown"} resource"

      _ ->
        alert.message
    end
  end

  # Generate summary for backpressure alerts
  defp get_backpressure_summary(details) do
    backpressure = details.backpressure

    cond do
      backpressure.queue_pressure ->
        "Queue pressure detected: #{render_handlers_with_pressure(details.queue_sizes)}"

      length(backpressure.slow_processing_types) > 0 ->
        "Slow event processing: #{Enum.join(backpressure.slow_processing_types, ", ")}"

      length(backpressure.high_error_types) > 0 ->
        "High error rates in: #{Enum.join(backpressure.high_error_types, ", ")}"

      true ->
        "Event system backpressure detected"
    end
  end

  # Format handlers with pressure for display
  defp render_handlers_with_pressure(queue_sizes) do
    queue_sizes
    |> Enum.filter(fn {_handler, size} -> size > 1000 end)
    |> Enum.map_join(", ", fn {handler, size} -> "#{handler} (#{size})" end)
  end
end
