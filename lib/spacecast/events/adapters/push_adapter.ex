defmodule Spacecast.Events.Adapters.PushAdapter do
  @moduledoc """
  Push notification adapter for sending push notifications.

  This module provides functionality for sending push notifications
  to mobile devices and web browsers.
  """

  @behaviour Spacecast.Events.Adapters.PushAdapterBehaviour

  require Logger

  @doc """
  Sends a push notification to a device or user.

  ## Parameters
  - device_token: Token of the target device
  - notification: Notification data to send

  ## Returns
  - {:ok, notification_id} on success
  - {:error, reason} on failure
  """
  def send_notification(device_token, notification)
      when is_binary(device_token) and is_map(notification) do
    case validate_notification_params(device_token, notification) do
      :ok ->
        # In production, this would send to FCM, APNS, or similar service
        notification_id = generate_notification_id()

        _push_data = %{
          id: notification_id,
          device_token: device_token,
          notification: notification,
          sent_at: DateTime.utc_now(),
          status: "sent"
        }

        Logger.info("Push notification sent to #{device_token}: #{notification_id}")
        {:ok, notification_id}

      {:error, reason} ->
        Logger.error("Failed to send push notification: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Sends a push notification to multiple devices.

  ## Parameters
  - device_tokens: List of device tokens
  - notification: Notification data to send

  ## Returns
  - {:ok, results} on success
  - {:error, reason} on failure
  """
  def send_bulk_notification(device_tokens, notification)
      when is_list(device_tokens) and is_map(notification) do
    case validate_bulk_notification_params(device_tokens, notification) do
      :ok ->
        # In production, this would batch send to push service
        results =
          Enum.map(device_tokens, fn token ->
            case send_notification(token, notification) do
              {:ok, notification_id} ->
                {token, {:ok, notification_id}}

              {:error, reason} ->
                {token, {:error, reason}}
            end
          end)

        success_count = Enum.count(results, fn {_token, {status, _}} -> status == :ok end)

        Logger.info("Bulk notification sent: #{success_count}/#{length(device_tokens)} successful")

        {:ok, results}

      {:error, reason} ->
        Logger.error("Failed to send bulk notification: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Sends a push notification to a topic or channel.

  ## Parameters
  - topic: Topic or channel name
  - notification: Notification data to send

  ## Returns
  - {:ok, notification_id} on success
  - {:error, reason} on failure
  """
  def send_topic_notification(topic, notification)
      when is_binary(topic) and is_map(notification) do
    case validate_topic_notification_params(topic, notification) do
      :ok ->
        # In production, this would send to topic subscribers
        notification_id = generate_notification_id()

        _topic_data = %{
          id: notification_id,
          topic: topic,
          notification: notification,
          sent_at: DateTime.utc_now(),
          status: "sent"
        }

        Logger.info("Topic notification sent to #{topic}: #{notification_id}")
        {:ok, notification_id}

      {:error, reason} ->
        Logger.error("Failed to send topic notification: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Schedules a push notification for later delivery.

  ## Parameters
  - device_token: Token of the target device
  - notification: Notification data to send
  - scheduled_time: When to send the notification

  ## Returns
  - {:ok, scheduled_id} on success
  - {:error, reason} on failure
  """
  def schedule_notification(device_token, notification, scheduled_time)
      when is_struct(scheduled_time, DateTime) do
    case validate_scheduled_notification_params(device_token, notification, scheduled_time) do
      :ok ->
        # In production, this would schedule with a job queue
        scheduled_id = generate_scheduled_id()

        _scheduled_data = %{
          id: scheduled_id,
          device_token: device_token,
          notification: notification,
          scheduled_time: scheduled_time,
          status: "scheduled"
        }

        Logger.info("Notification scheduled for #{device_token} at #{scheduled_time}: #{scheduled_id}")

        {:ok, scheduled_id}

      {:error, reason} ->
        Logger.error("Failed to schedule notification: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Cancels a scheduled push notification.

  ## Parameters
  - scheduled_id: ID of the scheduled notification

  ## Returns
  - {:ok, scheduled_id} on success
  - {:error, reason} on failure
  """
  def cancel_scheduled_notification(scheduled_id) when is_binary(scheduled_id) do
    # In production, this would cancel the scheduled job
    Logger.info("Scheduled notification cancelled: #{scheduled_id}")
    {:ok, scheduled_id}
  end

  @doc """
  Gets delivery status of a push notification.

  ## Parameters
  - notification_id: ID of the notification

  ## Returns
  - {:ok, status} on success
  - {:error, reason} on failure
  """
  def get_notification_status(notification_id) when is_binary(notification_id) do
    # In production, this would check with the push service
    status = %{
      id: notification_id,
      status: "delivered",
      delivered_at: DateTime.utc_now(),
      delivery_attempts: 1
    }

    {:ok, status}
  end

  # Private functions

  defp validate_notification_params(device_token, notification) do
    cond do
      !is_binary(device_token) or String.length(device_token) == 0 ->
        {:error, "Invalid device token"}

      !is_map(notification) or map_size(notification) == 0 ->
        {:error, "Invalid notification data"}

      !Map.has_key?(notification, :title) ->
        {:error, "Notification missing title"}

      !Map.has_key?(notification, :body) ->
        {:error, "Notification missing body"}

      true ->
        :ok
    end
  end

  defp validate_bulk_notification_params(device_tokens, notification) do
    cond do
      !is_list(device_tokens) or length(device_tokens) == 0 ->
        {:error, "Invalid device tokens list"}

      !Enum.all?(device_tokens, &is_binary/1) ->
        {:error, "All device tokens must be strings"}

      !is_map(notification) or map_size(notification) == 0 ->
        {:error, "Invalid notification data"}

      true ->
        :ok
    end
  end

  defp validate_topic_notification_params(topic, notification) do
    cond do
      !is_binary(topic) or String.length(topic) == 0 ->
        {:error, "Invalid topic"}

      !is_map(notification) or map_size(notification) == 0 ->
        {:error, "Invalid notification data"}

      true ->
        :ok
    end
  end

  defp validate_scheduled_notification_params(device_token, notification, scheduled_time) do
    cond do
      !is_binary(device_token) or String.length(device_token) == 0 ->
        {:error, "Invalid device token"}

      !is_map(notification) or map_size(notification) == 0 ->
        {:error, "Invalid notification data"}

      !is_struct(scheduled_time, DateTime) ->
        {:error, "Invalid scheduled time"}

      DateTime.compare(scheduled_time, DateTime.utc_now()) == :lt ->
        {:error, "Scheduled time must be in the future"}

      true ->
        :ok
    end
  end

  defp generate_notification_id do
    ("push_" <> :crypto.strong_rand_bytes(16)) |> Base.encode16(case: :lower)
  end

  defp generate_scheduled_id do
    ("sched_" <> :crypto.strong_rand_bytes(16)) |> Base.encode16(case: :lower)
  end
end
