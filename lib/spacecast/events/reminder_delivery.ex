defmodule Spacecast.Events.ReminderDelivery do
  @moduledoc """
  Handles the delivery of event reminders through various channels.
  """

  alias Spacecast.Events
  alias Spacecast.Events.EventReminder
  alias Spacecast.Events.DeliveryService
  alias Spacecast.Repo

  @doc """
  Sends a reminder and updates its status.
  Returns {:ok, reminder} on success or {:error, reason} on failure.
  """
  def send_reminder(reminder) do
    case reminder.status do
      "pending" -> do_send_reminder(reminder)
      _ -> {:error, "Reminder is not in pending status"}
    end
  end

  defp do_send_reminder(reminder) do
    Repo.transaction(fn -> process_reminder_delivery(reminder) end)
    |> handle_transaction_result()
  end

  defp handle_transaction_result({:ok, {:ok, reminder}}), do: {:ok, reminder}
  defp handle_transaction_result({:ok, result}), do: {:ok, result}
  defp handle_transaction_result({:error, reason}), do: {:error, reason}

  defp process_reminder_delivery(reminder) do
    reminder.event_id
    |> get_event_settings()
    |> handle_settings_result(reminder)
  end

  defp handle_settings_result({:ok, settings}, reminder) do
    reminder
    |> deliver_reminder(settings)
    |> handle_delivery_result(reminder)
  end

  defp handle_settings_result({:error, reason}, reminder) do
    update_and_rollback(reminder, "failed", reason)
  end

  defp handle_delivery_result({:ok, _}, reminder) do
    update_reminder_status(reminder, "sent")
  end

  defp handle_delivery_result({:error, reason}, reminder) do
    update_and_rollback(reminder, "failed", reason)
  end

  defp update_and_rollback(reminder, status, reason) do
    case update_reminder_status(reminder, status, reason) do
      {:ok, updated_reminder} ->
        Repo.rollback(reason)
        updated_reminder

      {:error, update_error} ->
        Repo.rollback(update_error)
    end
  end

  defp get_event_settings(event_id) do
    case Events.get_event_settings_by_event_id(event_id) do
      nil -> {:error, "No event settings found"}
      settings -> {:ok, settings}
    end
  end

  defp deliver_reminder(reminder, settings) do
    # Determine delivery type from recipient (email vs sms)
    delivery_type =
      if String.contains?(reminder.recipient, "@") do
        :email
      else
        :sms
      end

    # Send the reminder using the delivery service
    case DeliveryService.send_reminder(reminder, delivery_type, settings) do
      {:ok, _} = result -> result
      {:error, reason} -> {:error, "Failed to send reminder: #{reason}"}
    end
  end

  defp update_reminder_status(reminder, status, error_message \\ nil) do
    reminder
    |> EventReminder.changeset(%{
      status: status,
      sent_at: DateTime.utc_now(),
      error_message: error_message
    })
    |> Repo.update()
  end
end
