defmodule Spacecast.Events.SMSNotifications do
  @moduledoc """
  SMS notifications module for handling SMS-related events.
  """

  alias Spacecast.Events.Core.EventBus

  def send_sms_notification(phone_number, message) do
    case validate_phone_number(phone_number) do
      :ok ->
        case send_sms(phone_number, message) do
          {:ok, _response} ->
            EventBus.publish("sms_sent", %{phone_number: phone_number})
            {:ok, "SMS sent successfully"}

          {:error, reason} ->
            EventBus.publish("sms_failed", %{phone_number: phone_number, reason: reason})
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp validate_phone_number(phone_number) do
    case Regex.run(~r/^\+?[1-9]\d{1,14}$/, phone_number) do
      [_match] -> :ok
      nil -> {:error, :invalid_phone_number}
    end
  end

  defp send_sms(_phone_number, _message) do
    # TODO: Implement actual SMS sending logic
    # This is a placeholder that simulates SMS sending
    case :rand.uniform(10) do
      1 -> {:error, "Failed to send SMS"}
      _ -> {:ok, %{message_id: Ecto.UUID.generate()}}
    end
  end
end
