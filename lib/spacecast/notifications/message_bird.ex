defmodule Spacecast.Notifications.MessageBird do
  @moduledoc """
  MessageBird notifications module for sending SMS messages.
  """

  alias Spacecast.Events.Core.EventBus

  def send_sms(phone_number, message) do
    case validate_phone_number(phone_number) do
      :ok ->
        case send_sms_message(phone_number, message) do
          {:ok, response} ->
            EventBus.publish("sms_sent", %{phone_number: phone_number})
            {:ok, response}

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

  defp send_sms_message(_phone_number, _message) do
    # TODO: Implement actual MessageBird SMS sending logic
    # This is a placeholder that simulates SMS sending
    case :rand.uniform(10) do
      1 -> {:error, "Failed to send SMS"}
      _ -> {:ok, %{message_id: Ecto.UUID.generate()}}
    end
  end

  def message_create(_invalid_client, _invalid_params), do: {:error, :invalid_parameters}
end
