defmodule Spacecast.Events.Adapters.SMSAdapter do
  @moduledoc """
  Adapter for sending SMS notifications.
  Supports multiple SMS providers and proper error handling.
  """

  @behaviour Spacecast.Events.Adapters.Adapter
  require Logger

  @impl true
  def send_reminder(reminder, _settings, config, encrypted_message) do
    with {:ok, phone_number} <- validate_phone_number(reminder.recipient),
         {:ok, message} <- build_sms_message(reminder, encrypted_message, config),
         {:ok, _response} <- send_sms_message(phone_number, message, config) do
      Logger.info("SMS sent successfully to #{phone_number}")
      {:ok, "SMS sent successfully"}
    else
      {:error, :invalid_phone} ->
        Logger.error("Invalid phone number: #{reminder.recipient}")
        {:error, "Invalid phone number"}

      {:error, reason} ->
        Logger.error("Failed to send SMS: #{inspect(reason)}")
        {:error, "Failed to send SMS"}
    end
  end

  # Private functions

  defp validate_phone_number(phone_number) do
    # Basic phone number validation (E.164 format)
    case Regex.match?(~r/^\+[1-9]\d{1,14}$/, phone_number) do
      true -> {:ok, phone_number}
      false -> {:error, :invalid_phone}
    end
  end

  defp build_sms_message(reminder, encrypted_message, config) do
    # Build SMS message with optional prefix
    prefix = config.message_prefix || ""
    message = "#{prefix}#{reminder.message}\n\nEncrypted: #{encrypted_message}"

    # Truncate if too long
    message =
      if String.length(message) > 160 do
        String.slice(message, 0, 157) <> "..."
      else
        message
      end

    {:ok, message}
  end

  defp send_sms_message(phone_number, message, config) do
    case config.provider do
      "nexmo" -> send_nexmo_sms(phone_number, message, config)
      "twilio" -> send_twilio_sms(phone_number, message, config.account_sid, config.auth_token)
      _ -> {:error, "Unsupported SMS provider"}
    end
  end

  defp send_nexmo_sms(phone_number, message, config) do
    try do
      # Nexmo API endpoint
      url = "https://rest.nexmo.com/sms/json"

      # Prepare request body
      body =
        Jason.encode!(%{
          api_key: config.api_key,
          api_secret: config.api_secret,
          to: phone_number,
          from: config.from_number,
          text: message
        })

      # Send request
      case HTTPoison.post(url, body, [{"Content-Type", "application/json"}]) do
        {:ok, %HTTPoison.Response{status_code: 200, body: response_body}} ->
          case Jason.decode(response_body) do
            {:ok, %{"messages" => [%{"message-id" => message_id} | _]}} ->
              {:ok, %{message_id: message_id}}

            {:ok, %{"messages" => [%{"error-text" => error} | _]}} ->
              Logger.error("Nexmo API error: #{error}")
              {:error, "Failed to send SMS via Nexmo: #{error}"}

            _ ->
              Logger.error("Unexpected Nexmo response: #{response_body}")
              {:error, "Unexpected response from Nexmo"}
          end

        {:ok, %HTTPoison.Response{status_code: status}} ->
          Logger.error("Nexmo HTTP error: #{status}")
          {:error, "Nexmo service error: HTTP #{status}"}

        {:error, %HTTPoison.Error{reason: reason}} ->
          Logger.error("Nexmo request error: #{inspect(reason)}")
          {:error, "Failed to connect to Nexmo"}
      end
    rescue
      e ->
        Logger.error("Nexmo error: #{inspect(e)}")
        {:error, "Nexmo service error"}
    end
  end

  defp send_twilio_sms(to, message, account_sid, auth_token) do
    url = "https://api.twilio.com/2010-04-01/Accounts/#{account_sid}/Messages.json"
    auth = Base.encode64("#{account_sid}:#{auth_token}")

    headers = [
      {"Authorization", "Basic #{auth}"},
      {"Content-Type", "application/x-www-form-urlencoded"}
    ]

    body =
      URI.encode_query(%{
        To: to,
        # Replace with your Twilio phone number
        From: "+1234567890",
        Body: message
      })

    case HTTPoison.post(url, body, headers) do
      {:ok, %{status_code: status_code}} when status_code in 200..299 ->
        {:ok, "SMS sent successfully"}

      {:ok, %{status_code: status_code}} ->
        {:error, "Failed to send SMS with status code: #{status_code}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Failed to send SMS: #{reason}"}
    end
  end
end
