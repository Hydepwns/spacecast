defmodule Spacecast.Notifications.Twilio do
  @moduledoc """
  Twilio SMS service implementation for sending SMS notifications.

  This module provides SMS functionality through the Twilio API.
  """

  @behaviour Spacecast.Notifications.TwilioBehaviour

  require Logger

  @doc """
  Sends an SMS message to the specified phone number.

  ## Parameters
  - to: Phone number of the recipient (in E.164 format)
  - message: Text message content

  ## Returns
  - {:ok, message_sid} on success
  - {:error, reason} on failure
  """
  @impl true
  def send_sms(to, message) do
    case validate_sms_params(to, message) do
      :ok ->
        # In production, this would make an actual API call to Twilio
        message_sid = generate_message_sid()
        Logger.info("SMS sent to #{to}: #{message} (SID: #{message_sid})")
        {:ok, message_sid}

      {:error, reason} ->
        Logger.error("Failed to send SMS to #{to}: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Sends an SMS message using a predefined template.

  ## Parameters
  - to: Phone number of the recipient (in E.164 format)
  - template_name: Name of the template to use
  - template_data: Data to inject into the template

  ## Returns
  - {:ok, message_sid} on success
  - {:error, reason} on failure
  """
  @impl true
  def send_sms_with_template(to, template_name, template_data) do
    case render_template(template_name, template_data) do
      {:ok, message} ->
        send_sms(to, message)

      {:error, reason} ->
        Logger.error("Failed to render SMS template #{template_name}: #{reason}")
        {:error, reason}
    end
  end

  # Private functions

  defp validate_sms_params(to, message) when is_binary(to) and is_binary(message) do
    cond do
      String.length(message) > 1600 ->
        {:error, "Message too long (max 1600 characters)"}

      !valid_phone_number?(to) ->
        {:error, "Invalid phone number format"}

      true ->
        :ok
    end
  end

  defp validate_sms_params(_to, _message) do
    {:error, "Invalid parameters: to and message must be strings"}
  end

  defp valid_phone_number?(phone) do
    # Basic E.164 format validation
    String.match?(phone, ~r/^\+[1-9]\d{1,14}$/)
  end

  defp render_template(template_name, template_data) do
    # In production, this would use a proper templating system
    case template_name do
      "welcome" ->
        {:ok, "Welcome to Spacecast! Your account has been created successfully."}

      "verification" ->
        code = Map.get(template_data, "code", "123456")
        {:ok, "Your verification code is: #{code}"}

      "reminder" ->
        event_name = Map.get(template_data, "event_name", "Event")
        {:ok, "Reminder: #{event_name} is starting soon!"}

      _ ->
        {:error, "Unknown template: #{template_name}"}
    end
  end

  defp generate_message_sid do
    # Generate a mock Twilio message SID
    ("SM" <> :crypto.strong_rand_bytes(32)) |> Base.encode16(case: :lower)
  end
end
