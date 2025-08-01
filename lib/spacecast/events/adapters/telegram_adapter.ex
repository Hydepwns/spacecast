defmodule Spacecast.Events.Adapters.TelegramAdapter do
  @moduledoc """
  Adapter for sending notifications to Telegram channels and users.
  Supports rich formatting, inline keyboards, and proper error handling.
  """

  @behaviour Spacecast.Events.Adapters.Adapter
  require Logger

  @impl true
  def send_reminder(reminder, _settings, config, encrypted_message) do
    with {:ok, chat_id} <- validate_chat_id(reminder.recipient),
         {:ok, message} <- build_telegram_message(reminder, encrypted_message, config),
         {:ok, _response} <- send_telegram_message(chat_id, message, config) do
      Logger.info("Telegram message sent successfully to chat #{chat_id}")
      {:ok, "Telegram message sent successfully"}
    else
      {:error, :invalid_chat} ->
        Logger.error("Invalid Telegram chat ID: #{reminder.recipient}")
        {:error, "Invalid Telegram chat ID"}

      {:error, reason} ->
        Logger.error("Failed to send Telegram message: #{inspect(reason)}")
        {:error, "Failed to send Telegram message"}
    end
  end

  # Private functions

  defp validate_chat_id(chat_id) do
    # Telegram chat IDs can be positive (users) or negative (groups/channels)
    case Integer.parse(chat_id) do
      {id, ""} -> {:ok, id}
      _ -> {:error, :invalid_chat}
    end
  end

  defp build_telegram_message(reminder, encrypted_message, config) do
    # Build a Telegram message with rich formatting
    message = %{
      text: """
      *#{reminder.title}*

      #{reminder.message}

      Encrypted message: `#{encrypted_message}`
      """,
      parse_mode: "Markdown",
      disable_web_page_preview: true
    }

    # Add inline keyboard if configured
    message =
      case config.inline_keyboard do
        keyboard when is_list(keyboard) ->
          Map.put(message, :reply_markup, %{
            inline_keyboard: keyboard
          })

        _ ->
          message
      end

    {:ok, message}
  end

  defp send_telegram_message(chat_id, message, config) do
    try do
      # Telegram Bot API endpoint
      url = "https://api.telegram.org/bot#{config.bot_token}/sendMessage"

      # Add chat_id to the message
      message = Map.put(message, :chat_id, chat_id)

      # Prepare request body
      body = Jason.encode!(message)

      # Send request
      headers = [
        {"Content-Type", "application/json"}
      ]

      case HTTPoison.post(url, body, headers) do
        {:ok, %HTTPoison.Response{status_code: 200, body: response_body}} ->
          handle_successful_response(response_body)

        {:ok, %HTTPoison.Response{status_code: 401}} ->
          Logger.error("Telegram authentication error")
          {:error, "Telegram authentication failed"}

        {:ok, %HTTPoison.Response{status_code: 429, body: response_body}} ->
          handle_rate_limit(response_body)

        {:ok, %HTTPoison.Response{status_code: status}} ->
          Logger.error("Telegram HTTP error: #{status}")
          {:error, "Telegram service error: HTTP #{status}"}

        {:error, %HTTPoison.Error{reason: reason}} ->
          Logger.error("Telegram request error: #{inspect(reason)}")
          {:error, "Failed to connect to Telegram"}
      end
    rescue
      e ->
        Logger.error("Telegram error: #{inspect(e)}")
        {:error, "Telegram service error"}
    end
  end

  defp handle_successful_response(response_body) do
    case Jason.decode(response_body) do
      {:ok, %{"ok" => true, "result" => %{"message_id" => message_id}}} ->
        {:ok, %{message_id: message_id}}

      {:ok, %{"ok" => false, "description" => description}} ->
        Logger.error("Telegram API error: #{description}")
        {:error, "Failed to send Telegram message: #{description}"}

      _ ->
        Logger.error("Invalid Telegram API response")
        {:error, "Invalid Telegram API response"}
    end
  end

  defp handle_rate_limit(response_body) do
    case Jason.decode(response_body) do
      {:ok, %{"parameters" => %{"retry_after" => retry_after}}} ->
        Logger.warning("Telegram rate limit hit, retry after #{retry_after} seconds")
        Process.sleep(retry_after * 1000)
        {:error, "Rate limited, please retry"}

      {:ok, error} ->
        Logger.error("Telegram rate limit error: #{inspect(error)}")
        {:error, "Telegram rate limit error"}

      {:error, _} ->
        Logger.error("Invalid Telegram rate limit response")
        {:error, "Telegram rate limit error"}
    end
  end
end
