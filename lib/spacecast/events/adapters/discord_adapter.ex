defmodule Spacecast.Events.Adapters.DiscordAdapter do
  @moduledoc """
  Adapter for sending notifications to Discord channels.
  Supports rich embeds, mentions, and proper error handling.
  """

  @behaviour Spacecast.Events.Adapters.Adapter
  require Logger

  @impl true
  def send_reminder(reminder, _settings, config, encrypted_message) do
    with {:ok, channel_id} <- validate_channel_id(reminder.recipient),
         {:ok, message} <- build_discord_message(reminder, encrypted_message, config),
         {:ok, _response} <- send_discord_message(channel_id, message, config) do
      Logger.info("Discord message sent successfully to channel #{channel_id}")
      {:ok, "Discord message sent successfully"}
    else
      {:error, :invalid_channel} ->
        Logger.error("Invalid Discord channel ID: #{reminder.recipient}")
        {:error, "Invalid Discord channel ID"}

      {:error, reason} ->
        Logger.error("Failed to send Discord message: #{inspect(reason)}")
        {:error, "Failed to send Discord message"}
    end
  end

  # Private functions

  defp validate_channel_id(channel_id) do
    # Discord channel IDs are 17-19 digit numbers
    case Regex.match?(~r/^\d{17,19}$/, channel_id) do
      true -> {:ok, channel_id}
      false -> {:error, :invalid_channel}
    end
  end

  defp build_discord_message(reminder, encrypted_message, config) do
    # Build a Discord message with rich embed
    message = %{
      content: reminder.message,
      embed: %{
        title: reminder.title,
        description: "Encrypted message: #{encrypted_message}",
        color: config.embed_color || 0x3498DB,
        timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
        footer: %{
          text: "Spacecast"
        }
      }
    }

    # Add mentions if configured
    message =
      case config.mentions do
        mentions when is_list(mentions) ->
          content = Enum.map_join(mentions, " ", &"<@#{&1}>")
          Map.put(message, :content, "#{content}\n#{message.content}")

        _ ->
          message
      end

    {:ok, message}
  end

  defp send_discord_message(channel_id, message, config) do
    try do
      # Discord API endpoint
      url = "https://discord.com/api/v10/channels/#{channel_id}/messages"

      # Prepare request body and headers
      body = Jason.encode!(message)
      headers = build_headers(config)

      # Make the request and handle the response
      HTTPoison.post(url, body, headers)
      |> handle_discord_response()
    rescue
      e ->
        Logger.error("Discord error: #{inspect(e)}")
        {:error, "Discord service error"}
    end
  end

  defp build_headers(config) do
    [
      {"Content-Type", "application/json"},
      {"Authorization", "Bot #{config.bot_token}"},
      {"User-Agent", "Spacecast (https://github.com/your-repo, v1.0)"}
    ]
  end

  defp handle_discord_response({:ok, %HTTPoison.Response{status_code: 200, body: body}}) do
    parse_success_response(body)
  end

  defp handle_discord_response({:ok, %HTTPoison.Response{status_code: 401}}) do
    Logger.error("Discord authentication error")
    {:error, "Discord authentication failed"}
  end

  defp handle_discord_response({:ok, %HTTPoison.Response{status_code: 403}}) do
    Logger.error("Discord permission error")
    {:error, "Insufficient permissions to send message"}
  end

  defp handle_discord_response({:ok, %HTTPoison.Response{status_code: 429, body: body}}) do
    handle_rate_limit(body)
  end

  defp handle_discord_response({:ok, %HTTPoison.Response{status_code: status}}) do
    Logger.error("Discord HTTP error: #{status}")
    {:error, "Discord service error: HTTP #{status}"}
  end

  defp handle_discord_response({:error, %HTTPoison.Error{reason: reason}}) do
    Logger.error("Discord request error: #{inspect(reason)}")
    {:error, "Failed to connect to Discord"}
  end

  defp parse_success_response(body) do
    case Jason.decode(body) do
      {:ok, %{"id" => message_id}} ->
        {:ok, %{message_id: message_id}}

      {:ok, error} ->
        Logger.error("Discord API error: #{inspect(error)}")
        {:error, "Failed to send Discord message"}

      {:error, _} ->
        Logger.error("Invalid Discord API response")
        {:error, "Invalid Discord API response"}
    end
  end

  defp handle_rate_limit(response_body) do
    case Jason.decode(response_body) do
      {:ok, %{"retry_after" => retry_after}} ->
        Logger.warning("Discord rate limit hit, retry after #{retry_after}ms")
        Process.sleep(retry_after)
        {:error, "Rate limited, please retry"}

      {:ok, error} ->
        Logger.error("Discord rate limit error: #{inspect(error)}")
        {:error, "Discord rate limit error"}

      {:error, _} ->
        Logger.error("Invalid Discord rate limit response")
        {:error, "Discord rate limit error"}
    end
  end
end
