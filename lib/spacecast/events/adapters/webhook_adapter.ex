defmodule Spacecast.Events.Adapters.WebhookAdapter do
  @moduledoc """
  Adapter for sending notifications to custom webhook endpoints.
  Supports multiple authentication methods, retries, and custom payload formatting.
  """

  @behaviour Spacecast.Events.Adapters.Adapter
  require Logger

  @max_retries 3
  # 1 second
  @retry_delay 1000

  @impl true
  def send_reminder(reminder, _settings, config, encrypted_message) do
    with {:ok, webhook_url} <- validate_webhook_url(config.webhook_url),
         {:ok, payload} <- build_webhook_payload(reminder, encrypted_message, config),
         {:ok, _response} <- send_webhook_request(webhook_url, payload, config) do
      Logger.info("Webhook notification sent successfully to #{webhook_url}")
      {:ok, "Webhook notification sent successfully"}
    else
      {:error, :invalid_url} ->
        Logger.error("Invalid webhook URL: #{config.webhook_url}")
        {:error, "Invalid webhook URL"}

      {:error, reason} ->
        Logger.error("Failed to send webhook notification: #{inspect(reason)}")
        {:error, "Failed to send webhook notification"}
    end
  end

  # Private functions

  defp validate_webhook_url(url) do
    case URI.parse(url) do
      %URI{scheme: scheme, host: host} when scheme in ["http", "https"] and not is_nil(host) ->
        {:ok, url}

      _ ->
        {:error, :invalid_url}
    end
  end

  defp build_webhook_payload(reminder, encrypted_message, config) do
    # Build a structured webhook payload
    payload = %{
      event: "reminder",
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      data: %{
        reminder_id: reminder.id,
        title: reminder.title,
        message: reminder.message,
        recipient: reminder.recipient,
        encrypted_message: encrypted_message
      },
      metadata: %{
        source: "spacecast",
        version: config.version || "1.0"
      }
    }

    # Apply custom payload transformation if configured
    payload =
      case config.payload_transform do
        {module, function} when is_atom(module) and is_atom(function) ->
          apply(module, function, [payload])

        _ ->
          payload
      end

    {:ok, payload}
  end

  defp send_webhook_request(url, payload, config, retry_count \\ 0) do
    headers = build_headers(config)
    body = Jason.encode!(payload)

    case HTTPoison.post(url, body, headers) do
      {:ok, %{status_code: status_code}} when status_code in 200..299 ->
        {:ok, "Webhook request sent successfully"}

      {:ok, %{status_code: status_code}} ->
        {:error, "Webhook request failed with status code: #{status_code}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        if retry_count < @max_retries do
          Logger.warning(
            "Connection error, retrying webhook request after #{@retry_delay}ms (attempt #{retry_count + 1}/#{@max_retries})"
          )

          Process.sleep(@retry_delay)
          send_webhook_request(url, payload, config, retry_count + 1)
        else
          Logger.error("Connection error after #{@max_retries} retries: #{inspect(reason)}")
          {:error, "Connection error: #{inspect(reason)}"}
        end
    end
  end

  defp build_headers(config) do
    headers = [
      {"Content-Type", "application/json"},
      {"User-Agent", "Spacecast/1.0"}
    ]

    # Add authentication headers based on config
    headers =
      case config.auth do
        %{type: "basic", username: username, password: password} ->
          auth = Base.encode64("#{username}:#{password}")
          [{"Authorization", "Basic #{auth}"} | headers]

        %{type: "bearer", token: token} ->
          [{"Authorization", "Bearer #{token}"} | headers]

        %{type: "api_key", key: key, value: value} ->
          [{key, value} | headers]

        _ ->
          headers
      end

    # Add custom headers if configured
    case config.custom_headers do
      headers_map when is_map(headers_map) ->
        Enum.reduce(headers_map, headers, fn {key, value}, acc ->
          [{key, value} | acc]
        end)

      _ ->
        headers
    end
  end
end
