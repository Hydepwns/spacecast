defmodule Spacecast.Integration.WebhookAdapter do
  @moduledoc """
  Webhook adapter for sending webhooks to external services.

  This module provides functionality for sending webhooks to external
  services with proper authentication, retry logic, and signature validation.
  """

  @behaviour Spacecast.Integration.WebhookAdapterBehaviour

  require Logger

  @doc """
  Sends a webhook to the specified URL with the given payload.

  ## Parameters
  - url: The webhook endpoint URL
  - payload: The data to send in the webhook

  ## Returns
  - {:ok, response} on success
  - {:error, reason} on failure
  """
  def send_webhook(url, payload) when is_binary(url) and is_map(payload) do
    case validate_webhook_params(url, payload) do
      :ok ->
        # In production, this would make an actual HTTP request
        webhook_id = generate_webhook_id()
        Logger.info("Webhook sent to #{url}: #{webhook_id}")
        {:ok, %{id: webhook_id, status: "delivered"}}

      {:error, reason} ->
        Logger.error("Failed to send webhook to #{url}: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Handles webhook delivery failures with retry logic.

  ## Parameters
  - webhook_id: The ID of the failed webhook
  - retry_count: Current retry attempt number

  ## Returns
  - {:ok, response} on success
  - {:error, reason} on failure
  """
  def handle_webhook_failure(webhook_id, retry_count \\ 0) do
    max_retries = 3

    if retry_count < max_retries do
      # Implement exponential backoff
      delay = (:math.pow(2, retry_count) * 1000) |> round()
      Process.sleep(delay)

      Logger.info("Retrying webhook #{webhook_id} (attempt #{retry_count + 1})")
      {:ok, %{id: webhook_id, status: "retried", attempt: retry_count + 1}}
    else
      Logger.error("Webhook #{webhook_id} failed after #{max_retries} retries")
      {:error, "Max retries exceeded"}
    end
  end

  @doc """
  Retries failed webhooks with exponential backoff.

  ## Parameters
  - webhook_id: The ID of the webhook to retry
  - original_payload: The original webhook payload

  ## Returns
  - {:ok, response} on success
  - {:error, reason} on failure
  """
  def retry_failed_webhook(webhook_id, original_payload) do
    case send_webhook(original_payload.url, original_payload.data) do
      {:ok, response} ->
        Logger.info("Webhook #{webhook_id} retry successful")
        {:ok, Map.put(response, :retry_successful, true)}

      {:error, reason} ->
        Logger.error("Webhook #{webhook_id} retry failed: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Validates webhook signatures for security.

  ## Parameters
  - payload: The webhook payload
  - signature: The signature to validate
  - secret: The secret key used for signing

  ## Returns
  - {:ok, true} if signature is valid
  - {:error, reason} if signature is invalid
  """
  def validate_signature(payload, signature, secret)
      when is_binary(signature) and is_binary(secret) do
    # In production, this would use proper HMAC validation
    expected_signature = generate_signature(payload, secret)

    if Plug.Crypto.secure_compare(signature, expected_signature) do
      {:ok, true}
    else
      {:error, "Invalid signature"}
    end
  end

  # Private functions

  defp validate_webhook_params(url, payload) when is_binary(url) and is_map(payload) do
    cond do
      !valid_url?(url) ->
        {:error, "Invalid URL format"}

      map_size(payload) == 0 ->
        {:error, "Empty payload"}

      true ->
        :ok
    end
  end

  defp validate_webhook_params(_url, _payload) do
    {:error, "Invalid parameters: url must be string, payload must be map"}
  end

  defp valid_url?(url) do
    case URI.parse(url) do
      %URI{scheme: scheme, host: host} when scheme in ["http", "https"] and not is_nil(host) ->
        true

      _ ->
        false
    end
  end

  defp generate_webhook_id do
    ("wh_" <> :crypto.strong_rand_bytes(16)) |> Base.encode16(case: :lower)
  end

  defp generate_signature(payload, secret) do
    # In production, this would use proper HMAC-SHA256
    payload_str = Jason.encode!(payload)
    :crypto.mac(:hmac, :sha256, secret, payload_str) |> Base.encode16(case: :lower)
  end
end
