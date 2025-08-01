defmodule Spacecast.Integration.WebhookAdapterBehaviour do
  @moduledoc """
  Behaviour for webhook adapter implementations.
  """

  @callback send_webhook(String.t(), map()) :: {:ok, map()} | {:error, term()}
  @callback handle_webhook_failure(String.t(), non_neg_integer()) ::
              {:ok, map()} | {:error, term()}
  @callback retry_failed_webhook(String.t(), map()) :: {:ok, map()} | {:error, term()}
  @callback validate_signature(map(), String.t(), String.t()) ::
              {:ok, boolean()} | {:error, term()}
end
