defmodule Spacecast.Events.Adapters.PushAdapterBehaviour do
  @moduledoc """
  Behaviour for push adapter implementations.
  """

  @callback send_notification(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  @callback send_bulk_notification(list(), map()) :: {:ok, list()} | {:error, term()}
  @callback send_topic_notification(String.t(), map()) :: {:ok, String.t()} | {:error, term()}
  @callback schedule_notification(String.t(), map(), DateTime.t()) ::
              {:ok, String.t()} | {:error, term()}
  @callback cancel_scheduled_notification(String.t()) :: {:ok, String.t()} | {:error, term()}
  @callback get_notification_status(String.t()) :: {:ok, map()} | {:error, term()}
end
