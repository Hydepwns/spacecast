defmodule Spacecast.Events.DeliveryService do
  @moduledoc """
  Manages delivery of reminders through various channels.
  Supports multiple delivery methods and can be easily extended.
  """

  @type delivery_type :: :email | :sms | :push | :webhook
  @type delivery_result :: {:ok, String.t()} | {:error, String.t()}

  @doc """
  Sends a reminder through the specified delivery method.
  Returns {:ok, message} on success or {:error, reason} on failure.
  """
  @spec send_reminder(struct(), delivery_type(), map()) :: delivery_result()
  def send_reminder(reminder, delivery_type, settings) do
    crypto_service =
      Application.get_env(
        :spacecast,
        :crypto_service,
        Spacecast.Events.CryptoService
      )

    with {:ok, config} <- get_config(delivery_type),
         {:ok, adapter} <- get_adapter_with_config(delivery_type, config),
         {:ok, encrypted_message} <- crypto_service.encrypt_message(reminder, settings) do
      adapter.send_reminder(reminder, settings, config, encrypted_message)
    end
  end

  @doc """
  Sends a reminder through multiple delivery methods.
  Returns a map of results for each delivery method.
  """
  @spec send_reminder_multi(struct(), [delivery_type()], map()) :: %{
          delivery_type() => delivery_result()
        }
  def send_reminder_multi(reminder, delivery_types, settings) do
    delivery_types
    |> Enum.map(fn type -> {type, send_reminder(reminder, type, settings)} end)
    |> Map.new()
  end

  # Private functions

  defp get_adapter(:email), do: {:ok, Spacecast.Events.Adapters.EmailAdapter}
  defp get_adapter(:sms), do: {:ok, Spacecast.Events.Adapters.SMSAdapter}
  defp get_adapter(:push), do: {:ok, Spacecast.Events.Adapters.PushAdapter}
  defp get_adapter(:webhook), do: {:ok, Spacecast.Events.Adapters.WebhookAdapter}
  defp get_adapter(_), do: {:error, "Unsupported delivery type"}

  defp get_config(delivery_type) do
    config = Application.get_env(:spacecast, :reminder_services)

    case config[delivery_type] do
      nil -> {:error, "No configuration found for #{delivery_type}"}
      config -> {:ok, config}
    end
  end

  # Override adapter for mock provider in tests
  defp get_adapter_with_config(:email, %{provider: "mock"}),
    do: {:ok, Spacecast.Events.Adapters.MockEmailAdapter}

  defp get_adapter_with_config(delivery_type, _config), do: get_adapter(delivery_type)
end
