defmodule Spacecast.Events.Adapters.MockEmailAdapter do
  @moduledoc """
  Mock email adapter for testing purposes.
  Always returns success without actually sending emails.
  """

  @behaviour Spacecast.Events.Adapters.Adapter
  require Logger

  @impl true
  def send_reminder(reminder, _settings, _config, _encrypted_message) do
    Logger.info("Mock email sent to #{reminder.recipient}")
    {:ok, "Mock email sent successfully"}
  end
end
