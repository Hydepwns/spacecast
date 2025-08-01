defmodule Spacecast.Events.SMSAdapter do
  @moduledoc """
  Adapter for sending SMS reminders.
  """

  @behaviour Spacecast.Events.ReminderAdapter

  @impl true
  def send_reminder(reminder, _settings) do
    # TODO: Implement actual SMS sending using your preferred SMS service
    # For now, we'll just simulate a successful delivery
    {:ok, "SMS sent to #{reminder.recipient}"}
  end
end
