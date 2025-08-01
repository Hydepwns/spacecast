defmodule Spacecast.Events.EmailAdapter do
  @moduledoc """
  Adapter for sending email reminders.
  """

  @behaviour Spacecast.Events.ReminderAdapter

  @impl true
  def send_reminder(reminder, _settings) do
    # TODO: Implement actual email sending using your preferred email service
    # For now, we'll just simulate a successful delivery
    {:ok, "Email sent to #{reminder.recipient}"}
  end
end
