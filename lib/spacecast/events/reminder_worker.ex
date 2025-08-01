defmodule Spacecast.Events.ReminderWorker do
  @moduledoc """
  Background worker for processing event reminders.
  """

  use GenServer
  require Logger

  alias Spacecast.Events
  alias Spacecast.Events.ReminderDelivery

  @check_interval :timer.minutes(1)

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  @impl true
  def init(state) do
    schedule_check()
    {:ok, state}
  end

  @impl true
  def handle_info(:check_reminders, state) do
    process_due_reminders()
    schedule_check()
    {:noreply, state}
  end

  defp schedule_check do
    Process.send_after(self(), :check_reminders, @check_interval)
  end

  defp process_due_reminders do
    Events.list_all_due_reminders()
    |> Enum.each(fn reminder ->
      case ReminderDelivery.send_reminder(reminder) do
        {:ok, _} ->
          Logger.info("Successfully sent reminder #{reminder.id}")

        {:error, reason} ->
          Logger.error("Failed to send reminder #{reminder.id}: #{reason}")
      end
    end)
  end
end
