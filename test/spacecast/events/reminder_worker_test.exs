defmodule Spacecast.Events.ReminderWorkerTest do
  use Spacecast.DataCase, async: false

  alias Spacecast.Events

  setup do
    # ReminderWorker is already started globally in the application

    # Create an event directly in the database (not using MockEventStore)
    {:ok, event} =
      %Spacecast.Events.Core.Event{}
      |> Spacecast.Events.Core.Event.changeset(%{
        type: "calendar_event.created",
        data: %{
          title: "Test Event",
          description: "Test Description",
          start_time:
            DateTime.utc_now() |> DateTime.add(3600, :second) |> DateTime.truncate(:second),
          end_time:
            DateTime.utc_now() |> DateTime.add(7200, :second) |> DateTime.truncate(:second)
        },
        resource_type: "calendar_event",
        resource_id: Ecto.UUID.generate(),
        timestamp: DateTime.utc_now() |> DateTime.truncate(:second)
      })
      |> Spacecast.Repo.insert()

    # Create event settings
    {:ok, settings} =
      Events.create_event_settings(%{
        event_id: event.id,
        reminder_time: 30,
        reminder_type: "email",
        reminder_message: "Test reminder message"
      })

    # Create a due reminder
    {:ok, due_reminder} =
      Events.create_event_reminder(%{
        event_id: event.id,
        reminder_time:
          DateTime.utc_now() |> DateTime.add(-60, :second) |> DateTime.truncate(:second),
        status: "pending",
        recipient: "test@example.com"
      })

    # Create a future reminder
    {:ok, future_reminder} =
      Events.create_event_reminder(%{
        event_id: event.id,
        reminder_time:
          DateTime.utc_now() |> DateTime.add(3600, :second) |> DateTime.truncate(:second),
        status: "pending",
        recipient: "test@example.com"
      })

    # Allow ReminderWorker process to use the test DB connection
    Ecto.Adapters.SQL.Sandbox.allow(
      Spacecast.Repo,
      self(),
      Process.whereis(Spacecast.Events.ReminderWorker)
    )

    %{
      event: event,
      settings: settings,
      due_reminder: due_reminder,
      future_reminder: future_reminder
    }
  end

  test "processes due reminders", %{due_reminder: due_reminder} do
    # Manually trigger the reminder worker to process reminders
    send(Spacecast.Events.ReminderWorker, :check_reminders)

    # Wait for processing to complete
    wait_for_reminder_processing(due_reminder.id)

    # Check that the due reminder was processed
    updated_reminder = Events.get_event_reminder!(due_reminder.id)
    assert updated_reminder.status == "sent"
    assert updated_reminder.sent_at != nil
  end

  test "does not process future reminders", %{future_reminder: future_reminder} do
    # Manually trigger the reminder worker to process reminders
    send(Spacecast.Events.ReminderWorker, :check_reminders)

    # Wait a bit for processing
    wait_for_reminder_processing(future_reminder.id)

    # Check that the future reminder was not processed
    updated_reminder = Events.get_event_reminder!(future_reminder.id)
    assert updated_reminder.status == "pending"
    assert updated_reminder.sent_at == nil
  end

  # Helper function to wait for reminder processing
  defp wait_for_reminder_processing(reminder_id, max_attempts \\ 10) do
    wait_for_reminder_processing(reminder_id, max_attempts, 0)
  end

  defp wait_for_reminder_processing(_reminder_id, max_attempts, attempts)
       when attempts >= max_attempts do
    :ok
  end

  defp wait_for_reminder_processing(reminder_id, max_attempts, attempts) do
    reminder = Events.get_event_reminder!(reminder_id)

    case reminder.status do
      "sent" ->
        :ok

      "pending" ->
        Process.sleep(50)
        wait_for_reminder_processing(reminder_id, max_attempts, attempts + 1)
    end
  end
end
