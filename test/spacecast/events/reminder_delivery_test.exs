defmodule Spacecast.Events.ReminderDeliveryTest do
  use Spacecast.DataCase

  alias Spacecast.Events
  alias Spacecast.Events.ReminderDelivery
  alias Spacecast.Events.Event
  alias Spacecast.Repo

  describe "send_reminder/1" do
    setup do
      # Create a calendar event (not an event sourcing event)
      {:ok, event} =
        %Event{}
        |> Event.changeset(%{
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
          resource_id: Ecto.UUID.generate()
        })
        |> Repo.insert()

      # Create event settings
      {:ok, settings} =
        Events.create_event_settings(%{
          event_id: event.id,
          reminder_time: 30,
          reminder_type: "email",
          reminder_message: "Test reminder message"
        })

      # Create a reminder
      {:ok, reminder} =
        Events.create_event_reminder(%{
          event_id: event.id,
          # 30 minutes from now
          reminder_time:
            DateTime.utc_now() |> DateTime.add(1800, :second) |> DateTime.truncate(:second),
          status: "pending",
          recipient: "test@example.com"
        })

      %{event: event, settings: settings, reminder: reminder}
    end

    test "successfully sends a reminder", %{reminder: reminder} do
      result = ReminderDelivery.send_reminder(reminder)
      assert match?({:ok, %_{}}, result)
      {:ok, updated_reminder} = result
      assert updated_reminder.status == "sent"
      assert updated_reminder.sent_at != nil
    end

    test "fails to send a non-pending reminder", %{reminder: reminder} do
      {:ok, sent_reminder} = Events.update_event_reminder(reminder, %{status: "sent"})

      assert {:error, "Reminder is not in pending status"} =
               ReminderDelivery.send_reminder(sent_reminder)
    end

    test "fails when event settings are missing", %{reminder: reminder} do
      # Delete event settings
      Events.delete_event_settings(Events.get_event_settings_by_event_id(reminder.event_id))

      assert {:error, "No event settings found"} = ReminderDelivery.send_reminder(reminder)
    end
  end
end
