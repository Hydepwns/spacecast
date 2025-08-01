defmodule Spacecast.Events do
  @moduledoc """
  The Events context.
  """

  import Ecto.Query, warn: false
  alias Spacecast.Repo
  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.{EventSettings, EventReminder, EventNotification}

  @doc """
  Returns the list of all events.

  ## Parameters
  * `filters` - Map of filters to apply (default: %{})
  * `opts` - Additional options (default: [])

  ## Returns
  * List of events matching the filters
  """
  def list_events(_filters \\ %{}, _opts \\ []) do
    case Spacecast.Events.EventStore.list_all_events() do
      {:ok, events} -> events
      {:error, _} -> []
    end
  end

  @doc """
  Gets a single event.

  Raises `Ecto.NoResultsError` if the Event does not exist.
  """
  def get_event!(id), do: Repo.get!(Event, id)

  @doc """
  Gets a single event.

  Returns nil if the Event does not exist.
  """
  def get_event(id) do
    case Spacecast.Events.EventStore.get_events(%{id: id}) do
      {:ok, [event]} -> {:ok, event}
      {:ok, []} -> {:error, :not_found}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Creates an event.
  """
  def create_event(attrs \\ %{}) do
    # Check if this looks like a calendar event (has title, start_time, end_time)
    if Map.has_key?(attrs, :title) or Map.has_key?(attrs, "title") do
      # This is a calendar event, create an event sourcing event with the calendar data
      event_type = "calendar_event.created"
      resource_id = Ecto.UUID.generate()
      resource_type = "calendar_event"

      event_data = %{
        resource_id: resource_id,
        resource_type: resource_type,
        data: attrs
      }

      Spacecast.Events.EventStore.store_event(event_type, event_data)
    else
      # This is an event sourcing event, use the original logic
      Spacecast.Events.EventStore.store_event(attrs.type, attrs)
    end
  end

  @doc """
  Updates an event.
  """
  def update_event(%Event{} = event, attrs) do
    event
    |> Event.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes an event.
  """
  def delete_event(%Event{} = event) do
    Repo.delete(event)
  end

  @doc """
  Returns a changeset for creating/updating an event.
  """
  def change_event(%Event{} = event, attrs \\ %{}) do
    Event.changeset(event, attrs)
  end

  @doc """
  Gets event statistics.
  """
  def get_event_statistics do
    total_events = Repo.aggregate(Event, :count)

    upcoming_events =
      Repo.aggregate(from(e in Event, where: e.timestamp >= ^DateTime.utc_now()), :count)

    past_events = total_events - upcoming_events

    %{
      total: total_events,
      upcoming: upcoming_events,
      past: past_events
    }
  end

  @doc """
  Returns the list of past events.
  """
  def list_events_past do
    case Spacecast.Events.EventStore.get_events(%{
           timestamp: %{lt: DateTime.utc_now()},
           sort: [timestamp: :desc]
         }) do
      {:ok, events} -> events
      {:error, _} -> []
    end
  end

  @doc """
  Returns the list of upcoming events.
  """
  def list_events_upcoming do
    case Spacecast.Events.EventStore.get_events(%{
           timestamp: %{gte: DateTime.utc_now()},
           sort: [timestamp: :asc]
         }) do
      {:ok, events} -> events
      {:error, _} -> []
    end
  end

  @doc """
  Gets events for a specific resource.
  """
  def get_events_for_resource(resource_type, resource_id) do
    Spacecast.Events.EventStore.get_events(%{
      resource_type: resource_type,
      resource_id: resource_id,
      sort: [timestamp: :desc]
    })
  end

  @doc """
  Gets events with the given criteria.
  """
  def get_events(criteria) do
    Spacecast.Events.EventStore.get_events(criteria)
  end

  @doc """
  Returns a changeset for event notification.
  """
  def change_event_notification(notification \\ %EventNotification{}, attrs \\ %{}) do
    EventNotification.changeset(notification, attrs)
  end

  @doc """
  Returns a changeset for event notification template.
  """
  def change_event_notification_template(template \\ %EventNotification{}, attrs \\ %{}) do
    EventNotification.changeset(template, attrs)
  end

  @doc """
  Creates an event notification.
  """
  def create_event_notification(attrs) do
    %EventNotification{}
    |> EventNotification.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Creates an event notification template.
  """
  def create_event_notification_template(attrs \\ %{}) do
    %EventNotification{}
    |> EventNotification.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Exports an event report.
  """
  def export_report(report_id) do
    events = list_events()
    filename = "report_#{report_id}.csv"

    headers = ["ID", "Title", "Start Time", "End Time", "Description", "Status"]

    rows =
      Enum.map(events, fn event ->
        [
          event.id,
          event.title,
          event.start_time,
          event.end_time,
          event.description,
          event.status
        ]
      end)

    content =
      [headers | rows]
      |> Enum.map_join("\n", &Enum.join(&1, ","))

    case File.write(filename, content) do
      :ok -> {:ok, filename}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Returns a changeset for event settings.
  """
  def change_event_settings(settings \\ %EventSettings{}, attrs \\ %{}) do
    EventSettings.changeset(settings, attrs)
  end

  @doc """
  Gets the current event settings.
  """
  def get_event_settings do
    case Spacecast.RepoHelper.get(EventSettings, 1) do
      nil -> %EventSettings{}
      settings -> settings
    end
  end

  @doc """
  Updates event settings.
  """
  def update_event_settings(%EventSettings{} = settings, attrs) do
    settings
    |> EventSettings.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Returns the list of available timezones.
  """
  def get_available_timezones do
    Tzdata.zone_list()
  end

  @doc """
  Returns the list of available timezones.
  """
  def list_events_timezones do
    [
      "UTC",
      "America/New_York",
      "America/Chicago",
      "America/Denver",
      "America/Los_Angeles",
      "Europe/London",
      "Europe/Paris",
      "Europe/Berlin",
      "Asia/Tokyo",
      "Asia/Shanghai",
      "Australia/Sydney"
    ]
  end

  @doc """
  Returns a changeset for an event reminder.
  """
  def change_event_reminder(reminder \\ %EventReminder{}, attrs \\ %{}) do
    EventReminder.changeset(reminder, attrs)
  end

  @doc """
  Creates an event reminder.
  """
  def create_event_reminder(attrs \\ %{}) do
    %EventReminder{}
    |> EventReminder.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates an event reminder.
  """
  def update_event_reminder(%EventReminder{} = reminder, attrs) do
    reminder
    |> EventReminder.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes an event reminder.
  """
  def delete_event_reminder(%EventReminder{} = reminder) do
    Repo.delete(reminder)
  end

  @doc """
  Gets a single event reminder. Raises if not found.
  """
  def get_event_reminder!(id), do: Repo.get!(EventReminder, id)

  @doc """
  Returns the list of all event reminders.
  """
  def list_event_reminders do
    Repo.all(EventReminder)
  end

  @doc """
  Gets a single event notification.

  Raises `Ecto.NoResultsError` if the EventNotification does not exist.
  """
  def get_event_notification!(id), do: Repo.get!(EventNotification, id)

  @doc """
  Returns the list of event notifications.
  """
  def list_event_notifications do
    Repo.all(EventNotification)
  end

  @doc """
  Updates an event notification.
  """
  def update_event_notification(%EventNotification{} = notification, attrs) do
    notification
    |> EventNotification.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes an event notification.
  """
  def delete_event_notification(%EventNotification{} = notification) do
    Repo.delete(notification)
  end

  @doc """
  Returns the list of events within a date range.
  """
  def list_events_by_date_range(start_date, end_date) do
    from(e in Event,
      where: e.start_time >= ^start_date and e.end_time <= ^end_date,
      order_by: [asc: e.start_time]
    )
    |> Repo.all()
  end

  @doc """
  Gets a single event settings.

  Raises `Ecto.NoResultsError` if the EventSettings does not exist.
  """
  def get_event_settings!(id), do: Repo.get!(EventSettings, id)

  @doc """
  Gets event settings by event id.
  """
  def get_event_settings_by_event_id(event_id) do
    Repo.get_by(EventSettings, event_id: event_id)
  end

  @doc """
  Deletes event settings.
  """
  def delete_event_settings(%EventSettings{} = settings) do
    Repo.delete(settings)
  end

  def delete_event_settings(event_id) when is_integer(event_id) do
    case get_event_settings_by_event_id(event_id) do
      nil -> {:error, :not_found}
      settings -> delete_event_settings(settings)
    end
  end

  @doc """
  Creates event settings.
  """
  def create_event_settings(attrs \\ %{}) do
    %EventSettings{}
    |> EventSettings.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Returns a list of supported timezones.
  Uses the IANA timezone database names.
  """
  @spec list_timezones() :: [String.t()]
  def list_timezones do
    [
      "UTC",
      "America/New_York",
      "America/Chicago",
      "America/Denver",
      "America/Los_Angeles",
      "Europe/London",
      "Europe/Paris",
      "Europe/Berlin",
      "Asia/Tokyo",
      "Asia/Shanghai",
      "Australia/Sydney"
    ]
  end

  @doc """
  Returns a list of past events.
  """
  def list_past_events do
    now = DateTime.utc_now()

    from(e in Event,
      where: e.start_time < ^now,
      order_by: [desc: e.start_time]
    )
    |> Repo.all()
  end

  @doc """
  Returns a list of upcoming events.
  """
  def list_upcoming_events do
    now = DateTime.utc_now()

    from(e in Event,
      where: e.start_time >= ^now,
      order_by: [asc: e.start_time]
    )
    |> Repo.all()
  end

  @doc """
  Returns a list of due reminders for a given event.
  """
  def list_due_reminders(event_id) when is_binary(event_id) do
    now = DateTime.utc_now()

    from(r in EventReminder,
      where: r.event_id == ^event_id and r.reminder_time <= ^now and r.status == "pending",
      order_by: [asc: r.reminder_time]
    )
    |> Repo.all()
  end

  def list_due_reminders(_invalid_id), do: {:error, :invalid_event_id}

  @doc """
  Returns a list of all due reminders across all events.
  """
  def list_all_due_reminders do
    now = DateTime.utc_now()

    from(r in EventReminder,
      where: r.reminder_time <= ^now and r.status == "pending",
      order_by: [asc: r.reminder_time]
    )
    |> Repo.all()
  end

  @doc """
  Imports events from a file.
  """
  def import_events(file_path) do
    case File.read(file_path) do
      {:ok, content} ->
        events =
          content
          |> String.split("\n")
          # Skip header row
          |> Enum.drop(1)
          |> Enum.map(&parse_event_row/1)
          |> Enum.filter(&(&1 != nil))
          |> Enum.map(&create_event/1)

        {:ok, events}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_event_row(row) do
    case String.split(row, ",") do
      [title, start_time, end_time, description, status] ->
        %{
          title: title,
          start_time: parse_datetime(start_time),
          end_time: parse_datetime(end_time),
          description: description,
          status: status
        }

      _ ->
        nil
    end
  end

  defp parse_datetime(datetime_str) do
    case DateTime.from_iso8601(datetime_str) do
      {:ok, datetime, _} -> datetime
      _ -> nil
    end
  end

  @doc """
  Generates an event report.
  """
  def generate_event_report(report_id) do
    events = list_events()
    filename = "report_#{report_id}.csv"

    headers = ["ID", "Title", "Start Time", "End Time", "Description", "Status"]

    rows =
      Enum.map(events, fn event ->
        [
          event.id,
          event.title,
          event.start_time,
          event.end_time,
          event.description,
          event.status
        ]
      end)

    content =
      [headers | rows]
      |> Enum.map_join("\n", &Enum.join(&1, ","))

    case File.write(filename, content) do
      :ok -> {:ok, filename}
      {:error, reason} -> {:error, reason}
    end
  end

  # Wrapper for compatibility: create_event/2
  def create_event(type, attrs), do: Spacecast.Events.EventStore.store_event(type, attrs)
end
