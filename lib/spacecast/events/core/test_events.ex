defmodule Spacecast.Events.Core.TestEvents do
  @moduledoc """
  Utility module for generating test events.

  This module provides functions to generate realistic test data for the
  Resource Event System, including user events, resource events, and more.
  """

  require Logger

  alias Spacecast.Events.Event

  @doc """
  Generates a set of test data for the specified number of users.

  This will create a series of events that simulate user activity:
  - User creation
  - Multiple logins and logouts
  - Resource creation and updates
  - Some user deletions

  Returns {:ok, events} with the list of generated events.
  """
  @spec generate_test_data(pos_integer()) :: {:ok, [any()]}
  def generate_test_data(user_count \\ 5) do
    Logger.info("Generating test data for #{user_count} users")

    # Generate user events
    events = generate_user_events(user_count)

    # Publish all events
    Enum.each(events, &Spacecast.Events.EventBus.publish/1)

    {:ok, events}
  end

  @doc """
  Generates a single random event.

  Options:
  - :type - Specify the event type (default: random)
  - :resource_type - Specify the resource type (default: random)
  - :resource_id - Specify the resource ID (default: random)
  """
  @spec generate_random_event(Keyword.t()) :: {:ok, any()} | {:error, any()}
  def generate_random_event(opts \\ []) do
    # Get or generate event type
    event_type = Keyword.get(opts, :type, random_event_type())

    # Get or generate resource type and ID
    resource_type = Keyword.get(opts, :resource_type, random_resource_type())
    resource_id = Keyword.get(opts, :resource_id, random_id(resource_type))

    # Generate event data based on type
    event_data = generate_event_data(event_type, resource_type, resource_id)

    # Create the event
    Event.create(event_type, %{
      resource_id: resource_id,
      resource_type: resource_type,
      data: event_data
    })
  end

  # Private functions

  # Generates a series of user events
  defp generate_user_events(user_count) do
    # Generate user IDs
    user_ids = Enum.map(1..user_count, fn i -> "test-user-#{i}" end)

    # For each user, generate a series of events
    user_ids
    |> Enum.flat_map(fn user_id ->
      # Number of events per user (between 3 and 10)
      event_count = Enum.random(3..10)

      # Generate events for this user
      generate_user_lifecycle(user_id, event_count)
    end)
  end

  # Generates a lifecycle of events for a single user
  defp generate_user_lifecycle(user_id, event_count) do
    # Start with user creation
    {:ok, creation_event} =
      Event.create("user.created", %{
        resource_id: user_id,
        resource_type: "user",
        data: %{
          username: "user_#{user_id}",
          email: "#{user_id}@example.com",
          created_at: DateTime.utc_now() |> DateTime.to_iso8601()
        }
      })

    # Generate login/logout events
    login_events = generate_login_events(user_id, event_count)

    # Maybe generate resource events
    resource_events =
      if Enum.random(1..100) <= 70 do
        resource_count = Enum.random(1..3)
        generate_resource_events(user_id, resource_count)
      else
        []
      end

    # Maybe generate a user deletion event
    deletion_events =
      if Enum.random(1..100) <= 20 do
        {:ok, event} =
          Event.create("user.deleted", %{
            resource_id: user_id,
            resource_type: "user",
            data: %{
              reason: Enum.random(["requested", "inactive", "violation"])
            }
          })

        [event]
      else
        []
      end

    # Combine all events
    [creation_event] ++ login_events ++ resource_events ++ deletion_events
  end

  # Generates login/logout events for a user
  defp generate_login_events(user_id, count) do
    Enum.flat_map(1..count, fn _ ->
      # Generate a login event
      {:ok, login_event} =
        Event.create("user.login", %{
          resource_id: user_id,
          resource_type: "user",
          data: %{
            ip: random_ip(),
            user_agent: random_user_agent(),
            timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
          }
        })

      # Maybe generate a logout event (80% chance)
      logout_events =
        if Enum.random(1..100) <= 80 do
          # Add some time delay
          Process.sleep(Enum.random(10..100))

          {:ok, logout_event} =
            Event.create("user.logout", %{
              resource_id: user_id,
              resource_type: "user",
              data: %{
                timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
              }
            })

          [logout_event]
        else
          []
        end

      [login_event] ++ logout_events
    end)
  end

  # Generates resource events for a user
  defp generate_resource_events(user_id, count) do
    Enum.flat_map(1..count, fn i ->
      # Generate a resource ID
      resource_type = Enum.random(["document", "image", "project"])
      resource_id = "#{resource_type}-#{user_id}-#{i}"

      # Generate a creation event
      {:ok, creation_event} =
        Event.create("resource.created", %{
          resource_id: resource_id,
          resource_type: resource_type,
          data: %{
            owner_id: user_id,
            name: "#{String.capitalize(resource_type)} #{i}",
            created_at: DateTime.utc_now() |> DateTime.to_iso8601()
          }
        })

      # Maybe generate update events
      update_events =
        if Enum.random(1..100) <= 70 do
          update_count = Enum.random(1..3)

          Enum.map(1..update_count, fn _ ->
            # Add some time delay
            Process.sleep(Enum.random(10..50))

            {:ok, update_event} =
              Event.create("resource.updated", %{
                resource_id: resource_id,
                resource_type: resource_type,
                data: %{
                  updated_by: user_id,
                  changes: random_changes(resource_type),
                  updated_at: DateTime.utc_now() |> DateTime.to_iso8601()
                }
              })

            update_event
          end)
        else
          []
        end

      # Maybe generate a deletion event
      deletion_events =
        if Enum.random(1..100) <= 30 do
          # Add some time delay
          Process.sleep(Enum.random(10..50))

          {:ok, deletion_event} =
            Event.create("resource.deleted", %{
              resource_id: resource_id,
              resource_type: resource_type,
              data: %{
                deleted_by: user_id,
                reason: Enum.random(["no_longer_needed", "duplicate", "mistake"]),
                deleted_at: DateTime.utc_now() |> DateTime.to_iso8601()
              }
            })

          [deletion_event]
        else
          []
        end

      [creation_event] ++ update_events ++ deletion_events
    end)
  end

  # Generates random event data based on the event type
  defp generate_event_data(event_type, resource_type, resource_id) do
    case event_type do
      "user.created" ->
        %{
          username: "user_#{resource_id}",
          email: "#{resource_id}@example.com",
          created_at: DateTime.utc_now() |> DateTime.to_iso8601()
        }

      "user.login" ->
        %{
          ip: random_ip(),
          user_agent: random_user_agent(),
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
        }

      "user.logout" ->
        %{
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
        }

      "user.deleted" ->
        %{
          reason: Enum.random(["requested", "inactive", "violation"])
        }

      "resource.created" ->
        %{
          owner_id: "user-#{Enum.random(1..10)}",
          name: "#{String.capitalize(resource_type)} #{Enum.random(1..100)}",
          created_at: DateTime.utc_now() |> DateTime.to_iso8601()
        }

      "resource.updated" ->
        %{
          updated_by: "user-#{Enum.random(1..10)}",
          changes: random_changes(resource_type),
          updated_at: DateTime.utc_now() |> DateTime.to_iso8601()
        }

      "resource.deleted" ->
        %{
          deleted_by: "user-#{Enum.random(1..10)}",
          reason: Enum.random(["no_longer_needed", "duplicate", "mistake"]),
          deleted_at: DateTime.utc_now() |> DateTime.to_iso8601()
        }

      _ ->
        %{
          timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
        }
    end
  end

  # Random data generators

  defp random_event_type do
    Enum.random([
      "user.created",
      "user.login",
      "user.logout",
      "user.deleted",
      "resource.created",
      "resource.updated",
      "resource.deleted"
    ])
  end

  defp random_resource_type do
    Enum.random([
      "user",
      "document",
      "image",
      "project"
    ])
  end

  defp random_id(resource_type) do
    "#{resource_type}-#{Enum.random(1..1000)}"
  end

  defp random_ip do
    "#{Enum.random(1..255)}.#{Enum.random(1..255)}.#{Enum.random(1..255)}.#{Enum.random(1..255)}"
  end

  defp random_user_agent do
    Enum.random([
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.1.1 Safari/605.1.15",
      "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36",
      "Mozilla/5.0 (iPhone; CPU iPhone OS 14_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0 Mobile/15E148 Safari/604.1"
    ])
  end

  defp random_changes("document") do
    %{
      title: Enum.random([nil, "Updated Document Title"]),
      content: Enum.random([nil, "Updated content for the document"]),
      tags: Enum.random([nil, ["tag1", "tag2", "tag3"]])
    }
    |> Enum.reject(fn {_, v} -> is_nil(v) end)
    |> Enum.into(%{})
  end

  defp random_changes("image") do
    %{
      title: Enum.random([nil, "Updated Image Title"]),
      description: Enum.random([nil, "Updated image description"]),
      tags: Enum.random([nil, ["photo", "landscape", "nature"]])
    }
    |> Enum.reject(fn {_, v} -> is_nil(v) end)
    |> Enum.into(%{})
  end

  defp random_changes("project") do
    %{
      name: Enum.random([nil, "Updated Project Name"]),
      description: Enum.random([nil, "Updated project description"]),
      status: Enum.random([nil, "in_progress", "completed", "on_hold"])
    }
    |> Enum.reject(fn {_, v} -> is_nil(v) end)
    |> Enum.into(%{})
  end

  defp random_changes(_) do
    %{
      field1: "value1",
      field2: "value2"
    }
  end
end
