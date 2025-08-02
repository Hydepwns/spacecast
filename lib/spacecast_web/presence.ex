defmodule SpacecastWeb.Presence do
  @moduledoc """
  Phoenix Presence module for tracking user presence and broadcasting updates.
  """

  use Phoenix.Presence,
    otp_app: :spacecast,
    pubsub_server: Spacecast.PubSub

  def broadcast_to_users(_topic, event, payload, user_ids) do
    Enum.each(user_ids, fn user_id ->
      Phoenix.PubSub.broadcast(
        Spacecast.PubSub,
        "user:#{user_id}",
        {event, payload}
      )
    end)
  end
end
