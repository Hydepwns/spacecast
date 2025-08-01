defmodule Spacecast.Events.ReminderAdapter do
  @moduledoc """
  Behaviour for reminder delivery adapters.
  """

  @callback send_reminder(reminder :: struct(), settings :: struct()) ::
              {:ok, String.t()} | {:error, String.t()}
end
