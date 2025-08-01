defmodule Spacecast.Events.Adapters.Adapter do
  @moduledoc """
  Behaviour for reminder delivery adapters.
  Each adapter must implement the send_reminder/4 callback.
  """

  @type reminder :: struct()
  @type settings :: struct()
  @type config :: map()
  @type encrypted_message :: String.t()
  @type result :: {:ok, String.t()} | {:error, String.t()}

  @callback send_reminder(reminder(), settings(), config(), encrypted_message()) :: result()
end
