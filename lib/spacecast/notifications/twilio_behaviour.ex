defmodule Spacecast.Notifications.TwilioBehaviour do
  @moduledoc """
  Behaviour for Twilio SMS service implementations.
  """

  @callback send_sms(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  @callback send_sms_with_template(String.t(), String.t(), map()) ::
              {:ok, String.t()} | {:error, term()}
end
