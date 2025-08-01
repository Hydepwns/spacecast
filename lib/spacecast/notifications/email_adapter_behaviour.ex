defmodule Spacecast.Notifications.EmailAdapterBehaviour do
  @moduledoc """
  Behaviour for email adapter implementations.
  """

  @callback send_email(String.t(), String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  @callback send_html_email(String.t(), String.t(), String.t(), String.t() | nil) ::
              {:ok, String.t()} | {:error, term()}
  @callback send_templated_email(String.t(), String.t(), map()) ::
              {:ok, String.t()} | {:error, term()}
end
