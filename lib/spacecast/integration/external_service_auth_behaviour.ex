defmodule Spacecast.Integration.ExternalServiceAuthBehaviour do
  @moduledoc """
  Behaviour for external service auth implementations.
  """

  @callback validate_credentials(String.t(), map()) :: {:ok, boolean()} | {:error, term()}
  @callback encrypt_data(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  @callback decrypt_data(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  @callback generate_auth_token(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  @callback validate_auth_token(String.t(), String.t()) :: {:ok, map()} | {:error, term()}
end
