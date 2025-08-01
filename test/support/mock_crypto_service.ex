defmodule Spacecast.Events.MockCryptoService do
  @moduledoc """
  Mock crypto service for testing purposes.
  Always returns success without actual encryption.
  """

  require Logger

  @doc """
  Mock encryption that always succeeds.
  """
  def encrypt_message(_reminder, _settings) do
    Logger.info("Mock encryption completed")
    {:ok, "mock_encrypted_message"}
  end

  @doc """
  Mock decryption that always succeeds.
  """
  def decrypt_message(_encrypted_message, _session_id) do
    Logger.info("Mock decryption completed")
    {:ok, "mock_decrypted_message"}
  end
end
