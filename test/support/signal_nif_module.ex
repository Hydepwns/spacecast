defmodule :signal_nif do
  @moduledoc """
  Test stub for signal_nif module to eliminate undefined function warnings.
  """

  @doc """
  Stub for generate_identity_key_pair/0
  """
  def generate_identity_key_pair do
    {:ok, {"stub_public_key", "stub_private_key"}}
  end

  @doc """
  Stub for generate_pre_key/1
  """
  def generate_pre_key(_key_id) do
    {:ok, {1, "stub_pre_key"}}
  end

  @doc """
  Stub for generate_signed_pre_key/2
  """
  def generate_signed_pre_key(_identity_key, _key_id) do
    {:ok, {1, "stub_signed_pre_key", "stub_signature"}}
  end

  @doc """
  Stub for create_session/2
  """
  def create_session(_local_identity_key, _remote_identity_key) do
    {:ok, make_ref()}
  end

  @doc """
  Stub for process_pre_key_bundle/2
  """
  def process_pre_key_bundle(_session, _bundle) do
    :ok
  end

  @doc """
  Stub for encrypt_message/2
  """
  def encrypt_message(_session, _message) do
    {:ok, "stub_encrypted_message"}
  end

  @doc """
  Stub for decrypt_message/2
  """
  def decrypt_message(_session, _ciphertext) do
    {:ok, "stub_decrypted_message"}
  end
end
