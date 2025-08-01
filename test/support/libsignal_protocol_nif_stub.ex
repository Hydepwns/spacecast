defmodule LibsignalProtocolNifStub do
  @moduledoc """
  Test stub for libsignal_protocol_nif module to eliminate undefined function warnings.
  """

  @doc """
  Stub for init/0
  """
  def init do
    :ok
  end

  @doc """
  Stub for generate_identity_key_pair/0
  """
  def generate_identity_key_pair do
    {:ok, {"stub_public_key", "stub_signature"}}
  end

  @doc """
  Stub for generate_pre_key/1
  """
  def generate_pre_key(key_id) do
    {:ok, {key_id, "stub_pre_key"}}
  end

  @doc """
  Stub for generate_signed_pre_key/2
  """
  def generate_signed_pre_key(_identity_key, key_id) do
    {:ok, {key_id, "stub_signed_pre_key", "stub_signature"}}
  end

  @doc """
  Stub for create_session/1
  """
  def create_session(_recipient_id) do
    {:ok, :mock_session}
  end

  @doc """
  Stub for create_session/2
  """
  def create_session(_local_identity_key, _remote_identity_key) do
    {:ok, :mock_session}
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

  @doc """
  Stub for get_cache_stats/1
  """
  def get_cache_stats(_session) do
    {:ok, %{chain_key_count: 0, root_key_count: 0}}
  end

  @doc """
  Stub for reset_cache_stats/1
  """
  def reset_cache_stats(_session) do
    :ok
  end

  @doc """
  Stub for set_cache_size/3
  """
  def set_cache_size(_session, _chain_key_size, _root_key_size) do
    :ok
  end
end
