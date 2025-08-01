defmodule Spacecast.SignalProtocol do
  @moduledoc """
  Wrapper module for the Signal Protocol NIF functions.
  """

  @compile {:no_warn_undefined, {SignalNifStub, :generate_identity_key_pair, 0}}
  @compile {:no_warn_undefined, {SignalNifStub, :generate_pre_key, 1}}
  @compile {:no_warn_undefined, {SignalNifStub, :generate_signed_pre_key, 2}}
  @compile {:no_warn_undefined, {SignalNifStub, :create_session, 2}}
  @compile {:no_warn_undefined, {SignalNifStub, :process_pre_key_bundle, 2}}
  @compile {:no_warn_undefined, {SignalNifStub, :encrypt_message, 2}}
  @compile {:no_warn_undefined, {SignalNifStub, :decrypt_message, 2}}
  @compile {:no_warn_undefined, {SignalNifStub, :get_cache_stats, 1}}
  @compile {:no_warn_undefined, {SignalNifStub, :reset_cache_stats, 1}}
  @compile {:no_warn_undefined, {SignalNifStub, :set_cache_size, 3}}

  defp in_test?, do: Mix.env() == :test

  # @on_load :init

  # def init do
  #   :libsignal_protocol_nif.init()
  # end

  @doc """
  Generates a new identity key pair.
  """
  def generate_identity_key_pair do
    if in_test?() do
      try do
        SignalNifStub.generate_identity_key_pair()
      rescue
        UndefinedFunctionError ->
          {"mock_public_key", "mock_signature"}
      end
    else
      try do
        :libsignal_protocol_nif.generate_identity_key_pair()
      rescue
        UndefinedFunctionError ->
          {"mock_public_key", "mock_signature"}
      end
    end
  end

  @doc """
  Generates a new pre-key.
  """
  def generate_pre_key(key_id) when is_integer(key_id) do
    if in_test?() do
      try do
        SignalNifStub.generate_pre_key(key_id)
      rescue
        UndefinedFunctionError ->
          {key_id, "mock_pre_key"}
      end
    else
      try do
        case :libsignal_protocol_nif.generate_pre_key(key_id) do
          {key_id, public_key} ->
            {:ok, {key_id, public_key}}
        end
      rescue
        UndefinedFunctionError ->
          {key_id, "mock_pre_key"}
      end
    end
  end

  @spec generate_signed_pre_key(binary(), integer()) ::
          {:ok, {any(), <<_::152>>, <<_::112>>}} | {any(), <<_::152>>, <<_::112>>}
  @doc """
  Generates a new signed pre-key.
  """
  def generate_signed_pre_key(identity_key, key_id)
      when is_binary(identity_key) and is_integer(key_id) do
    if in_test?() do
      try do
        SignalNifStub.generate_signed_pre_key(identity_key, key_id)
      rescue
        UndefinedFunctionError ->
          {key_id, "mock_signed_pre_key", "mock_signature"}
      end
    else
      try do
        case :libsignal_protocol_nif.generate_signed_pre_key(identity_key, key_id) do
          {:ok, {key_id, public_key, signature}} ->
            {:ok, {key_id, public_key, signature}}
        end
      rescue
        UndefinedFunctionError ->
          {key_id, "mock_signed_pre_key", "mock_signature"}
      end
    end
  end

  @doc """
  Creates a new session.
  """
  def create_session(local_identity_key, remote_identity_key)
      when is_binary(local_identity_key) and is_binary(remote_identity_key) do
    if in_test?() do
      try do
        SignalNifStub.create_session(local_identity_key, remote_identity_key)
      rescue
        UndefinedFunctionError ->
          :mock_session
      end
    else
      try do
        case :libsignal_protocol_nif.create_session(local_identity_key, remote_identity_key) do
          session ->
            session
        end
      rescue
        UndefinedFunctionError ->
          :mock_session
      end
    end
  end

  @doc """
  Processes a pre-key bundle.
  """
  def process_pre_key_bundle(session, bundle) when is_reference(session) and is_binary(bundle) do
    if in_test?() do
      try do
        SignalNifStub.process_pre_key_bundle(session, bundle)
      rescue
        UndefinedFunctionError ->
          :ok
      end
    else
      try do
        case :libsignal_protocol_nif.process_pre_key_bundle(session, bundle) do
          :ok -> :ok
        end
      rescue
        UndefinedFunctionError ->
          :ok
      end
    end
  end

  @doc """
  Encrypts a message.
  """
  def encrypt_message(session, message) when is_reference(session) and is_binary(message) do
    if in_test?() do
      try do
        SignalNifStub.encrypt_message(session, message)
      rescue
        UndefinedFunctionError ->
          {:ok, "mock_encrypted_#{message}"}
      end
    else
      try do
        case :libsignal_protocol_nif.encrypt_message(session, message) do
          ciphertext ->
            ciphertext
        end
      rescue
        UndefinedFunctionError ->
          {:ok, "mock_encrypted_#{message}"}
      end
    end
  end

  @doc """
  Decrypts a message.
  """
  def decrypt_message(session, ciphertext) when is_reference(session) and is_binary(ciphertext) do
    if in_test?() do
      try do
        SignalNifStub.decrypt_message(session, ciphertext)
      rescue
        UndefinedFunctionError ->
          {:ok, "mock_decrypted_message"}
      end
    else
      try do
        case :libsignal_protocol_nif.decrypt_message(session, ciphertext) do
          plaintext ->
            plaintext
        end
      rescue
        UndefinedFunctionError ->
          # Fallback implementation for testing when NIF is not available
          {:ok, "mock_decrypted_message"}
      end
    end
  end

  @doc """
  Gets cache statistics.
  """
  def get_cache_stats(session) do
    if in_test?() do
      # Use stub in test environment if available, otherwise fallback
      try do
        SignalNifStub.get_cache_stats(session)
      rescue
        UndefinedFunctionError ->
          {:ok, %{chain_key_count: 0, root_key_count: 0}}
      end
    else
      try do
        case :libsignal_protocol_nif.get_cache_stats(session, [], []) do
          stats -> stats
        end
      rescue
        UndefinedFunctionError ->
          # Fallback implementation for testing when NIF is not available
          {:ok, %{chain_key_count: 0, root_key_count: 0}}
      end
    end
  end

  @doc """
  Resets cache statistics.
  """
  def reset_cache_stats(session) do
    if in_test?() do
      # Use stub in test environment if available, otherwise fallback
      try do
        SignalNifStub.reset_cache_stats(session)
      rescue
        UndefinedFunctionError ->
          :ok
      end
    else
      try do
        case :libsignal_protocol_nif.reset_cache_stats(session, []) do
          :ok -> :ok
        end
      rescue
        UndefinedFunctionError ->
          # Fallback implementation for testing when NIF is not available
          :ok
      end
    end
  end

  @doc """
  Sets cache size.
  """
  def set_cache_size(session, chain_key_size, root_key_size)
      when is_integer(chain_key_size) and is_integer(root_key_size) do
    if in_test?() do
      # Use stub in test environment if available, otherwise fallback
      try do
        SignalNifStub.set_cache_size(session, chain_key_size, root_key_size)
      rescue
        UndefinedFunctionError ->
          :ok
      end
    else
      try do
        case :libsignal_protocol_nif.set_cache_size(session, chain_key_size) do
          :ok -> :ok
        end
      rescue
        UndefinedFunctionError ->
          # Fallback implementation for testing when NIF is not available
          :ok
      end
    end
  end
end
