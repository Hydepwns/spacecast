defmodule Spacecast.SignalProtocolStub do
  @moduledoc """
  Stub implementation of SignalProtocol for testing.

  This module provides stub implementations that redirect signal_nif calls
  to the SignalNifStub module to eliminate undefined function warnings.
  """

  # Alias the stub module
  alias SignalNifStub, as: SignalNif

  @spec generate_identity_key_pair() :: {:ok, {<<_::120>>, <<_::112>>}} | {:error, any()}
  @doc """
  Stub for generate_identity_key_pair/0
  """
  def generate_identity_key_pair do
    SignalNif.generate_identity_key_pair()
  end

  @spec generate_pre_key(integer()) :: {:ok, {integer(), <<_::120>>}} | {:error, any()}
  @doc """
  Stub for generate_pre_key/1
  """
  def generate_pre_key(key_id) do
    SignalNif.generate_pre_key(key_id)
  end

  @spec generate_signed_pre_key({<<_::120>>, <<_::112>>}, integer()) :: {:ok, {integer(), <<_::120>>}} | {:error, any()}
  @doc """
  Stub for generate_signed_pre_key/2
  """
  def generate_signed_pre_key(identity_key, key_id) do
    SignalNif.generate_signed_pre_key(identity_key, key_id)
  end

  @spec create_session({<<_::120>>, <<_::112>>}, {<<_::120>>, <<_::112>>}) :: {:ok, {<<_::120>>, <<_::112>>}} | {:error, any()}
  @doc """
  Stub for create_session/2
  """
  def create_session(local_identity_key, remote_identity_key) do
    SignalNif.create_session(local_identity_key, remote_identity_key)
  end

  @spec process_pre_key_bundle({<<_::120>>, <<_::112>>}, {<<_::120>>, <<_::112>>}) :: {:ok, {<<_::120>>, <<_::112>>}} | {:error, any()}
  @doc """
  Stub for process_pre_key_bundle/2
  """
  def process_pre_key_bundle(session, bundle) do
    SignalNif.process_pre_key_bundle(session, bundle)
  end

  @spec encrypt_message({<<_::120>>, <<_::112>>}, binary()) :: {:ok, binary()} | {:error, any()}
  @doc """
  Stub for encrypt_message/2
  """
  def encrypt_message(session, message) do
    SignalNif.encrypt_message(session, message)
  end

  @spec decrypt_message({<<_::120>>, <<_::112>>}, binary()) :: {:ok, binary()} | {:error, any()}
  @doc """
  Stub for decrypt_message/2
  """
  def decrypt_message(session, ciphertext) do
    SignalNif.decrypt_message(session, ciphertext)
  end
end
