defmodule LibsignalProtocol do
  @moduledoc """
  Elixir wrapper for the Signal Protocol library.
  Provides a clean, idiomatic interface for secure messaging.
  """

  # Suppress warnings for undefined NIF functions during development
  @compile {:no_warn_undefined, {:libsignal_protocol_nif, :init, 0}}
  @compile {:no_warn_undefined, {:libsignal_protocol_nif, :create_session, 1}}
  @compile {:no_warn_undefined, {:libsignal_protocol_nif, :create_session, 2}}
  @compile {:no_warn_undefined, {:libsignal_protocol_nif, :generate_identity_key_pair, 0}}

  # No @on_load - we'll use the existing Erlang NIF module directly

  @doc """
  Initializes the Signal Protocol library.
  Returns `:ok` on success or `{:error, reason}` on failure.
  """
  @spec init() :: :ok | {:error, String.t()}
  def init do
    try do
      # Ensure the NIF module is loaded first
      :code.ensure_loaded(:libsignal_protocol_nif)

      case :libsignal_protocol_nif.init() do
        :ok -> :ok
      end
    rescue
      UndefinedFunctionError ->
        {:error, "NIF not loaded - libsignal_protocol_nif.init/0 not found"}
    catch
      :error, :undef ->
        {:error, "NIF not loaded - function undefined"}
    end
  end

  @doc """
  Creates a new session for a recipient using a public key.
  Returns `{:ok, session}` on success or `{:error, reason}` on failure.
  """
  @spec create_session(binary()) :: {:ok, binary()} | {:error, String.t()}
  def create_session(public_key) when is_binary(public_key) do
    try do
      case :libsignal_protocol_nif.create_session(public_key) do
        {:ok, session} -> {:ok, session}
      end
    rescue
      UndefinedFunctionError ->
        {:error, "NIF function create_session/1 not found"}
    catch
      :error, :undef ->
        {:error, "NIF function create_session/1 undefined"}
    end
  end

  @doc """
  Creates a new session using local private key and remote public key.
  Returns `{:ok, session}` on success or `{:error, reason}` on failure.
  """
  @spec create_session(binary(), binary()) :: {:ok, binary()} | {:error, String.t()}
  def create_session(local_private_key, remote_public_key)
      when is_binary(local_private_key) and is_binary(remote_public_key) do
    try do
      case :libsignal_protocol_nif.create_session(local_private_key, remote_public_key) do
        {:ok, session} -> {:ok, session}
      end
    rescue
      UndefinedFunctionError ->
        {:error, "NIF function create_session/2 not found"}
    catch
      :error, :undef ->
        {:error, "NIF function create_session/2 undefined"}
    end
  end

  @doc """
  Generates a new identity key pair.
  Returns `{:ok, {public_key, signature}}` on success.
  """
  @spec generate_identity_key_pair() :: {:ok, {binary(), binary()}} | {:error, String.t()}
  def generate_identity_key_pair do
    try do
      case :libsignal_protocol_nif.generate_identity_key_pair() do
        {:ok, {public_key, signature}} -> {:ok, {public_key, signature}}
      end
    rescue
      UndefinedFunctionError ->
        {:error, "NIF function generate_identity_key_pair/0 not found"}
    catch
      :error, :undef ->
        {:error, "NIF function generate_identity_key_pair/0 undefined"}
    end
  end

  @doc """
  Encrypts a message using the given session.
  Returns `{:ok, ciphertext}` on success.
  """
  @spec encrypt_message(binary(), binary()) :: {:ok, binary()} | {:error, String.t()}
  def encrypt_message(session, message) when is_binary(session) and is_binary(message) do
    try do
      case :libsignal_protocol_nif.encrypt_message(session, message) do
        {:ok, ciphertext} -> {:ok, ciphertext}
      end
    rescue
      UndefinedFunctionError ->
        {:error, "NIF function encrypt_message/2 not found"}
    catch
      :error, :undef ->
        {:error, "NIF function encrypt_message/2 undefined"}
    end
  end

  @doc """
  Decrypts a message using the given session.
  Returns `{:ok, plaintext}` on success.
  """
  @spec decrypt_message(binary(), binary()) :: {:ok, binary()} | {:error, String.t()}
  def decrypt_message(session, ciphertext) when is_binary(session) and is_binary(ciphertext) do
    try do
      case :libsignal_protocol_nif.decrypt_message(session, ciphertext) do
        {:ok, plaintext} -> {:ok, plaintext}
      end
    rescue
      UndefinedFunctionError ->
        {:error, "NIF function decrypt_message/2 not found"}
    catch
      :error, :undef ->
        {:error, "NIF function decrypt_message/2 undefined"}
    end
  end
end
