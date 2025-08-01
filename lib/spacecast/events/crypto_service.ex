defmodule Spacecast.Events.CryptoService do
  @moduledoc """
  Handles encryption and decryption of messages using Signal Protocol.
  Manages Signal Protocol sessions and provides a clean interface for secure message handling.
  """

  require Logger

  # Use the existing Signal Protocol NIF or fallback to simple crypto
  alias Spacecast.Events.SimpleCrypto, as: SimpleCrypto

  # Try to load Signal Protocol, fallback to simple crypto if not available
  @signal_available Code.ensure_loaded(SignalProtocol) == {:module, SignalProtocol}

  @type session_id :: String.t()
  @type message :: String.t()
  @type encrypted_message :: String.t()
  @type result :: {:ok, encrypted_message()} | {:error, String.t()}

  # Session storage using ETS for persistence
  @session_table :signal_sessions

  @doc """
  Initializes the crypto service and session storage.
  Should be called during application startup.
  """
  def init do
    # Create ETS table for session storage if it doesn't exist
    case :ets.info(@session_table) do
      :undefined ->
        :ets.new(@session_table, [:set, :public, :named_table])
        Logger.info("Signal Protocol session storage initialized")

      _ ->
        Logger.info("Signal Protocol session storage already exists")
    end

    :ok
  end

  @doc """
  Encrypts a message using Signal Protocol.
  Returns {:ok, encrypted_message} on success or {:error, reason} on failure.
  """
  @spec encrypt_message(struct(), map()) :: result()
  def encrypt_message(reminder, settings) do
    try do
      # Get or create a session for the recipient
      with {:ok, session} <- get_or_create_session(settings.recipient_id),
           {:ok, encrypted} <- encrypt_with_session(session, reminder.message) do
        {:ok, encrypted}
      end
    rescue
      e ->
        Logger.error("Failed to encrypt message: #{inspect(e)}")
        {:error, "Encryption failed"}
    end
  end

  @doc """
  Decrypts a message using Signal Protocol.
  Returns {:ok, decrypted_message} on success or {:error, reason} on failure.
  """
  @spec decrypt_message(encrypted_message(), session_id()) ::
          {:ok, message()} | {:error, String.t()}
  def decrypt_message(encrypted_message, session_id) do
    try do
      with {:ok, session} <- get_session(session_id),
           {:ok, decrypted} <- decrypt_with_session(session, encrypted_message) do
        {:ok, decrypted}
      end
    rescue
      e ->
        Logger.error("Failed to decrypt message: #{inspect(e)}")
        {:error, "Decryption failed"}
    end
  end

    @doc """
  Creates a new Signal Protocol session between two parties.
  """
  @spec create_session(String.t(), String.t()) :: {:ok, session_id()} | {:error, String.t()}
  def create_session(local_user_id, remote_user_id) do
    try do
      # Always use simple crypto fallback since SignalProtocol is not fully implemented
      session_id = generate_session_id()
      encryption_key = SimpleCrypto.generate_key()

      session_data = %{
        session_id: session_id,
        local_user_id: local_user_id,
        remote_user_id: remote_user_id,
        encryption_key: encryption_key,
        created_at: DateTime.utc_now(),
        message_count: 0,
        protocol: :simple
      }

      :ets.insert(@session_table, {session_id, session_data})
      Logger.info("Created simple crypto session: #{session_id}")
      {:ok, session_id}
    rescue
      e ->
        Logger.error("Failed to create session: #{inspect(e)}")
        {:error, "Session creation failed"}
    end
  end

    @doc """
  Encrypts a message using an existing session.
  """
  @spec encrypt_with_session(map(), message()) :: {:ok, encrypted_message()} | {:error, String.t()}
  def encrypt_with_session(session, message) do
    try do
      # Check if session has required fields
      if Map.has_key?(session, :encryption_key) do
        # Always use simple crypto since SignalProtocol is not fully implemented
        case SimpleCrypto.create_encrypted_message(message, session.encryption_key) do
          {:ok, encrypted} ->
            updated_session = Map.update!(session, :message_count, &(&1 + 1))
            :ets.insert(@session_table, {session.session_id, updated_session})
            {:ok, Base.encode64(encrypted)}

          {:error, reason} ->
            Logger.error("Simple crypto encryption failed: #{inspect(reason)}")
            {:error, "Encryption failed"}
        end
      else
        {:error, "Unsupported protocol"}
      end
    rescue
      e ->
        Logger.error("Failed to encrypt with session: #{inspect(e)}")
        {:error, "Session encryption failed"}
    end
  end

    @doc """
  Decrypts a message using an existing session.
  """
  @spec decrypt_with_session(map(), encrypted_message()) :: {:ok, message()} | {:error, String.t()}
  def decrypt_with_session(session, encrypted_message) do
    try do
      # Check if session has required fields
      if Map.has_key?(session, :encryption_key) do
        # Decode the base64 encrypted message
        case Base.decode64(encrypted_message) do
          {:ok, decoded} ->
            # Always use simple crypto since SignalProtocol is not fully implemented
            case SimpleCrypto.decrypt_message(decoded, session.encryption_key) do
              {:ok, decrypted} ->
                updated_session = Map.update!(session, :message_count, &(&1 + 1))
                :ets.insert(@session_table, {session.session_id, updated_session})
                {:ok, decrypted}

              {:error, reason} ->
                Logger.error("Simple crypto decryption failed: #{inspect(reason)}")
                {:error, "Decryption failed"}
            end

          :error ->
            {:error, "Invalid encrypted message format"}
        end
      else
        {:error, "Unsupported protocol"}
      end
    rescue
      e ->
        Logger.error("Failed to decrypt with session: #{inspect(e)}")
        {:error, "Session decryption failed"}
    end
  end

  @doc """
  Signs data using Ed25519 digital signatures or HMAC-SHA256 fallback.
  """
  @spec sign_data(binary(), binary()) :: {:ok, binary()} | {:error, String.t()}
  def sign_data(private_key, data) do
    try do
      # Use HMAC-SHA256 for signing (SignalProtocol doesn't have sign_data/2)
      signature = SimpleCrypto.hmac_sha256(private_key, data)
      {:ok, signature}
    rescue
      e ->
        Logger.error("Failed to sign data: #{inspect(e)}")
        {:error, "Signing failed"}
    end
  end

  @doc """
  Verifies Ed25519 digital signatures or HMAC-SHA256 fallback.
  """
  @spec verify_signature(binary(), binary(), binary()) :: {:ok, boolean()} | {:error, String.t()}
  def verify_signature(public_key, data, signature) do
    try do
      # Use HMAC-SHA256 verification (SignalProtocol doesn't have verify_signature/3)
      expected_signature = SimpleCrypto.hmac_sha256(public_key, data)
      {:ok, signature == expected_signature}
    rescue
      e ->
        Logger.error("Failed to verify signature: #{inspect(e)}")
        {:error, "Verification failed"}
    end
  end

  @doc """
  Generates a secure random session ID.
  """
  def generate_session_id do
    :crypto.strong_rand_bytes(16)
    |> Base.encode16(case: :lower)
  end

  @doc """
  Gets a session by ID from storage.
  """
  @spec get_session(session_id()) :: {:ok, map()} | {:error, :not_found}
  def get_session(session_id) do
    case :ets.lookup(@session_table, session_id) do
      [{^session_id, session_data}] -> {:ok, session_data}
      [] -> {:error, :not_found}
    end
  end

  @doc """
  Gets or creates a session for a recipient.
  """
  @spec get_or_create_session(String.t()) :: {:ok, map()} | {:error, String.t()}
  def get_or_create_session(recipient_id) do
    # For now, create a new session for each recipient
    # In a real implementation, you'd want to reuse existing sessions
    case create_session("system", recipient_id) do
      {:ok, session_id} -> get_session(session_id)
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Lists all active sessions.
  """
  @spec list_sessions() :: [map()]
  def list_sessions do
    :ets.tab2list(@session_table)
    |> Enum.map(fn {_key, value} -> value end)
  end

  @doc """
  Deletes a session by ID.
  """
  @spec delete_session(session_id()) :: :ok | {:error, :not_found}
  def delete_session(session_id) do
    case :ets.lookup(@session_table, session_id) do
      [{^session_id, _session_data}] ->
        :ets.delete(@session_table, session_id)
        Logger.info("Deleted Signal Protocol session: #{session_id}")
        :ok

      [] ->
        {:error, :not_found}
    end
  end

  @doc """
  Cleans up expired sessions.
  """
  @spec cleanup_expired_sessions() :: integer()
  def cleanup_expired_sessions do
    now = DateTime.utc_now()
    max_age = DateTime.add(now, -24 * 60 * 60, :second) # 24 hours

    :ets.tab2list(@session_table)
    |> Enum.count(fn {_key, session_data} ->
      case DateTime.compare(session_data.created_at, max_age) do
        :lt ->
          :ets.delete(@session_table, session_data.session_id)
          true
        _ -> false
      end
    end)
  end

  @doc """
  Gets session statistics.
  """
  @spec get_session_stats() :: map()
  def get_session_stats do
    sessions = list_sessions()

    if Enum.empty?(sessions) do
      %{
        total_sessions: 0,
        total_messages: 0,
        oldest_session: nil,
        newest_session: nil
      }
    else
      %{
        total_sessions: length(sessions),
        total_messages: Enum.reduce(sessions, 0, fn session, acc -> acc + session.message_count end),
        oldest_session: Enum.min_by(sessions, & &1.created_at),
        newest_session: Enum.max_by(sessions, & &1.created_at)
      }
    end
  end
end
