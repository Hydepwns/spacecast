defmodule Spacecast.Events.SimpleCrypto do
  @moduledoc """
  Simple cryptographic operations using Erlang's crypto module.
  Provides basic encryption/decryption as a fallback when Signal Protocol is not available.
  """

  require Logger

  @type key :: binary()
  @type iv :: binary()
  @type encrypted_data :: binary()
  @type plaintext :: binary()

  @doc """
  Generates a random encryption key.
  """
  @spec generate_key() :: key()
  def generate_key do
    :crypto.strong_rand_bytes(32)
  end

  @doc """
  Generates a random initialization vector.
  """
  @spec generate_iv() :: iv()
  def generate_iv do
    :crypto.strong_rand_bytes(16)
  end

  @doc """
  Encrypts data using AES-256-CBC.
  """
  @spec encrypt(plaintext(), key(), iv()) :: {:ok, encrypted_data()} | {:error, String.t()}
  def encrypt(plaintext, key, iv) when is_binary(plaintext) and is_binary(key) and is_binary(iv) do
    try do
      # Pad the plaintext to block size
      padded_plaintext = pad_plaintext(plaintext)

      # Encrypt using AES-256-CBC
      encrypted = :crypto.crypto_one_time(:aes_256_cbc, key, iv, padded_plaintext, true)
      {:ok, encrypted}
    rescue
      e ->
        Logger.error("Encryption failed: #{inspect(e)}")
        {:error, "Encryption failed"}
    end
  end

  def encrypt(_plaintext, _key, _iv) do
    {:error, "Invalid parameters"}
  end

  @doc """
  Decrypts data using AES-256-CBC.
  """
  @spec decrypt(encrypted_data(), key(), iv()) :: {:ok, plaintext()} | {:error, String.t()}
  def decrypt(encrypted_data, key, iv) when is_binary(encrypted_data) and is_binary(key) and is_binary(iv) do
    try do
      # Decrypt using AES-256-CBC
      decrypted = :crypto.crypto_one_time(:aes_256_cbc, key, iv, encrypted_data, false)

      # Remove padding
      case unpad_plaintext(decrypted) do
        {:ok, plaintext} -> {:ok, plaintext}
        {:error, reason} -> {:error, reason}
      end
    rescue
      e ->
        Logger.error("Decryption failed: #{inspect(e)}")
        {:error, "Decryption failed"}
    end
  end

  def decrypt(_encrypted_data, _key, _iv) do
    {:error, "Invalid parameters"}
  end

  @doc """
  Encrypts data with a generated key and IV.
  Returns the encrypted data, key, and IV.
  """
  @spec encrypt_with_generated_key(plaintext()) :: {:ok, encrypted_data(), key(), iv()} | {:error, String.t()}
  def encrypt_with_generated_key(plaintext) do
    key = generate_key()
    iv = generate_iv()

    case encrypt(plaintext, key, iv) do
      {:ok, encrypted} -> {:ok, encrypted, key, iv}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Creates a simple encrypted message format that includes the IV.
  """
  @spec create_encrypted_message(plaintext(), key()) :: {:ok, binary()} | {:error, String.t()}
  def create_encrypted_message(plaintext, key) do
    iv = generate_iv()

    case encrypt(plaintext, key, iv) do
      {:ok, encrypted} ->
        # Format: IV (16 bytes) + encrypted data
        message = iv <> encrypted
        {:ok, message}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Decrypts a message in the simple format (IV + encrypted data).
  """
  @spec decrypt_message(binary(), key()) :: {:ok, plaintext()} | {:error, String.t()}
  def decrypt_message(message, key) when byte_size(message) > 16 do
    iv = binary_part(message, 0, 16)
    encrypted_data = binary_part(message, 16, byte_size(message) - 16)

    decrypt(encrypted_data, key, iv)
  end

  def decrypt_message(_message, _key) do
    {:error, "Message too short"}
  end

  @doc """
  Generates a SHA-256 hash of the input data.
  """
  @spec sha256(binary()) :: binary()
  def sha256(data) do
    :crypto.hash(:sha256, data)
  end

  @doc """
  Generates an HMAC-SHA256 of the input data.
  """
  @spec hmac_sha256(binary(), binary()) :: binary()
  def hmac_sha256(key, data) do
    :crypto.mac(:hmac, :sha256, key, data)
  end

  @doc """
  Generates a secure random string.
  """
  @spec random_string(integer()) :: String.t()
  def random_string(length) do
    :crypto.strong_rand_bytes(length)
    |> Base.encode64()
    |> binary_part(0, length)
  end

  # Private functions

  defp pad_plaintext(plaintext) do
    block_size = 16
    padding_length = block_size - rem(byte_size(plaintext), block_size)
    padding = List.duplicate(padding_length, padding_length) |> :binary.list_to_bin()
    plaintext <> padding
  end

  defp unpad_plaintext(padded_plaintext) do
    case byte_size(padded_plaintext) do
      0 ->
        {:error, "Empty plaintext"}

      size ->
        padding_length = binary_part(padded_plaintext, size - 1, 1) |> :binary.bin_to_list() |> hd()

        if padding_length > 0 and padding_length <= 16 do
          plaintext_size = size - padding_length

          if plaintext_size >= 0 do
            plaintext = binary_part(padded_plaintext, 0, plaintext_size)
            {:ok, plaintext}
          else
            {:error, "Invalid padding"}
          end
        else
          {:error, "Invalid padding length"}
        end
    end
  end
end
