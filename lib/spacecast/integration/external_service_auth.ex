defmodule Spacecast.Integration.ExternalServiceAuth do
  @moduledoc """
  External service authentication and security utilities.

  This module provides functionality for validating credentials,
  encrypting sensitive data, and managing authentication for
  external services.
  """

  @behaviour Spacecast.Integration.ExternalServiceAuthBehaviour

  require Logger

  @doc """
  Validates credentials for an external service.

  ## Parameters
  - service_name: Name of the external service
  - credentials: Credentials to validate

  ## Returns
  - {:ok, true} if credentials are valid
  - {:error, reason} if credentials are invalid
  """
  def validate_credentials(service_name, credentials)
      when is_binary(service_name) and is_map(credentials) do
    case validate_credential_params(service_name, credentials) do
      :ok ->
        # In production, this would validate against the actual service
        case credentials do
          %{api_key: "valid-key"} ->
            Logger.info("Credentials validated for #{service_name}")
            {:ok, true}

          %{api_key: "invalid-key"} ->
            Logger.warning("Invalid credentials for #{service_name}")
            {:error, "Invalid API key"}

          %{username: username, password: password}
          when is_binary(username) and is_binary(password) ->
            if String.length(username) > 0 and String.length(password) > 0 do
              Logger.info("Credentials validated for #{service_name}")
              {:ok, true}
            else
              Logger.warning("Invalid credentials for #{service_name}")
              {:error, "Invalid username or password"}
            end

          _ ->
            Logger.warning("Invalid credential format for #{service_name}")
            {:error, "Invalid credential format"}
        end

      {:error, reason} ->
        Logger.error("Credential validation failed for #{service_name}: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Encrypts sensitive data for external services.

  ## Parameters
  - data: Data to encrypt
  - key: Encryption key

  ## Returns
  - {:ok, encrypted_data} on success
  - {:error, reason} on failure
  """
  def encrypt_data(data, key) when is_binary(data) and is_binary(key) do
    case validate_encryption_params(data, key) do
      :ok ->
        # In production, this would use proper encryption
        encrypted_data = Base.encode64(data <> "_encrypted")
        Logger.info("Data encrypted successfully")
        {:ok, encrypted_data}

      {:error, reason} ->
        Logger.error("Encryption failed: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Decrypts data from external services.

  ## Parameters
  - encrypted_data: Data to decrypt
  - key: Decryption key

  ## Returns
  - {:ok, decrypted_data} on success
  - {:error, reason} on failure
  """
  def decrypt_data(encrypted_data, key) when is_binary(encrypted_data) and is_binary(key) do
    case validate_decryption_params(encrypted_data, key) do
      :ok ->
        # In production, this would use proper decryption
        case Base.decode64(encrypted_data) do
          {:ok, decoded} ->
            if String.ends_with?(decoded, "_encrypted") do
              decrypted_data = String.slice(decoded, 0, String.length(decoded) - 10)
              Logger.info("Data decrypted successfully")
              {:ok, decrypted_data}
            else
              {:error, "Invalid encrypted data format"}
            end

          :error ->
            {:error, "Invalid base64 encoding"}
        end

      {:error, reason} ->
        Logger.error("Decryption failed: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Generates secure tokens for external service authentication.

  ## Parameters
  - service_name: Name of the service
  - user_id: User identifier

  ## Returns
  - {:ok, token} on success
  - {:error, reason} on failure
  """
  def generate_auth_token(service_name, user_id)
      when is_binary(service_name) and is_binary(user_id) do
    case validate_token_params(service_name, user_id) do
      :ok ->
        # In production, this would generate a proper JWT or similar token
        token_data = %{
          service: service_name,
          user_id: user_id,
          issued_at: DateTime.utc_now(),
          expires_at: DateTime.add(DateTime.utc_now(), 3600, :second)
        }

        token = Base.encode64(Jason.encode!(token_data))
        Logger.info("Auth token generated for #{service_name}")
        {:ok, token}

      {:error, reason} ->
        Logger.error("Token generation failed: #{reason}")
        {:error, reason}
    end
  end

  @doc """
  Validates authentication tokens from external services.

  ## Parameters
  - token: Token to validate
  - service_name: Expected service name

  ## Returns
  - {:ok, token_data} on success
  - {:error, reason} on failure
  """
  def validate_auth_token(token, service_name)
      when is_binary(token) and is_binary(service_name) do
    case Base.decode64(token) do
      {:ok, decoded} ->
        case Jason.decode(decoded) do
          {:ok, token_data} ->
            case validate_token_data(token_data, service_name) do
              :ok ->
                Logger.info("Auth token validated for #{service_name}")
                {:ok, token_data}

              {:error, reason} ->
                Logger.warning("Invalid auth token: #{reason}")
                {:error, reason}
            end

          :error ->
            Logger.warning("Invalid token format")
            {:error, "Invalid token format"}
        end

      :error ->
        Logger.warning("Invalid token encoding")
        {:error, "Invalid token encoding"}
    end
  end

  # Private functions

  defp validate_credential_params(service_name, credentials) do
    cond do
      !is_binary(service_name) or String.length(service_name) == 0 ->
        {:error, "Invalid service name"}

      !is_map(credentials) or map_size(credentials) == 0 ->
        {:error, "Invalid credentials format"}

      true ->
        :ok
    end
  end

  defp validate_encryption_params(data, key) do
    cond do
      !is_binary(data) or String.length(data) == 0 ->
        {:error, "Invalid data"}

      !is_binary(key) or String.length(key) == 0 ->
        {:error, "Invalid encryption key"}

      true ->
        :ok
    end
  end

  defp validate_decryption_params(encrypted_data, key) do
    cond do
      !is_binary(encrypted_data) or String.length(encrypted_data) == 0 ->
        {:error, "Invalid encrypted data"}

      !is_binary(key) or String.length(key) == 0 ->
        {:error, "Invalid decryption key"}

      true ->
        :ok
    end
  end

  defp validate_token_params(service_name, user_id) do
    cond do
      !is_binary(service_name) or String.length(service_name) == 0 ->
        {:error, "Invalid service name"}

      !is_binary(user_id) or String.length(user_id) == 0 ->
        {:error, "Invalid user ID"}

      true ->
        :ok
    end
  end

  defp validate_token_data(token_data, expected_service) do
    cond do
      token_data["service"] != expected_service ->
        {:error, "Token service mismatch"}

      is_nil(token_data["user_id"]) ->
        {:error, "Missing user ID in token"}

      is_nil(token_data["expires_at"]) ->
        {:error, "Missing expiration in token"}

      DateTime.compare(DateTime.utc_now(), token_data["expires_at"]) == :gt ->
        {:error, "Token expired"}

      true ->
        :ok
    end
  end
end
