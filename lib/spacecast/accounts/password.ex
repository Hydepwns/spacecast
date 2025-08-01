defmodule Spacecast.Accounts.Password do
  @moduledoc """
  Password hashing and verification using Bcrypt.
  """

  @min_password_length 8
  # Bcrypt's maximum password length
  @max_password_length 72

  @doc """
  Verifies a password against a hash.
  """
  def verify_pass(password, hash)
      when is_binary(password) and byte_size(password) > 0 and
             is_binary(hash) and byte_size(hash) > 0 do
    Bcrypt.verify_pass(password, hash)
  end

  def verify_pass(_invalid_password, _invalid_hash), do: {:error, :invalid_parameters}

  @doc """
  Generates a hash for the given password.
  """
  def hash_password(password) when is_binary(password) do
    case valid_password?(password) do
      true -> {:ok, Bcrypt.hash_pwd_salt(password)}
      false -> {:error, :invalid_password}
    end
  end

  def hash_password(_invalid_password), do: {:error, :invalid_password}

  @doc """
  Returns true if the password is valid, false otherwise.
  """
  def valid_password?(password) when is_binary(password) do
    length = String.length(password)
    has_uppercase = String.match?(password, ~r/[A-Z]/)
    has_lowercase = String.match?(password, ~r/[a-z]/)
    has_number = String.match?(password, ~r/[0-9]/)
    has_special = String.match?(password, ~r/[^A-Za-z0-9]/)

    length >= @min_password_length and
      length <= @max_password_length and
      has_uppercase and
      has_lowercase and
      has_number and
      has_special
  end

  def valid_password?(_invalid_password), do: false

  @doc """
  Returns true if the password is invalid, false otherwise.
  """
  def invalid_password?(password) when is_binary(password) do
    not valid_password?(password)
  end

  def invalid_password?(_invalid_password), do: true

  @doc """
  Returns a list of password validation errors.
  """
  def password_errors(password) when is_binary(password) do
    errors =
      if String.length(password) < @min_password_length do
        ["Password must be at least #{@min_password_length} characters long"]
      else
        []
      end

    errors =
      if String.length(password) > @max_password_length do
        ["Password must be at most #{@max_password_length} characters long" | errors]
      else
        errors
      end

    errors =
      if String.match?(password, ~r/[A-Z]/) do
        errors
      else
        ["Password must contain at least one uppercase letter" | errors]
      end

    errors =
      if String.match?(password, ~r/[a-z]/) do
        errors
      else
        ["Password must contain at least one lowercase letter" | errors]
      end

    errors =
      if String.match?(password, ~r/[0-9]/) do
        errors
      else
        ["Password must contain at least one number" | errors]
      end

    errors =
      if String.match?(password, ~r/[^A-Za-z0-9]/) do
        errors
      else
        ["Password must contain at least one special character" | errors]
      end

    errors
  end

  def password_errors(_invalid_password), do: ["Invalid password format"]
end
