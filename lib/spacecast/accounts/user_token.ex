defmodule Spacecast.Accounts.UserToken do
  @moduledoc """
  Handles user authentication tokens, including generation, verification, and management
  of session, password reset, and email confirmation tokens.
  """

  use Ecto.Schema
  import Ecto.Query
  alias Spacecast.RepoHelper

  @hash_algorithm :sha256
  @rand_size 32

  # It is very important to keep the reset password token expiry short,
  # since someone with access to the email may take over the account.
  @confirm_validity_in_days 7
  @change_email_validity_in_days 7

  schema "users_tokens" do
    field :token, :binary
    field :context, :string
    field :sent_to, :string
    belongs_to :user, Spacecast.Accounts.User

    timestamps(updated_at: false)
  end

  @doc """
  Generates a token that will be stored in a signed place,
  such as session or cookie. As they are signed, those
  tokens do not need to be hashed.
  """
  def sign_token(user, context) when is_binary(context) do
    Phoenix.Token.sign(SpacecastWeb.Endpoint, context, user.id)
  end

  def sign_token(_invalid_user, _invalid_context), do: {:error, :invalid_parameters}

  @doc """
  Encodes the given token into a URL-safe string.
  """
  def encode_token(token) when is_binary(token) do
    {:ok, Base.url_encode64(token, padding: false)}
  end

  def encode_token(_invalid_token), do: {:error, :invalid_token}

  @doc """
  Decodes the given token from a URL-safe string.
  """
  def decode_token(token) when is_binary(token) do
    case Base.url_decode64(token, padding: false) do
      {:ok, decoded} -> {:ok, decoded}
      :error -> {:error, :invalid_token}
    end
  end

  def decode_token(_invalid_token), do: {:error, :invalid_token}

  @doc """
  Generates a token and its hash to be delivered to the user's email.
  """
  def build_email_token(user, context) when is_binary(context) do
    build_hashed_token(user, context, user.email)
  end

  def build_email_token(_invalid_user, _invalid_context), do: {:error, :invalid_parameters}

  @doc """
  Checks if the token is valid and returns its underlying lookup query.
  """
  def verify_email_token_query(token, context) when is_binary(token) and is_binary(context) do
    query =
      from token in token_and_context_query(token, context),
        join: user in assoc(token, :user),
        where: token.sent_to == user.email,
        where: token.inserted_at > ago(@confirm_validity_in_days, "day")

    {:ok, query}
  end

  def verify_email_token_query(_invalid_token, _invalid_context),
    do: {:error, :invalid_parameters}

  @doc """
  Checks if the token is valid and returns its underlying lookup query.
  """
  def verify_password_reset_token_query(token) when is_binary(token) do
    verify_email_token_query(token, "reset_password")
  end

  def verify_password_reset_token_query(_invalid_token), do: {:error, :invalid_token}

  @doc """
  Checks if the token is valid and returns its underlying lookup query.
  """
  def verify_session_token_query(token) when is_binary(token) do
    verify_token_query(token, "session")
  end

  def verify_session_token_query(_invalid_token), do: {:error, :invalid_token}

  @doc """
  Returns the given token with the given context.
  """
  def token_and_context_query(token, context) when is_binary(token) and is_binary(context) do
    from Spacecast.Accounts.UserToken,
      where: [token: ^token, context: ^context]
  end

  def token_and_context_query(_invalid_token, _invalid_context), do: {:error, :invalid_parameters}

  @doc """
  Gets all tokens for the given user for the given contexts.
  """
  def user_and_contexts_query(user, :all) do
    from t in Spacecast.Accounts.UserToken, where: t.user_id == ^user.id
  end

  def user_and_contexts_query(user, [_ | _] = contexts) do
    from t in Spacecast.Accounts.UserToken,
      where: t.user_id == ^user.id and t.context in ^contexts
  end

  def user_and_contexts_query(_invalid_user, _invalid_contexts), do: {:error, :invalid_parameters}

  @doc """
  Builds a token with a hashed counter part.
  """
  def build_hashed_token(user, context, sent_to) when is_binary(context) and is_binary(sent_to) do
    build_hashed_token(user, context, sent_to, user.email)
  end

  def build_hashed_token(_user, context, sent_to)
      when not (is_binary(context) and is_binary(sent_to)) do
    {:error, :invalid_parameters}
  end

  def build_hashed_token(user, context, sent_to, email)
      when is_binary(context) and is_binary(sent_to) and is_binary(email) do
    token = :crypto.strong_rand_bytes(@rand_size)
    hashed_token = :crypto.hash(@hash_algorithm, token)

    {Base.url_encode64(token, padding: false),
     %Spacecast.Accounts.UserToken{
       token: hashed_token,
       context: context,
       sent_to: sent_to,
       user_id: user.id
     }}
  end

  def build_hashed_token(_user, context, sent_to, email)
      when not (is_binary(context) and is_binary(sent_to) and is_binary(email)) do
    {:error, :invalid_parameters}
  end

  @doc """
  Returns the token struct for the given token value.
  """
  def verify_token_query(token, context) when is_binary(token) and is_binary(context) do
    case Base.url_decode64(token, padding: false) do
      {:ok, decoded_token} ->
        hashed_token = :crypto.hash(@hash_algorithm, decoded_token)
        {:ok, token_and_context_query(hashed_token, context)}

      :error ->
        {:error, :invalid_token}
    end
  end

  def verify_token_query(_invalid_token, _invalid_context), do: {:error, :invalid_parameters}

  @doc """
  Returns the token struct for the given token value.
  """
  def verify_change_email_token_query(token, email) when is_binary(token) and is_binary(email) do
    case Base.url_decode64(token, padding: false) do
      {:ok, decoded_token} ->
        hashed_token = :crypto.hash(@hash_algorithm, decoded_token)

        {:ok,
         from(token in token_and_context_query(hashed_token, "change_email"),
           where:
             token.sent_to == ^email and
               token.inserted_at > ago(@change_email_validity_in_days, "day")
         )}

      :error ->
        {:error, :invalid_token}
    end
  end

  def verify_change_email_token_query(_invalid_token, _invalid_email),
    do: {:error, :invalid_parameters}

  @doc """
  Deletes all tokens for a user.
  """
  def delete_all_tokens(user, :all) do
    from(t in Spacecast.Accounts.UserToken, where: t.user_id == ^user.id)
    |> RepoHelper.delete_all()
  end

  def delete_all_tokens(user, [_ | _] = contexts) do
    from(t in Spacecast.Accounts.UserToken,
      where: t.user_id == ^user.id and t.context in ^contexts
    )
    |> RepoHelper.delete_all()
  end

  def delete_all_tokens(_invalid_user, _invalid_contexts), do: {:error, :invalid_parameters}

  @doc """
  Returns a changeset for the token.
  """
  def changeset(token, attrs \\ %{}) do
    token
    |> Ecto.Changeset.cast(attrs, [:token, :context, :sent_to])
    |> Ecto.Changeset.validate_required([:token, :context])
    |> Ecto.Changeset.foreign_key_constraint(:user_id)
  end
end
