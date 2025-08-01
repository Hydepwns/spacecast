defmodule Spacecast.RepoHelper do
  @moduledoc """
  Helper module for accessing the configured repo (real or mock).

  This module provides a centralized way to access the database repository,
  allowing for easy switching between real and mock repositories for testing.
  """

  @doc """
  Returns the configured repo module (real Repo or mock).
  """
  @spec repo() :: module()
  def repo do
    Application.get_env(:spacecast, :repo, Spacecast.Repo)
  end

  @doc """
  Convenience function for repo().get/3

  ## Parameters

  - `schema` - The Ecto schema module
  - `id` - The primary key value
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `nil` - Record not found
  - `struct` - The found record

  ## Examples

  ```elixir
  # Get a user by ID
  user = RepoHelper.get(User, 1)

  # Get with timeout
  user = RepoHelper.get(User, 1, timeout: 5000)
  ```
  """
  @spec get(module(), any(), Keyword.t()) :: any() | nil
  def get(schema, id, opts \\ []) do
    repo().get(schema, id, opts)
  end

  @doc """
  Convenience function for repo().get!/2

  ## Parameters

  - `schema` - The Ecto schema module
  - `id` - The primary key value
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `struct` - The found record

  ## Raises

  - `Ecto.QueryError` - If the record is not found

  ## Examples

  ```elixir
  # Get a user by ID (raises if not found)
  user = RepoHelper.get!(User, 1)
  ```
  """
  @spec get!(module(), any(), Keyword.t()) :: any()
  def get!(schema, id, opts \\ []) do
    repo().get!(schema, id, opts)
  end

  @doc """
  Convenience function for repo().get_by/2

  ## Parameters

  - `schema` - The Ecto schema module
  - `clauses` - The where clauses
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `nil` - Record not found
  - `struct` - The found record

  ## Examples

  ```elixir
  # Get a user by email
  user = RepoHelper.get_by(User, email: "test@example.com")

  # Get with timeout
  user = RepoHelper.get_by(User, email: "test@example.com", timeout: 5000)
  ```
  """
  @spec get_by(module(), Keyword.t(), Keyword.t()) :: any() | nil
  def get_by(schema, clauses, opts \\ []) do
    repo().get_by(schema, clauses, opts)
  end

  @doc """
  Convenience function for repo().insert/2

  ## Parameters

  - `struct` - The struct to insert
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `{:ok, struct}` - Insert successful
  - `{:error, changeset}` - Insert failed

  ## Examples

  ```elixir
  # Insert a new user
  {:ok, user} = RepoHelper.insert(%User{name: "John", email: "john@example.com"})
  ```
  """
  @spec insert(any(), Keyword.t()) :: {:ok, any()} | {:error, any()}
  def insert(struct, opts \\ []) do
    repo().insert(struct, opts)
  end

  @doc """
  Convenience function for repo().update/2

  ## Parameters

  - `struct` - The struct to update
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `{:ok, struct}` - Update successful
  - `{:error, changeset}` - Update failed

  ## Examples

  ```elixir
  # Update a user
  {:ok, updated_user} = RepoHelper.update(%{user | name: "Jane"})
  ```
  """
  @spec update(any(), Keyword.t()) :: {:ok, any()} | {:error, any()}
  def update(struct, opts \\ []) do
    repo().update(struct, opts)
  end

  @doc """
  Convenience function for repo().delete/2

  ## Parameters

  - `struct` - The struct to delete
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `{:ok, struct}` - Delete successful
  - `{:error, changeset}` - Delete failed

  ## Examples

  ```elixir
  # Delete a user
  {:ok, deleted_user} = RepoHelper.delete(user)
  ```
  """
  @spec delete(any(), Keyword.t()) :: {:ok, any()} | {:error, any()}
  def delete(struct, opts \\ []) do
    repo().delete(struct, opts)
  end

  @doc """
  Convenience function for repo().all/2

  ## Parameters

  - `queryable` - The queryable (schema or query)
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `[struct]` - List of records

  ## Examples

  ```elixir
  # Get all users
  users = RepoHelper.all(User)

  # Get with timeout
  users = RepoHelper.all(User, timeout: 5000)
  ```
  """
  @spec all(any(), Keyword.t()) :: [any()]
  def all(queryable, opts \\ []) do
    repo().all(queryable, opts)
  end

  @doc """
  Convenience function for repo().one/2

  ## Parameters

  - `queryable` - The queryable (schema or query)
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `nil` - No record found
  - `struct` - The found record

  ## Examples

  ```elixir
  # Get one user
  user = RepoHelper.one(User)

  # Get with timeout
  user = RepoHelper.one(User, timeout: 5000)
  ```
  """
  @spec one(any(), Keyword.t()) :: any() | nil
  def one(queryable, opts \\ []) do
    repo().one(queryable, opts)
  end

  @doc """
  Convenience function for repo().aggregate/3

  ## Parameters

  - `queryable` - The queryable (schema or query)
  - `aggregate` - The aggregate function (:count, :avg, :sum, :min, :max)
  - `field` - The field to aggregate on
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `number` - The aggregate result
  - `nil` - No records found

  ## Examples

  ```elixir
  # Count all users
  count = RepoHelper.aggregate(User, :count, :id)

  # Get average age
  avg_age = RepoHelper.aggregate(User, :avg, :age)
  ```
  """
  @spec aggregate(any(), atom(), atom(), Keyword.t()) :: any()
  def aggregate(queryable, aggregate, field, opts \\ []) do
    repo().aggregate(queryable, aggregate, field, opts)
  end

  @doc """
  Convenience function for repo().exists?/2

  ## Parameters

  - `queryable` - The queryable (schema or query)
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `boolean` - Whether any records exist

  ## Examples

  ```elixir
  # Check if any users exist
  has_users = RepoHelper.exists?(User)

  # Check with conditions
  has_active_users = RepoHelper.exists?(from u in User, where: u.status == "active")
  ```
  """
  @spec exists?(any(), Keyword.t()) :: boolean()
  def exists?(queryable, opts \\ []) do
    repo().exists?(queryable, opts)
  end

  @doc """
  Convenience function for repo().transaction/2

  ## Parameters

  - `fun` - The function to execute in the transaction
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `{:ok, result}` - Transaction successful
  - `{:error, reason}` - Transaction failed

  ## Examples

  ```elixir
  # Execute a transaction
  {:ok, result} = RepoHelper.transaction(fn ->
    # Transaction logic here
    RepoHelper.insert(%User{name: "John"})
  end)
  ```
  """
  @spec transaction(fun(), Keyword.t()) :: {:ok, any()} | {:error, any()}
  def transaction(fun, opts \\ []) do
    repo().transaction(fun, opts)
  end

  @doc """
  Convenience function for repo().rollback/1

  ## Parameters

  - `value` - The value to rollback with

  ## Returns

  - `no_return` - Raises an exception

  ## Examples

  ```elixir
  # Rollback a transaction
  RepoHelper.transaction(fn ->
    case some_condition do
      false -> RepoHelper.rollback(:invalid_data)
      true -> {:ok, result}
    end
  end)
  ```
  """
  @spec rollback(any()) :: no_return()
  def rollback(value) do
    repo().rollback(value)
  end

  @doc """
  Convenience function for repo().delete_all/2

  ## Parameters

  - `queryable` - The queryable (schema or query)
  - `opts` - Optional parameters (default: [])

  ## Returns

  - `{count, nil}` - Number of deleted records

  ## Examples

  ```elixir
  # Delete all users
  {count, nil} = RepoHelper.delete_all(User)

  # Delete with conditions
  {count, nil} = RepoHelper.delete_all(from u in User, where: u.status == "inactive")
  ```
  """
  @spec delete_all(any(), Keyword.t()) :: {non_neg_integer(), nil}
  def delete_all(queryable, opts \\ []) do
    repo().delete_all(queryable, opts)
  end
end
