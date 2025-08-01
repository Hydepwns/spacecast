defmodule Spacecast.TestRepoBehaviour do
  @moduledoc """
  Behaviour for Repo functions that can be mocked in tests.
  """

  @callback get(module, any, Keyword.t()) :: any
  @callback get!(module, any, Keyword.t()) :: any
  @callback get_by(module, Keyword.t(), Keyword.t()) :: any
  @callback get_by(module, Keyword.t()) :: any
  @callback insert(Ecto.Changeset.t()) :: {:ok, any} | {:error, Ecto.Changeset.t()}
  @callback insert(Ecto.Changeset.t(), Keyword.t()) :: {:ok, any} | {:error, Ecto.Changeset.t()}
  @callback update(Ecto.Changeset.t()) :: {:ok, any} | {:error, Ecto.Changeset.t()}
  @callback update(Ecto.Changeset.t(), Keyword.t()) :: {:ok, any} | {:error, Ecto.Changeset.t()}
  @callback delete(any) :: {:ok, any} | {:error, Ecto.Changeset.t()}
  @callback delete(any, Keyword.t()) :: {:ok, any} | {:error, Ecto.Changeset.t()}
  @callback delete_all(module) :: {integer, nil}
  @callback delete_all(module, Keyword.t()) :: {integer, nil}
  @callback all(module) :: [any]
  @callback all(module, Keyword.t()) :: [any]
  @callback one(module) :: any
  @callback one(module, Keyword.t()) :: any
  @callback aggregate(module, atom, atom) :: integer
  @callback aggregate(module, atom, atom, Keyword.t()) :: integer
  @callback exists?(module) :: boolean
  @callback exists?(module, Keyword.t()) :: boolean
  @callback transaction((-> any)) :: {:ok, any} | {:error, any}
  @callback transaction((-> any), Keyword.t()) :: {:ok, any} | {:error, any}
  @callback rollback(any) :: no_return
end
