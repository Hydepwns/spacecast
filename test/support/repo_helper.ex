defmodule Spacecast.TestRepoHelper do
  @moduledoc """
  Helper module for accessing the configured repo (real or mock) in tests.
  """

  @doc """
  Returns the configured repo module (real Repo or mock).
  """
  def repo do
    Application.get_env(:spacecast, :repo, Spacecast.Repo)
  end

  @doc """
  Convenience function for repo().get/3
  """
  def get(schema, id, opts \\ []) do
    repo().get(schema, id, opts)
  end

  @doc """
  Convenience function for repo().get!/2
  """
  def get!(schema, id, opts \\ []) do
    repo().get!(schema, id, opts)
  end

  @doc """
  Convenience function for repo().get_by/2
  """
  def get_by(schema, clauses, opts \\ []) do
    repo().get_by(schema, clauses, opts)
  end
end
