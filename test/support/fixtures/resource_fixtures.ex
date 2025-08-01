defmodule Spacecast.ResourceSystemFixtures do
  @moduledoc """
  This module contains test fixtures for the resource system.
  """

  alias Spacecast.Resources.ResourceSystem

  @doc """
  Creates a test resource with the given attributes and options.
  """
  def resource_fixture(attrs \\ %{}, opts \\ []) do
    attrs = Map.put_new(attrs, :content, %{text: "Test content"})
    ResourceSystem.create_resource(attrs, opts)
  end
end

defmodule Spacecast.TestSupport.ResourceFixtures do
  @moduledoc """
  Test helpers for creating resource entities for tests.
  """
  alias Spacecast.ResourceSystemFixtures

  @doc """
  Create a test resource. Accepts optional attrs map and options.
  Returns {:ok, resource} on success, {:error, changeset} on failure.
  """
  def create_test_resource(attrs \\ %{}, opts \\ []) do
    unique_suffix = System.unique_integer([:positive]) |> Integer.to_string()

    attrs =
      Map.merge(
        %{
          id: unique_suffix,
          name: "Test Resource #{unique_suffix}",
          type: "document",
          content: %{text: "Test content"},
          status: "published",
          description: ""
        },
        attrs
      )

    # Pass options to resource_fixture
    ResourceSystemFixtures.resource_fixture(attrs, opts)
  end

  @doc """
  Create a test user. Accepts optional attrs map.
  Returns {:ok, user} on success, {:error, changeset} on failure.
  """
  def create_user(attrs \\ %{}) do
    %Spacecast.Schemas.User{}
    |> Spacecast.Schemas.User.changeset(attrs)
    |> Spacecast.Repo.insert()
  end
end
