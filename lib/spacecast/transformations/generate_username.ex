defmodule Spacecast.Transformations.GenerateUsername do
  @moduledoc """
  Transformation that generates a username from a user's name.

  This transformation demonstrates more advanced functionality by
  generating a username based on a user's name if one doesn't exist.
  It also handles potential conflicts by appending a number if needed.
  """

  use Spacecast.Utils.Transformation

  @doc """
  Transforms a resource by generating a username if one doesn't exist.

  ## Parameters

  - resource: The resource to transform
  - context: Additional context for the transformation

  ## Returns

  - `{:ok, transformed_resource}` with the generated username
  - `{:error, reason}` if there was an error during transformation
  """
  @impl true
  def transform(resource, context) do
    # Only generate username if it doesn't exist and name is present
    if should_generate_username?(resource) do
      # Generate a username from the name
      username = generate_username_from_name(resource.name)

      # Check for conflicts if a conflict checker is provided
      username =
        if context[:username_exists?] do
          ensure_unique_username(username, context[:username_exists?])
        else
          username
        end

      # Return the transformed resource
      {:ok, Map.put(resource, :username, username)}
    else
      # No need to generate username, return unchanged
      {:ok, resource}
    end
  end

  @doc """
  Determines whether this transformation is applicable to the given resource.

  ## Parameters

  - resource: The resource to check
  - context: Additional context for the transformation

  ## Returns

  Boolean indicating whether the transformation should be applied.
  """
  @impl true
  def applicable?(resource, _context) do
    # Only apply to resources with a name field
    Map.has_key?(resource, :name) && resource.name != nil
  end

  @doc """
  Provides information about this transformation.

  ## Returns

  A map with metadata about the transformation.
  """
  @impl true
  def info do
    %{
      name: "Generate Username",
      description: "Generates a username from a user's name if one doesn't exist",
      version: "1.0",
      categories: [:user, :username_generation],
      metadata: %{
        priority: :medium,
        dependencies: [:normalize_email]
      }
    }
  end

  # Private helper functions

  # Determine if a username should be generated
  defp should_generate_username?(resource) do
    # Check if username is missing or empty
    # And name is present
    (not Map.has_key?(resource, :username) || is_nil(resource.username) || resource.username == "") &&
      Map.has_key?(resource, :name) && resource.name != nil && resource.name != ""
  end

  # Generate a username from a name
  defp generate_username_from_name(name) do
    name
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]/, "")
    |> ensure_minimum_length()
  end

  # Ensure the username is at least 3 characters long
  defp ensure_minimum_length(username) do
    if String.length(username) < 3 do
      username <> String.duplicate("0", 3 - String.length(username))
    else
      username
    end
  end

  # Ensure the username is unique by appending a number if needed
  defp ensure_unique_username(username, exists_fn) do
    if exists_fn.(username) do
      # Try adding incrementing numbers until we find a unique username
      Stream.iterate(1, &(&1 + 1))
      |> Enum.reduce_while(username, fn i, username ->
        candidate = "#{username}#{i}"

        if exists_fn.(candidate) do
          {:cont, username}
        else
          {:halt, candidate}
        end
      end)
    else
      username
    end
  end
end
