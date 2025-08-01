defmodule Spacecast.Transformations.NormalizeEmail do
  @moduledoc """
  Transformation that normalizes email addresses to lowercase.

  This is a simple example transformation that demonstrates the
  Transformation system. It converts any email address fields
  to lowercase for consistency.
  """

  use Spacecast.Utils.Transformation

  @doc """
  Transforms a resource by normalizing its email address to lowercase.

  ## Parameters

  - resource: The resource to transform
  - context: Additional context for the transformation

  ## Returns

  - `{:ok, transformed_resource}` with the email normalized to lowercase
  - `{:error, reason}` if there was an error during transformation
  """
  @impl true
  def transform(resource, _context) do
    if Map.has_key?(resource, :email) && resource.email do
      # Normalize email to lowercase
      email = String.downcase(resource.email)

      # Return the transformed resource
      {:ok, %{resource | email: email}}
    else
      # No email field or nil email, return unchanged
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
    # Only apply to resources with an email field that is not nil
    Map.has_key?(resource, :email) && resource.email != nil
  end

  @doc """
  Provides information about this transformation.

  ## Returns

  A map with metadata about the transformation.
  """
  @impl true
  def info do
    %{
      name: "Normalize Email",
      description: "Converts email addresses to lowercase for consistency",
      version: "1.0",
      categories: [:email, :normalization],
      metadata: %{
        priority: :high
      }
    }
  end
end
