defmodule Spacecast.Transformations.GenerateSlug do
  @moduledoc """
  Transformation that generates SEO-friendly slugs from a title field.

  This transformation creates a URL-friendly slug from a title,
  ensuring it's unique by checking against existing slugs in the database.
  It's useful for creating readable URLs for content pages.

  ## Example

  ```elixir
  # Register the transformation
  TransformationRegistry.register(GenerateSlug)

  # Apply to a resource with a title
  resource = %{
    title: "Hello, World! This is a Test"
  }

  # With slug_exists? function in context to check for duplicates
  context = %{
    slug_exists?: fn slug -> slug == "hello-world" end
  }

  # After transformation:
  # resource.slug = "hello-world-this-is-a-test"
  ```
  """

  use Spacecast.Utils.Transformation

  @doc """
  Transforms a resource by generating a slug from the title.

  ## Parameters

  - resource: The resource to transform
  - context: Additional context for the transformation
             Can include a `slug_exists?` function to check for duplicates

  ## Returns

  - `{:ok, transformed_resource}` with the generated slug
  - `{:error, reason}` if there was an error during transformation
  """
  @impl true
  def transform(resource, context) do
    # Generate the base slug from the title
    base_slug = generate_base_slug(resource.title)

    # Ensure slug uniqueness if a checker function is provided
    slug =
      if is_function(context[:slug_exists?], 1) do
        ensure_unique_slug(base_slug, context[:slug_exists?])
      else
        base_slug
      end

    # Return the transformed resource with the slug
    {:ok, Map.put(resource, :slug, slug)}
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
    # Apply if resource has a title but no slug (or empty slug)
    Map.has_key?(resource, :title) &&
      is_binary(resource.title) &&
      resource.title != "" &&
      (!Map.has_key?(resource, :slug) || is_nil(resource.slug) || resource.slug == "")
  end

  @doc """
  Provides information about this transformation.

  ## Returns

  A map with metadata about the transformation.
  """
  @impl true
  def info do
    %{
      name: "Generate Slug",
      description: "Generates SEO-friendly slugs from title fields",
      version: "1.0",
      categories: [:content, :seo],
      metadata: %{
        priority: :medium
      }
    }
  end

  # Private helper functions

  # Generate the base slug from a title
  defp generate_base_slug(title) do
    title
    |> String.downcase()
    # Remove special characters
    |> String.replace(~r/[^\w\s-]/, "")
    # Replace spaces with hyphens
    |> String.replace(~r/\s+/, "-")
    # Replace multiple hyphens with single
    |> String.replace(~r/-+/, "-")
    # Trim leading/trailing hyphens
    |> String.trim("-")
  end

  # Ensure the slug is unique by appending a number if needed
  defp ensure_unique_slug(slug, exists_fn) do
    if exists_fn.(slug) do
      # Add incrementing numbers until a unique slug is found
      find_unique_slug(slug, exists_fn, 1)
    else
      slug
    end
  end

  # Recursively try to find a unique slug
  defp find_unique_slug(base_slug, exists_fn, counter) do
    candidate = "#{base_slug}-#{counter}"

    if exists_fn.(candidate) do
      # Try the next number
      find_unique_slug(base_slug, exists_fn, counter + 1)
    else
      candidate
    end
  end
end
