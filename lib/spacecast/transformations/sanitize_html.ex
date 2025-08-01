defmodule Spacecast.Transformations.SanitizeHtml do
  @moduledoc """
  Transformation that sanitizes HTML content to prevent XSS attacks.

  This transformation sanitizes any field that may contain HTML content,
  removing potentially dangerous tags and attributes. It's designed to 
  be used on user-generated content before storing or displaying it.

  ## Example

  ```elixir
  # Register the transformation
  TransformationRegistry.register(SanitizeHtml)

  # Apply to a resource with HTML content
  resource = %{
    description: "<script>alert('XSS')</script><p>Hello world</p>"
  }

  # After transformation:
  # resource.description == "<p>Hello world</p>"
  ```
  """

  use Spacecast.Utils.Transformation

  # List of fields that may contain HTML content
  @html_fields [:description, :content, :body, :html]

  @doc """
  Transforms a resource by sanitizing HTML content fields.

  ## Parameters

  - resource: The resource to transform
  - context: Additional context for the transformation

  ## Returns

  - `{:ok, transformed_resource}` with sanitized HTML fields
  - `{:error, reason}` if there was an error during transformation
  """
  @impl true
  def transform(resource, _context) do
    # Get all HTML fields that exist in the resource
    fields_to_sanitize = Enum.filter(@html_fields, &Map.has_key?(resource, &1))

    # Sanitize each field
    sanitized_resource =
      Enum.reduce(fields_to_sanitize, resource, fn field, acc ->
        sanitized_content = sanitize_html(Map.get(acc, field))
        Map.put(acc, field, sanitized_content)
      end)

    {:ok, sanitized_resource}
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
    # Check if any HTML fields exist in the resource
    Enum.any?(@html_fields, fn field ->
      Map.has_key?(resource, field) &&
        is_binary(Map.get(resource, field)) &&
        Map.get(resource, field) != ""
    end)
  end

  @doc """
  Provides information about this transformation.

  ## Returns

  A map with metadata about the transformation.
  """
  @impl true
  def info do
    %{
      name: "Sanitize HTML",
      description: "Sanitizes HTML content to prevent XSS attacks",
      version: "1.0",
      categories: [:security, :content],
      metadata: %{
        priority: :high
      }
    }
  end

  # Private helper functions

  # Sanitize HTML content
  defp sanitize_html(nil), do: nil
  defp sanitize_html(""), do: ""

  defp sanitize_html(html) when is_binary(html) do
    # This is a simplified version. In a real application,
    # you would use a proper HTML sanitization library like HtmlSanitizeEx.
    # For demonstration purposes, we're implementing a basic sanitizer.

    # Remove script tags and their content
    html = Regex.replace(~r/<script\b[^<]*(?:(?!<\/script>)<[^<]*)*<\/script>/i, html, "")

    # Remove on* attributes (event handlers)
    html = Regex.replace(~r/\s+on\w+\s*=\s*("[^"]*"|'[^']*')/i, html, "")

    # Remove javascript: protocol
    html = Regex.replace(~r/javascript:[^\s"']+/i, html, "")

    # This is where you would use a proper HTML parser and sanitizer
    # For this example, we're just returning the result of our simple regex sanitization
    html
  end

  defp sanitize_html(other), do: other
end
