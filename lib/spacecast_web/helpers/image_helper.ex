defmodule SpacecastWeb.ImageHelper do
  @moduledoc """
  Helper functions for optimizing image loading and rendering.
  Provides functionality for serving WebP images with fallbacks.
  """
  # Add alias for Phoenix.HTML.Tag
  alias Phoenix.HTML.Tag

  @doc """
  Generates an HTML picture element with WebP and fallback sources.
  Automatically checks if a WebP version exists, otherwise uses only the original.

  ## Examples

      {optimized_image_tag("/images/logo.png", alt: "Logo", class: "header-logo")}

  Will generate:

      <picture>
        <source srcset="/images/logo.webp" type="image/webp">
        <img src="/images/logo.png" alt="Logo" class="header-logo">
      </picture>

  If logo.webp doesn't exist, it will generate just the img tag.
  """
  def optimized_image_tag(image_path, attrs \\ []) do
    # Extract file info
    case extract_path_info(image_path) do
      {path, filename, _ext} ->
        # Construct the path to the WebP version
        webp_path = "#{path}#{filename}.webp"

        # Get static path (handles fingerprinting)
        original_static_path = static_image_path(image_path)

        # Check if WebP version exists in the filesystem
        webp_exists = webp_file_exists?(webp_path)

        if webp_exists do
          webp_static_path = static_image_path(webp_path)

          # Generate picture tag with WebP and fallback
          Tag.content_tag(:picture, [
            Tag.tag(:source, srcset: webp_static_path, type: "image/webp"),
            Tag.tag(:source, srcset: original_static_path),
            Tag.tag(:img, Keyword.merge([src: original_static_path], attrs))
          ])
        else
          # If WebP doesn't exist, just use the original image
          Tag.tag(:img, Keyword.merge([src: original_static_path], attrs))
        end
    end
  end

  @spec static_image_path(binary()) :: binary()
  @doc """
  Returns a string with the static path for the given image.
  Handles the static path helpers with proper asset fingerprinting.
  """
  def static_image_path(image_path) do
    SpacecastWeb.Endpoint.static_path(image_path)
  end

  # Helper function to extract path information from image path
  defp extract_path_info(image_path) do
    # Split the path to get directory and filename
    path_parts = String.split(image_path, "/")
    filename_with_ext = List.last(path_parts)
    directory = Enum.drop(path_parts, -1) |> Enum.join("/")

    # Handle case where filename_with_ext might be nil
    case filename_with_ext do
      nil ->
        {"", "", ""}

      filename ->
        # Extract filename and extension
        case String.split(filename, ".") do
          [name, ext] -> {directory <> "/", name, "." <> ext}
          [name] -> {directory <> "/", name, ""}
        end
    end
  end

  # Helper function to check if WebP file exists
  defp webp_file_exists?(webp_path) do
    # Convert to absolute path for filesystem check
    absolute_path = Path.join([:code.priv_dir(:spacecast_web), "static", webp_path])
    File.exists?(absolute_path)
  end
end
