defmodule SpacecastWeb.ResponsiveImageHelper do
  @moduledoc """
  Helper functions for rendering responsive images with multiple
  sizes, formats, and quality variations.

  This module provides functions for implementing responsive images using
  the srcset and sizes attributes, enabling browsers to select the most
  appropriate image based on screen size, pixel density, and network conditions.
  """

  alias Phoenix.HTML.Tag

  # Default sizes attribute to use if not specified
  @default_sizes "(max-width: 640px) 100vw, (max-width: 1024px) 50vw, 33vw"

  # Available image widths (should match those in responsive_image_generator.js)
  @image_widths [320, 640, 960, 1280, 1920]

  # Quality variations (should match those in responsive_image_generator.js)
  # Used for documentation purposes and in the generate_srcset function
  @quality_variations ["low", "medium", "high"]

  @doc """
  Renders a responsive image with appropriate srcset and sizes attributes.

  ## Options

  * `:alt` - Alt text for the image
  * `:class` - CSS classes to apply to the img tag
  * `:sizes` - The sizes attribute value (defaults to a sensible responsive default)
  * `:lazy` - Whether to use lazy loading (defaults to true)
  * `:quality_variation` - Which quality variation to use: "low", "medium", "high" (defaults to "high")
  * `:widths` - List of widths to include (defaults to all available widths)

  ## Examples

  ```heex
  {responsive_image_tag("/images/hero.png",
       alt: "Hero image",
       class: "hero-image",
       sizes: "(max-width: 768px) 100vw, 50vw")}
  ```

  This will generate a responsive image tag with WebP sources and fallbacks,
  with appropriate srcset attributes.
  """
  def responsive_image_tag(image_path, opts \\ []) do
    # Extract file info
    {_path, filename, ext} = extract_path_info(image_path)

    # Options with defaults
    alt = Keyword.get(opts, :alt, "")
    class = Keyword.get(opts, :class, "")
    sizes = Keyword.get(opts, :sizes, @default_sizes)
    lazy = Keyword.get(opts, :lazy, true)
    quality = Keyword.get(opts, :quality_variation, "high")
    widths = Keyword.get(opts, :widths, @image_widths)

    # Responsiveness attributes
    loading_attr = if lazy, do: [loading: "lazy"], else: []

    # Check if responsive directory exists
    responsive_dir_exists = responsive_directory_exists?(filename)

    if responsive_dir_exists do
      webp_srcset = generate_srcset(filename, "webp", widths, quality)
      fallback_ext = if ext in [".png", ".PNG"], do: ".png", else: ".jpg"
      fallback_srcset = generate_srcset(filename, fallback_ext, widths, quality)

      # Original image path as fallback
      original_static_path = SpacecastWeb.Endpoint.static_path(image_path)

      # Generate picture tag with multiple sources
      Tag.content_tag(:picture, [
        Tag.tag(:source,
          type: "image/webp",
          srcset: webp_srcset,
          sizes: sizes
        ),
        Tag.tag(:source,
          type: fallback_content_type(fallback_ext),
          srcset: fallback_srcset,
          sizes: sizes
        ),
        Tag.tag(
          :img,
          Keyword.merge(
            [src: original_static_path, alt: alt, class: class] ++ loading_attr,
            Keyword.drop(opts, [:alt, :class, :sizes, :lazy, :quality_variation, :widths])
          )
        )
      ])
    else
      # Fallback to standard optimized_image_tag from ImageHelper
      SpacecastWeb.ImageHelper.optimized_image_tag(image_path, opts)
    end
  end

  @doc """
  Generates a low-resolution placeholder image URL for use with
  progressive image loading techniques.

  Returns the URL for the smallest available size of the image.
  """
  def placeholder_image_url(image_path) do
    {_path, filename, _ext} = extract_path_info(image_path)

    # Use the smallest size as a placeholder
    smallest_size = Enum.min(@image_widths)

    if responsive_directory_exists?(filename) do
      # Try to use WebP for the placeholder
      placeholder_path = "/images/responsive/#{filename}/#{filename}-#{smallest_size}w-low.webp"
      SpacecastWeb.Endpoint.static_path(placeholder_path)
    else
      # Fallback to original image
      SpacecastWeb.Endpoint.static_path(image_path)
    end
  end

  # Private helper to generate srcset attribute value
  defp generate_srcset(filename, extension, widths, quality) do
    format_ext = if String.starts_with?(extension, "."), do: extension, else: ".#{extension}"

    # Validate that quality is one of the allowed values
    quality = if quality in @quality_variations, do: quality, else: "high"

    widths
    |> Enum.map(fn width ->
      url =
        if extension == "webp" do
          "/images/responsive/#{filename}/#{filename}-#{width}w-#{quality}.webp"
        else
          "/images/responsive/#{filename}/#{filename}-#{width}w#{format_ext}"
        end

      "#{SpacecastWeb.Endpoint.static_path(url)} #{width}w"
    end)
    |> Enum.join(", ")
  end

  # Private helper to determine if responsive directory exists for image
  defp responsive_directory_exists?(filename) do
    responsive_dir =
      Path.join([
        Application.app_dir(:spacecast, "priv/static/images/responsive"),
        filename
      ])

    File.exists?(responsive_dir) && File.dir?(responsive_dir)
  end

  defp extract_path_info(image_path) do
    path = Path.dirname(image_path)
    path = if path == ".", do: "", else: "#{path}/"

    filename = Path.basename(image_path, Path.extname(image_path))
    ext = Path.extname(image_path)

    {path, filename, ext}
  end

  defp fallback_content_type(ext) when is_binary(ext) do
    case String.downcase(ext) do
      ".jpg" -> "image/jpeg"
      ".jpeg" -> "image/jpeg"
      ".png" -> "image/png"
      _ -> "image/jpeg"
    end
  end
end
