defmodule SpacecastWeb.Components.UI.ResponsiveImage do
  @moduledoc """
  Responsive image component with optimizations for mobile devices.

  This component provides:
  - Automatic srcset generation for responsive images
  - Lazy loading with placeholder support
  - Data-saver mode support
  - Optimized loading strategy
  """
  use Phoenix.Component

  @doc """
  Renders a responsive image with optimizations for mobile devices.

  ## Examples

      <.responsive_image
        src="/images/example.jpg"
        alt="Example image"
        placeholder="/images/example-placeholder.jpg"
        widths={[320, 640, 960, 1280]}
        sizes="(max-width: 768px) 100vw, 50vw"
        class="my-image"
        lazy={true}
        essential={true}
      />

  ## Attributes

  * `src` - The source URL of the image (required)
  * `alt` - Alt text for the image (required)
  * `placeholder` - Low-quality image placeholder URL
  * `widths` - List of image widths for srcset
  * `sizes` - Sizes attribute for responsive images
  * `class` - Additional CSS classes
  * `lazy` - Whether to lazy load the image (default: true)
  * `essential` - Whether the image is essential content (default: true)
  * `rest` - Additional HTML attributes
  """
  attr :src, :string, required: true
  attr :alt, :string, required: true
  attr :placeholder, :string, default: nil
  attr :widths, :list, default: [320, 640, 960, 1280]
  attr :sizes, :string, default: "(max-width: 768px) 100vw, 50vw"
  attr :class, :string, default: ""
  attr :lazy, :boolean, default: true
  attr :essential, :boolean, default: true
  attr :rest, :global

  def responsive_image(assigns) do
    ~H"""
    <img
      src={if @lazy, do: @placeholder || low_quality_placeholder(@src), else: @src}
      alt={@alt}
      class={["responsive-image", @lazy && "lazy-load", @class]}
      data-src={if @lazy, do: @src}
      data-srcset={generate_srcset(@src, @widths)}
      data-sizes={@sizes}
      data-placeholder={@placeholder || low_quality_placeholder(@src)}
      data-nonessential={if @essential, do: "false", else: "true"}
      loading={if @lazy, do: "lazy", else: "eager"}
      decoding="async"
      width={extract_dimensions(@src)[:width]}
      height={extract_dimensions(@src)[:height]}
      {@rest}
    />
    """
  end

  @doc """
  Generates a low-quality placeholder URL from a source image.

  This is a simple implementation that assumes image URLs follow a pattern.
  In a real application, this would likely use a more sophisticated approach
  or be pre-computed.
  """
  def low_quality_placeholder(src) do
    # In a real implementation, this would:
    # 1. Check if a placeholder exists
    # 2. Generate a placeholder if needed
    # 3. Return a data URI or path to tiny placeholder

    # For now, simulate by returning a modified path
    case Path.extname(src) do
      ext when ext in [".jpg", ".jpeg", ".png", ".webp"] ->
        base = Path.basename(src, ext)
        dir = Path.dirname(src)
        Path.join(dir, "#{base}-placeholder#{ext}")

      _ ->
        # Fallback to a 1x1 pixel transparent data URI
        "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 1 1'%3E%3C/svg%3E"
    end
  end

  @doc """
  Generates a srcset attribute for responsive images.

  ## Examples

      iex> generate_srcset("/images/example.jpg", [320, 640, 960])
      "/images/example-320w.jpg 320w, /images/example-640w.jpg 640w, /images/example-960w.jpg 960w"
  """
  def generate_srcset(src, widths) do
    # In a real implementation, this would:
    # 1. Check if responsive variants exist
    # 2. Generate variants if needed
    # 3. Return a srcset attribute

    # Extract base name and extension
    ext = Path.extname(src)
    base = Path.basename(src, ext)
    dir = Path.dirname(src)

    # Generate srcset entries
    widths
    |> Enum.map(fn width ->
      path = Path.join(dir, "#{base}-#{width}w#{ext}")
      "#{path} #{width}w"
    end)
    |> Enum.join(", ")
  end

  @doc """
  Attempts to extract width and height from an image path.

  This is a placeholder implementation. In a real application,
  this would likely use a more sophisticated approach or be pre-computed.
  """
  def extract_dimensions(_src) do
    # In a real implementation, this would:
    # 1. Look up dimensions from a cache or metadata
    # 2. Calculate dimensions if needed
    # 3. Return width and height

    # For now, return placeholder values
    %{width: "100%", height: "auto"}
  end
end
