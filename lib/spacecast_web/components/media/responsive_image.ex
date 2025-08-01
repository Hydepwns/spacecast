defmodule SpacecastWeb.Components.Media.ResponsiveImage do
  @moduledoc """
  Responsive image component that automatically uses optimized images.

  This component makes it easy to use the optimized WebP images with proper
  responsive behavior, fallbacks, and accessibility attributes.

  ## Examples

  ```heex
  <.responsive_image
    image_name="foxheist1"
    original_format="png"
    alt="Fox Heist Scene"
    width="640"
    height="360"
  />
  ```

  With additional options:

  ```heex
  <.responsive_image
    image_name="foxmask"
    original_format="png"
    quality="high"
    sizes="(max-width: 768px) 100vw, 50vw"
    alt="Fox with Mask"
    width="800"
    height="600"
    class="my-custom-class"
    loading="eager"
  />
  ```
  """
  use Phoenix.Component

  @doc """
  Renders a responsive image with WebP and original format versions at multiple sizes.

  ## Attributes

  * `image_name` - Required. The base name of the image without extension
  * `original_format` - Optional. The original format of the image (default: "png")
  * `quality` - Optional. The WebP quality to use (default: "medium", options: "low", "medium", "high")
  * `sizes` - Optional. The sizes attribute for the srcset (default responsive sizes)
  * `alt` - Optional. The alt text for the image (defaults to image_name if not provided)
  * `width` - Required. The width of the image in pixels
  * `height` - Required. The height of the image in pixels
  * `class` - Optional. Additional CSS classes to apply to the image
  * `loading` - Optional. The loading attribute (default: "lazy", options: "lazy", "eager", "auto")
  """
  attr :image_name, :string, required: true
  attr :original_format, :string, default: "png"
  attr :quality, :string, default: "medium"
  attr :sizes, :string, default: "(max-width: 640px) 100vw, (max-width: 1024px) 50vw, 33vw"
  attr :alt, :string, default: nil
  attr :width, :integer, required: true
  attr :height, :integer, required: true
  attr :class, :string, default: nil
  attr :loading, :string, default: "lazy"

  def responsive_image(assigns) do
    # Set default alt text to image name if not provided
    assigns = assign_new(assigns, :alt, fn -> assigns.image_name end)

    ~H"""
    <picture>
      <!-- WebP versions -->
      <source type="image/webp" srcset={build_srcset(@image_name, @quality, "webp")} sizes={@sizes} />
      <!-- Original format versions -->
      <source srcset={build_srcset(@image_name, nil, @original_format)} sizes={@sizes} />
      <!-- Fallback -->
      <img src={"/images/#{@image_name}.#{@original_format}"} alt={@alt} loading={@loading} width={@width} height={@height} class={@class} />
    </picture>
    """
  end

  # Helper function to build srcset
  defp build_srcset(image_name, quality, format) do
    widths = [320, 640, 960, 1280, 1920]

    srcset =
      widths
      |> Enum.map(fn width ->
        quality_suffix = if quality, do: "-#{quality}", else: ""

        "/images/responsive/#{image_name}/#{image_name}-#{width}w#{quality_suffix}.#{format} #{width}w"
      end)
      |> Enum.join(", ")

    srcset
  end
end
