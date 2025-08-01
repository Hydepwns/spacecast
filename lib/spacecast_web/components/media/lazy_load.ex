defmodule SpacecastWeb.Components.Media.LazyLoad do
  @moduledoc """
  LazyLoad component that only renders its content when it's about to be visible.

  This component uses IntersectionObserver to defer rendering of components
  until they are approaching the viewport, improving initial page load performance.

  ## Examples

  ```heex
  <.lazy_load id="my-lazy-section" margin="200px">
    <.responsive_image
      image_name="foxheist1"
      original_format="png"
      alt="Fox Heist Scene"
      width="640"
      height="360"
    />
  </.lazy_load>
  ```

  With a placeholder:

  ```heex
  <.lazy_load id="chart-container" class="chart-wrapper">
    <:placeholder>
      <div class="loading-placeholder">
        <p>Chart loading...</p>
      </div>
    </:placeholder>
    
    <.complex_chart data={@chart_data} />
  </.lazy_load>
  ```
  """
  use Phoenix.Component

  @doc """
  Renders content only when it's close to being visible in the viewport.

  ## Attributes

  * `id` - Required. Unique ID for the component
  * `class` - Optional. Additional CSS classes for the container
  * `margin` - Optional. Root margin for the IntersectionObserver (default: "100px")
  * `threshold` - Optional. Visibility threshold to trigger loading (default: 0.1)
  * `skip_lazy` - Optional. Set to true to bypass lazy loading (default: false)
  * `placeholder` - Optional slot. Content to show while actual content is loading

  ## Slots

  * default - The content to lazy load
  * `:placeholder` - Optional content to display while the main content is loading
  """
  attr :id, :string, required: true
  attr :class, :string, default: nil
  attr :margin, :string, default: "100px"
  attr :threshold, :float, default: 0.1
  attr :skip_lazy, :boolean, default: false

  slot :placeholder
  slot :inner_block, required: true

  def lazy_load(assigns) do
    ~H"""
    <div id={@id} class={["lazy-load-container", @class]} phx-hook="LazyLoad" data-margin={@margin} data-threshold={@threshold} data-loaded={@skip_lazy}>
      <div class="lazy-load-placeholder" data-lazy-placeholder style={if @skip_lazy, do: "display: none;"}>
        <div :if={Enum.any?(@placeholder)}>
          {render_slot(@placeholder)}
        </div>
        <div :if={!Enum.any?(@placeholder)} class="default-placeholder" aria-hidden="true"></div>
      </div>

      <div class="lazy-load-content" data-lazy-content style={if !@skip_lazy, do: "display: none;"}>
        {render_slot(@inner_block)}
      </div>
    </div>
    """
  end
end
