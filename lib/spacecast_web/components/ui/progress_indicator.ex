defmodule SpacecastWeb.Components.UI.ProgressIndicator do
  @moduledoc """
  A collection of monospace progress indicators using ASCII art.

  This component provides various styles of progress indicators that maintain
  the monospace grid aesthetic, including:
  - Linear progress bars
  - Step indicators
  - Loading spinners
  - Circular progress indicators
  """
  use Phoenix.Component

  import Jason, only: [encode!: 1]

  @doc """
  Renders a linear progress bar with ASCII art.

  ## Examples
      
      <.linear_progress id="download-progress" value={75} max={100} />
      
      <.linear_progress id="task-progress" value={3} max={10} label="Step 3 of 10" />
      
      <.linear_progress id="custom-progress" value={50} max={100} 
                       empty_char="░" filled_char="█" />

  """
  attr :id, :string, required: true, doc: "the unique identifier for the progress bar"
  attr :value, :integer, required: true, doc: "the current progress value"
  attr :max, :integer, default: 100, doc: "the maximum progress value"
  attr :label, :string, default: nil, doc: "optional label to display with the progress"
  attr :class, :string, default: "", doc: "additional CSS classes"
  attr :width, :integer, default: 40, doc: "width of the progress bar in characters"
  attr :empty_char, :string, default: "·", doc: "character used for empty progress"
  attr :filled_char, :string, default: "■", doc: "character used for filled progress"
  attr :animate, :boolean, default: false, doc: "whether to animate the progress bar"

  def linear_progress(assigns) do
    # Calculate percentage and number of filled characters
    percentage = min(100, trunc(assigns.value / assigns.max * 100))
    filled_count = trunc(percentage / 100 * assigns.width)
    empty_count = assigns.width - filled_count

    assigns =
      assign(assigns,
        percentage: percentage,
        filled_count: filled_count,
        empty_count: empty_count
      )

    ~H"""
    <div id={@id} class={"monospace-progress linear #{@class} #{if @animate, do: "animate"}"} phx-hook="ProgressIndicatorHook">
      <div class="progress-container">
        <div class="progress-bar" role="progressbar" aria-valuenow={@value} aria-valuemin="0" aria-valuemax={@max}>
          [{String.duplicate(@filled_char, @filled_count)}{String.duplicate(@empty_char, @empty_count)}] {@percentage}%
        </div>
        <%= if @label do %>
          <div class="progress-label">{@label}</div>
        <% end %>
      </div>
    </div>
    """
  end

  @doc """
  Renders a step indicator with ASCII art.

  ## Examples
      
      <.step_indicator id="checkout-steps" current_step={2} total_steps={4} 
                      labels={["Cart", "Shipping", "Payment", "Confirmation"]} />
      
      <.step_indicator id="wizard-progress" current_step={1} total_steps={3} />

  """
  attr :id, :string, required: true, doc: "the unique identifier for the step indicator"
  attr :current_step, :integer, required: true, doc: "the current step (1-based)"
  attr :total_steps, :integer, required: true, doc: "the total number of steps"
  attr :labels, :list, default: [], doc: "optional labels for each step"
  attr :class, :string, default: "", doc: "additional CSS classes"

  attr :style, :string,
    default: "numbered",
    values: ["numbered", "dots", "arrows"],
    doc: "style of the step indicator"

  def step_indicator(assigns) do
    # Ensure current_step is within bounds
    current_step = max(1, min(assigns.current_step, assigns.total_steps))

    # Generate labels if not provided
    labels =
      if Enum.empty?(assigns.labels) do
        Enum.map(1..assigns.total_steps, fn i -> "Step #{i}" end)
      else
        assigns.labels
      end

    assigns =
      assign(assigns,
        current_step: current_step,
        labels: labels
      )

    ~H"""
    <div id={@id} class={"monospace-progress step-indicator #{@style} #{@class}"} phx-hook="ProgressIndicatorHook">
      <div class="steps-container">
        <%= case @style do %>
          <% "numbered" -> %>
            <div class="steps-track">
              <%= for i <- 1..@total_steps do %>
                <div class={"step-item #{cond do
                  i < @current_step -> "completed"
                  i == @current_step -> "current"
                  true -> "pending"
                end}"}>
                  <div class="step-marker">
                    <%= if i < @current_step do %>
                      [✓]
                    <% else %>
                      [{i}]
                    <% end %>
                  </div>
                  <div class="step-label">{Enum.at(@labels, i - 1, "")}</div>
                </div>
                <%= if i < @total_steps do %>
                  <div class="step-connector">
                    <%= if i < @current_step do %>
                      ====
                    <% else %>
                      ----
                    <% end %>
                  </div>
                <% end %>
              <% end %>
            </div>
          <% "dots" -> %>
            <div class="steps-track">
              <%= for i <- 1..@total_steps do %>
                <div class={"step-item #{cond do
                  i < @current_step -> "completed"
                  i == @current_step -> "current"
                  true -> "pending"
                end}"}>
                  <div class="step-marker">
                    <%= cond do %>
                      <% i < @current_step -> %>
                        (•)
                      <% i == @current_step -> %>
                        (◉)
                      <% true -> %>
                        (○)
                    <% end %>
                  </div>
                  <div class="step-label">{Enum.at(@labels, i - 1, "")}</div>
                </div>
                <%= if i < @total_steps do %>
                  <div class="step-connector">
                    <%= if i < @current_step do %>
                      ····
                    <% else %>
                      ····
                    <% end %>
                  </div>
                <% end %>
              <% end %>
            </div>
          <% "arrows" -> %>
            <div class="steps-track">
              <%= for i <- 1..@total_steps do %>
                <div class={"step-item #{cond do
                  i < @current_step -> "completed"
                  i == @current_step -> "current"
                  true -> "pending"
                end}"}>
                  <div class="step-marker">
                    <%= cond do %>
                      <% i < @current_step -> %>
                        [→]
                      <% i == @current_step -> %>
                        [►]
                      <% true -> %>
                        [─]
                    <% end %>
                  </div>
                  <div class="step-label">{Enum.at(@labels, i - 1, "")}</div>
                </div>
                <%= if i < @total_steps do %>
                  <div class="step-connector">
                    <%= if i < @current_step do %>
                      ──►
                    <% else %>
                      ───
                    <% end %>
                  </div>
                <% end %>
              <% end %>
            </div>
        <% end %>
      </div>
    </div>
    """
  end

  @doc """
  Renders a loading spinner with ASCII art.

  ## Examples
      
      <.spinner id="loading-spinner" />
      
      <.spinner id="custom-spinner" frames={["/", "-", "\\", "|"]} speed={150} label="Loading..." />

  """
  attr :id, :string, required: true, doc: "the unique identifier for the spinner"

  attr :frames, :list,
    default: ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"],
    doc: "animation frames"

  attr :speed, :integer, default: 100, doc: "animation speed in milliseconds"
  attr :label, :string, default: nil, doc: "optional label to display with the spinner"
  attr :class, :string, default: "", doc: "additional CSS classes"

  attr :style, :string,
    default: "braille",
    values: ["braille", "ascii", "dots", "line"],
    doc: "style of the spinner"

  def spinner(assigns) do
    # Set frames based on style if not explicitly provided
    frames =
      cond do
        assigns.frames != ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] ->
          assigns.frames

        assigns.style == "ascii" ->
          ["/", "-", "\\", "|"]

        assigns.style == "dots" ->
          ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]

        assigns.style == "line" ->
          ["▁", "▂", "▃", "▄", "▅", "▆", "▇", "█", "▇", "▆", "▅", "▄", "▃", "▂"]

        # braille style (default)
        true ->
          ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
      end

    assigns = assign(assigns, frames: frames)

    ~H"""
    <div id={@id} class={"monospace-progress spinner #{@style} #{@class}"} phx-hook="ProgressIndicatorHook" data-frames={encode!(@frames)} data-speed={@speed}>
      <div class="spinner-container">
        <div class="spinner-animation" role="status" aria-live="polite">
          {if Enum.empty?(@frames),
            do: "",
            else:
              (case @frames do
                 [h | _] -> h
                 _ -> ""
               end)}
        </div>
        <%= if @label do %>
          <div class="spinner-label">{@label}</div>
        <% end %>
      </div>
    </div>
    """
  end

  @doc """
  Renders a circular progress indicator with ASCII art.

  ## Examples
      
      <.circular_progress id="upload-progress" value={75} max={100} />
      
      <.circular_progress id="task-progress" value={3} max={10} label="3/10 Complete" />

  """
  attr :id, :string, required: true, doc: "the unique identifier for the progress indicator"
  attr :value, :integer, required: true, doc: "the current progress value"
  attr :max, :integer, default: 100, doc: "the maximum progress value"
  attr :label, :string, default: nil, doc: "optional label to display with the progress"
  attr :class, :string, default: "", doc: "additional CSS classes"

  attr :size, :string,
    default: "medium",
    values: ["small", "medium", "large"],
    doc: "size of the circular indicator"

  def circular_progress(assigns) do
    # Calculate percentage
    percentage = min(100, trunc(assigns.value / assigns.max * 100))

    # Choose the appropriate ASCII art based on percentage and size
    {circle, inner_text} = get_circle_art(percentage, assigns.size)

    assigns =
      assign(assigns,
        percentage: percentage,
        circle: circle,
        inner_text: inner_text
      )

    ~H"""
    <div id={@id} class={"monospace-progress circular #{@size} #{@class}"} phx-hook="ProgressIndicatorHook">
      <div class="circular-container">
        <div class="circular-progress" role="progressbar" aria-valuenow={@value} aria-valuemin="0" aria-valuemax={@max}>
          <pre class="circle-art"><%= @circle %></pre>
          <div class="circle-text">{@inner_text}</div>
        </div>
        <%= if @label do %>
          <div class="progress-label">{@label}</div>
        <% end %>
      </div>
    </div>
    """
  end

  # Helper function to get the appropriate ASCII art circle based on percentage and size
  defp get_circle_art(percentage, size) do
    # Small circle (3x3)
    small_circles = %{
      0 => {"┌─┐\n│ │\n└─┘", "0%"},
      25 => {"┌─┐\n│╵│\n└─┘", "25"},
      50 => {"┌─┐\n│┼│\n└─┘", "50"},
      75 => {"┌─┐\n│╷│\n└─┘", "75"},
      100 => {"┌─┐\n│█│\n└─┘", "100"}
    }

    # Medium circle (5x5)
    medium_circles = %{
      0 => {"┌───┐\n│   │\n│   │\n│   │\n└───┘", "0%"},
      10 => {"┌───┐\n│   │\n│   │\n│▁  │\n└───┘", "10%"},
      20 => {"┌───┐\n│   │\n│   │\n│▂  │\n└───┘", "20%"},
      30 => {"┌───┐\n│   │\n│   │\n│▃▃ │\n└───┘", "30%"},
      40 => {"┌───┐\n│   │\n│   │\n│▄▄▄│\n└───┘", "40%"},
      50 => {"┌───┐\n│   │\n│▅▅▅│\n│▅▅▅│\n└───┘", "50%"},
      60 => {"┌───┐\n│▆  │\n│▆▆▆│\n│▆▆▆│\n└───┘", "60%"},
      70 => {"┌───┐\n│▇▇ │\n│▇▇▇│\n│▇▇▇│\n└───┘", "70%"},
      80 => {"┌───┐\n│███│\n│███│\n│███│\n└───┘", "80%"},
      90 => {"┌───┐\n│███│\n│███│\n│███│\n└───┘", "90%"},
      100 => {"┌───┐\n│███│\n│███│\n│███│\n└───┘", "100%"}
    }

    # Large circle (7x7)
    large_circles = %{
      0 => {"┌─────┐\n│     │\n│     │\n│     │\n│     │\n│     │\n└─────┘", "0%"},
      10 => {"┌─────┐\n│     │\n│     │\n│     │\n│     │\n│▁    │\n└─────┘", "10%"},
      20 => {"┌─────┐\n│     │\n│     │\n│     │\n│     │\n│▂▂   │\n└─────┘", "20%"},
      30 => {"┌─────┐\n│     │\n│     │\n│     │\n│▃▃   │\n│▃▃▃  │\n└─────┘", "30%"},
      40 => {"┌─────┐\n│     │\n│     │\n│     │\n│▄▄▄▄ │\n│▄▄▄▄ │\n└─────┘", "40%"},
      50 => {"┌─────┐\n│     │\n│     │\n│▅▅▅▅▅│\n│▅▅▅▅▅│\n│▅▅▅▅▅│\n└─────┘", "50%"},
      60 => {"┌─────┐\n│     │\n│▆▆▆  │\n│▆▆▆▆▆│\n│▆▆▆▆▆│\n│▆▆▆▆▆│\n└─────┘", "60%"},
      70 => {"┌─────┐\n│▇    │\n│▇▇▇▇ │\n│▇▇▇▇▇│\n│▇▇▇▇▇│\n│▇▇▇▇▇│\n└─────┘", "70%"},
      80 => {"┌─────┐\n│█████│\n│█████│\n│█████│\n│█████│\n│█████│\n└─────┘", "80%"},
      90 => {"┌─────┐\n│█████│\n│█████│\n│█████│\n│█████│\n│█████│\n└─────┘", "90%"},
      100 => {"┌─────┐\n│█████│\n│█████│\n│█████│\n│█████│\n│█████│\n└─────┘", "100%"}
    }

    # Choose the appropriate circle map based on size
    circle_map =
      case size do
        "small" -> small_circles
        "large" -> large_circles
        _ -> medium_circles
      end

    # Find the closest percentage in the map
    closest_key =
      circle_map
      |> Map.keys()
      |> Enum.sort()
      |> Enum.reduce(0, fn key, acc ->
        if abs(percentage - key) < abs(percentage - acc), do: key, else: acc
      end)

    # Return the circle art and inner text
    Map.get(circle_map, closest_key)
  end
end
