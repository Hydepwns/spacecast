defmodule SpacecastWeb.Components.UI.Timeline do
  @moduledoc """
  A monospace timeline component that uses ASCII art to represent timeline events.

  This component displays a vertical or horizontal timeline with customizable
  events, maintaining the monospace grid aesthetic.
  """
  use Phoenix.Component

  @doc """
  Renders a vertical timeline with ASCII art elements.

  ## Examples
      
      <.vertical_timeline id="project-history">
        <:event title="Project Start" date="2023-01-15">
          Initial project setup and planning phase.
        </:event>
        <:event title="Alpha Release" date="2023-03-22" highlight={true}>
          First alpha version released to early testers.
        </:event>
        <:event title="Beta Release" date="2023-06-10">
          Beta version with core features implemented.
        </:event>
        <:event title="Version 1.0" date="2023-09-01" highlight={true}>
          Official release with all planned features.
        </:event>
      </.vertical_timeline>

  """
  attr :id, :string, required: true, doc: "the unique identifier for the timeline"
  attr :class, :string, default: "", doc: "additional CSS classes"

  attr :line_style, :string,
    default: "solid",
    values: ["solid", "dashed", "dotted"],
    doc: "style of the timeline line"

  slot :event, required: true do
    attr :title, :string, required: true
    attr :date, :string, required: true
    attr :highlight, :boolean
    attr :icon, :string
  end

  def vertical_timeline(assigns) do
    ~H"""
    <div id={@id} class="monospace-timeline vertical" phx-hook="MonospaceTimeline">
      <%= for event <- @event do %>
        <div class={"timeline-item #{if Map.get(event, :highlight, false), do: "highlighted"}"}>
          <div class="timeline-connector">
            <%= case @line_style do %>
              <% "solid" -> %>
                <span class="timeline-line">│</span>
              <% "dashed" -> %>
                <span class="timeline-line">┊</span>
              <% "dotted" -> %>
                <span class="timeline-line">┆</span>
            <% end %>
          </div>

          <div class="timeline-content">
            <div class="timeline-header">
              <h3 class="timeline-title">{event.title}</h3>
              <span class="timeline-date">{event.date}</span>
            </div>

            <div class="timeline-body">
              {render_slot(event)}
            </div>

            <%= if Map.get(event, :highlight, false) do %>
              <div class="timeline-highlight-marker">★</div>
            <% end %>
          </div>
        </div>
      <% end %>
    </div>
    """
  end

  @doc """
  Renders a horizontal timeline with ASCII art elements.

  ## Examples
      
      <.horizontal_timeline id="product-roadmap">
        <:event title="Planning" date="Q1 2023">
          Project planning and requirements gathering.
        </:event>
        <:event title="Development" date="Q2 2023" highlight={true}>
          Core feature development.
        </:event>
        <:event title="Testing" date="Q3 2023">
          QA and beta testing phase.
        </:event>
        <:event title="Release" date="Q4 2023" highlight={true}>
          Public release and marketing.
        </:event>
      </.horizontal_timeline>

  """
  attr :id, :string, required: true, doc: "the unique identifier for the timeline"
  attr :class, :string, default: "", doc: "additional CSS classes"

  attr :line_style, :string,
    default: "solid",
    values: ["solid", "dashed", "dotted"],
    doc: "style of the timeline line"

  slot :event, required: true do
    attr :title, :string, required: true
    attr :date, :string, required: true
    attr :highlight, :boolean
    attr :icon, :string
  end

  def horizontal_timeline(assigns) do
    ~H"""
    <div id={@id} class={"monospace-timeline horizontal #{@class}"} phx-hook="TimelineHook">
      <div class="timeline-container">
        <!-- Horizontal timeline line with appropriate style -->
        <div class="timeline-line">
          <%= case @line_style do %>
            <% "solid" -> %>
              ━━━<%= for _ <- 1..length(@event) do %>
                ━━━━━━━━━
              <% end %>━━━
            <% "dashed" -> %>
              ┅┅┅<%= for _ <- 1..length(@event) do %>
                ┅┅┅┅┅┅┅┅┅
              <% end %>┅┅┅
            <% "dotted" -> %>
              ┄┄┄<%= for _ <- 1..length(@event) do %>
                ┄┄┄┄┄┄┄┄┄
              <% end %>┄┄┄
          <% end %>
        </div>
        <!-- Timeline events -->
        <div class="timeline-events">
          <%= for {event, _index} <- Enum.with_index(@event) do %>
            <div class={"timeline-event #{if Map.get(event, :highlight, false), do: "highlighted"}"}>
              <!-- Node marker with appropriate style -->
              <div class="timeline-node">
                <%= if Map.get(event, :highlight, false) do %>
                  ┏━┓
                  ┃◆┃
                  ┗━┛
                <% else %>
                  ┏━┓
                  ┃○┃
                  ┗━┛
                <% end %>
              </div>

              <div class="timeline-content">
                <div class="timeline-header">
                  <h3 class="timeline-title">{event.title}</h3>
                  <span class="timeline-date">{event.date}</span>
                </div>
                <div class="timeline-body">
                  {render_slot(event)}
                </div>
              </div>
            </div>
          <% end %>
        </div>
      </div>
    </div>
    """
  end
end
