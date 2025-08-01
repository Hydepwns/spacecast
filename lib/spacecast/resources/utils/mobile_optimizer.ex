defmodule Spacecast.Resources.MobileOptimizer do
  @moduledoc """
  Optimizes resource interfaces for mobile devices.

  This module provides mobile-specific optimizations for resource interfaces, including:
  - Progressive loading strategies for resources on mobile
  - Bandwidth-aware resource delivery
  - Mobile-optimized UI patterns for resource display
  - Touch-friendly interaction patterns for resource manipulation
  """

  require Logger
  import Phoenix.Component

  @spec progressive_loading_strategy(atom(), any() | nil, map()) :: {:ok, map()} | {:error, any()}
  def progressive_loading_strategy(resource_type, resource_id, client_info) do
    # Determine device capabilities
    is_mobile = client_info.user_agent =~ ~r/(Android|iPhone|iPad|iPod)/
    connection_type = client_info.connection_type || :unknown
    _viewport_width = client_info.viewport_width || 375

    # Create loading strategy based on device and connection
    strategy =
      cond do
        is_mobile and connection_type == :slow_3g ->
          %{
            initial_fields: essential_fields(resource_type),
            batch_size: 5,
            lazy_relationships: true,
            prefetch: false,
            initial_page_size: 10,
            image_quality: :low,
            text_only_mode: true,
            update_frequency: :manual
          }

        is_mobile and connection_type in [:fast_3g, :cellular] ->
          %{
            initial_fields: essential_fields(resource_type) ++ secondary_fields(resource_type),
            batch_size: 10,
            lazy_relationships: true,
            prefetch: limited_prefetch(resource_type, resource_id),
            initial_page_size: 20,
            image_quality: :medium,
            text_only_mode: false,
            update_frequency: :on_visibility
          }

        is_mobile ->
          %{
            initial_fields: essential_fields(resource_type) ++ secondary_fields(resource_type),
            batch_size: 15,
            lazy_relationships: true,
            prefetch: full_prefetch(resource_type, resource_id),
            initial_page_size: 25,
            image_quality: :high,
            text_only_mode: false,
            update_frequency: :realtime
          }

        true ->
          %{
            initial_fields: :all,
            batch_size: 25,
            lazy_relationships: false,
            prefetch: full_prefetch(resource_type, resource_id),
            initial_page_size: 50,
            image_quality: :original,
            text_only_mode: false,
            update_frequency: :realtime
          }
      end

    {:ok, strategy}
  end

  @spec compact_mobile_representation(map(), integer(), keyword()) :: map()
  def compact_mobile_representation(resource, viewport_width, opts \\ []) do
    resource_type = Map.get(opts, :resource_type, resource.__struct__)

    # Determine display characteristics based on viewport width
    display_config =
      cond do
        viewport_width < 360 ->
          %{
            max_title_length: 40,
            max_description_length: 80,
            image_size: :thumbnail,
            show_metadata: false,
            layout: :minimal
          }

        viewport_width < 480 ->
          %{
            max_title_length: 60,
            max_description_length: 120,
            image_size: :small,
            show_metadata: true,
            layout: :compact
          }

        viewport_width < 768 ->
          %{
            max_title_length: 80,
            max_description_length: 160,
            image_size: :medium,
            show_metadata: true,
            layout: :standard
          }

        true ->
          %{
            max_title_length: 100,
            max_description_length: 200,
            image_size: :large,
            show_metadata: true,
            layout: :expanded
          }
      end

    # Generate a compact representation based on resource type and display config
    generate_compact_representation(resource, resource_type, display_config)
  end

  @spec assign_lazy_collection(Phoenix.LiveView.Socket.t(), atom(), keyword()) ::
          Phoenix.LiveView.Socket.t()
  def assign_lazy_collection(socket, resource_type, opts \\ []) do
    client_info = get_client_info(socket)

    # Get progressive loading strategy
    {:ok, strategy} = progressive_loading_strategy(resource_type, nil, client_info)

    # Set up initial page of data
    page_size = strategy.initial_page_size
    loader_function = Keyword.get(opts, :loader, &default_loader/2)

    # Load initial data
    {:ok, initial_resources} =
      loader_function.(resource_type, %{
        limit: page_size,
        offset: 0,
        fields: strategy.initial_fields
      })

    # Calculate total pages
    total_count = Keyword.get(opts, :total_count, 100)
    total_pages = Float.ceil(total_count / page_size) |> trunc()

    # Assign lazy loading state to socket
    socket
    |> assign(:resources, initial_resources)
    |> assign(:lazy_loading_state, %{
      resource_type: resource_type,
      current_page: 1,
      total_pages: total_pages,
      page_size: page_size,
      strategy: strategy,
      loader: loader_function,
      loading: false,
      end_reached: total_pages <= 1
    })
  end

  @doc """
  Loads the next page of resources for a lazy loading component.

  ## Parameters
  * `socket` - The LiveView socket

  ## Returns
  * Updated socket with next page of resources loaded
  """
  def load_next_page(socket) do
    lazy_state = socket.assigns.lazy_loading_state

    # Check if we're already at the end
    if lazy_state.end_reached or lazy_state.loading do
      socket
    else
      # Mark as loading
      socket = assign(socket, :lazy_loading_state, %{lazy_state | loading: true})

      # Calculate next page parameters
      next_page = lazy_state.current_page + 1
      offset = (next_page - 1) * lazy_state.page_size

      # Load next page
      {:ok, next_page_resources} =
        lazy_state.loader.(lazy_state.resource_type, %{
          limit: lazy_state.page_size,
          offset: offset,
          fields: lazy_state.strategy.initial_fields
        })

      # Check if we've reached the end
      end_reached = next_page >= lazy_state.total_pages || Enum.empty?(next_page_resources)

      # Update resources and state
      socket
      |> assign(:resources, socket.assigns.resources ++ next_page_resources)
      |> assign(:lazy_loading_state, %{
        lazy_state
        | current_page: next_page,
          loading: false,
          end_reached: end_reached
      })
    end
  end

  @doc """
  Creates a mobile-optimized view of a resource detail.

  This function creates a responsive layout for resource details
  that adapts to different mobile screen sizes.

  ## Parameters
  * `resource` - The resource to display
  * `client_info` - Information about the client
  * `opts` - Additional options

  ## Returns
  * Mobile-optimized view configuration for the resource
  """
  def mobile_detail_view(resource, client_info, _opts \\ []) do
    viewport_width = client_info.viewport_width || 375
    resource_type = resource.__struct__

    # Determine detail layout based on viewport width
    layout =
      cond do
        viewport_width < 360 -> :stacked_minimal
        viewport_width < 480 -> :stacked_standard
        viewport_width < 768 -> :two_column
        true -> :responsive
      end

    # Determine which sections to show initially vs. on-demand
    sections =
      cond do
        viewport_width < 480 ->
          %{
            initial: [:header, :summary],
            expandable: [:details, :media, :relationships, :actions, :metadata],
            hidden: [:debug, :history]
          }

        viewport_width < 768 ->
          %{
            initial: [:header, :summary, :details, :actions],
            expandable: [:media, :relationships, :metadata],
            hidden: [:debug, :history]
          }

        true ->
          %{
            initial: [:header, :summary, :details, :media, :actions, :relationships],
            expandable: [:metadata],
            hidden: [:debug, :history]
          }
      end

    # Create the view configuration
    %{
      layout: layout,
      sections: sections,
      fields: organize_fields_for_mobile(resource, resource_type, viewport_width),
      media_quality: determine_media_quality(client_info),
      interaction_mode: determine_interaction_mode(client_info),
      touch_targets: %{
        # Minimum size in px for touch targets
        min_size: 44,
        # Minimum spacing between touch targets
        spacing: 8
      }
    }
  end

  @doc """
  Generates CSS for mobile resource layouts.

  ## Parameters
  * `opts` - Options for the CSS generation

  ## Returns
  * CSS string with mobile optimization styles
  """
  def mobile_resource_css(_opts \\ []) do
    """
    /* Mobile resource optimization styles */

    /* Base resource card styles */
    .resource-card {
      display: flex;
      flex-direction: column;
      padding: 12px;
      margin-bottom: 12px;
      border-radius: 8px;
      background-color: var(--resource-card-bg, #ffffff);
      box-shadow: 0 1px 3px rgba(0, 0, 0, 0.12);
    }

    /* Mobile-specific optimizations */
    @media (max-width: 767px) {
      .resource-card {
        padding: 10px;
        margin-bottom: 8px;
      }
      
      .resource-card__title {
        font-size: 16px;
        line-height: 1.3;
      }
      
      .resource-card__description {
        font-size: 14px;
        display: -webkit-box;
        -webkit-line-clamp: 2;
        -webkit-box-orient: vertical;
        overflow: hidden;
      }
      
      .resource-card__image {
        max-height: 180px;
        object-fit: cover;
      }
      
      /* Optimize touch targets */
      .resource-card__action {
        min-height: 44px;
        min-width: 44px;
        padding: 12px;
        margin: 4px;
      }
      
      /* Lazy loading indicator */
      .resource-list__loading {
        text-align: center;
        padding: 20px;
        font-size: 14px;
      }
      
      /* Stacked layouts */
      .resource-detail--stacked_minimal,
      .resource-detail--stacked_standard {
        display: flex;
        flex-direction: column;
      }
      
      /* Two column layout */
      .resource-detail--two_column {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 10px;
      }
      
      /* Expandable sections */
      .resource-section--expandable {
        max-height: 0;
        overflow: hidden;
        transition: max-height 0.3s ease;
      }
      
      .resource-section--expanded {
        max-height: 2000px;
      }
    }

    /* Super small mobile screens */
    @media (max-width: 359px) {
      .resource-card {
        padding: 8px;
        margin-bottom: 6px;
      }
      
      .resource-card__title {
        font-size: 15px;
      }
      
      .resource-card__description {
        font-size: 13px;
        -webkit-line-clamp: 1;
      }
      
      /* Hide non-essential elements */
      .resource-card__metadata {
        display: none;
      }
    }
    """
  end

  @doc """
  Creates a LiveComponent for mobile-optimized resource lists.

  ## Example

  ```elixir
  # In your LiveView
  def mount(_params, _session, socket) do
    socket = MobileOptimizer.assign_lazy_collection(socket, :post, total_count: 100)
    {:ok, socket}
  end

  # In your template
  def render(assigns) do
    ~H\"\"\"
    <.mobile_resource_list resources={@resources} state={@lazy_loading_state} />
    \"\"\"
  end

  # Handle the load more event
  def handle_event("load_more", _params, socket) do
    socket = MobileOptimizer.load_next_page(socket)
    {:noreply, socket}
  end
  """
  def mobile_resource_list(assigns) do
    ~H"""
    <div class="resource-list" id={@id || "resource-list"} phx-hook="LazyResourceList">
      <%= for resource <- @resources do %>
        <div class="resource-card">
          <h3 class="resource-card__title">{resource.title || resource.name}</h3>

          <%= if Map.has_key?(resource, :image_url) && resource.image_url do %>
            <img class="resource-card__image" src={resource.image_url} alt={resource.title || resource.name} loading="lazy" />
          <% end %>

          <%= if Map.has_key?(resource, :description) && resource.description do %>
            <div class="resource-card__description">{resource.description}</div>
          <% end %>

          <%= if Map.get(@state.strategy, :show_metadata, true) && Map.has_key?(resource, :metadata) do %>
            <div class="resource-card__metadata">
              <%= for {key, value} <- resource.metadata do %>
                <span class="metadata-item">{key}: {value}</span>
              <% end %>
            </div>
          <% end %>

          <div class="resource-card__actions">
            <button class="resource-card__action" phx-click="view_resource" phx-value-id={resource.id}>
              View
            </button>
          </div>
        </div>
      <% end %>

      <%= unless @state.end_reached do %>
        <div class="resource-list__loading" phx-click="load_more">
          <%= if @state.loading do %>
            <span>Loading more...</span>
          <% else %>
            <button>Load More</button>
          <% end %>
        </div>
      <% end %>
    </div>
    """
  end

  # Private helper functions

  defp essential_fields(:user), do: [:id, :name, :avatar_url]
  defp essential_fields(:post), do: [:id, :title, :summary, :created_at, :author_id]
  defp essential_fields(:team), do: [:id, :name, :slug, :avatar_url]
  defp essential_fields(_), do: [:id, :name]

  defp secondary_fields(:user), do: [:email, :bio]
  defp secondary_fields(:post), do: [:image_url, :tags, :likes_count]
  defp secondary_fields(:team), do: [:description, :member_count]
  defp secondary_fields(_), do: []

  defp limited_prefetch(:user, _id), do: [:teams]
  defp limited_prefetch(:post, _id), do: [:author]
  defp limited_prefetch(:team, _id), do: [:owner]
  defp limited_prefetch(_, _), do: []

  defp full_prefetch(:user, id), do: [:teams, {:posts, [user_id: id, limit: 5]}]
  defp full_prefetch(:post, id), do: [:author, {:comments, [post_id: id, limit: 3]}]
  defp full_prefetch(:team, id), do: [:owner, {:members, [team_id: id, limit: 5]}]
  defp full_prefetch(_, _), do: []

  defp generate_compact_representation(resource, :user, display_config) do
    %{
      id: resource.id,
      name: truncate(resource.name, display_config.max_title_length),
      avatar_url: optimize_image_url(resource.avatar_url, display_config.image_size),
      bio:
        if(display_config.show_metadata && resource.bio,
          do: truncate(resource.bio, display_config.max_description_length),
          else: nil
        ),
      metadata:
        if(display_config.show_metadata,
          do: %{
            teams_count: length(Map.get(resource, :teams, [])),
            posts_count: Map.get(resource, :posts_count, 0)
          }
        )
    }
  end

  defp generate_compact_representation(resource, :post, display_config) do
    %{
      id: resource.id,
      title: truncate(resource.title, display_config.max_title_length),
      summary: truncate(resource.summary || "", display_config.max_description_length),
      image_url:
        if(resource.image_url,
          do: optimize_image_url(resource.image_url, display_config.image_size),
          else: nil
        ),
      author_name: Map.get(resource, :author_name) || get_in(resource, [:author, :name]),
      metadata:
        if(display_config.show_metadata,
          do: %{
            published:
              if(resource.published_at, do: format_date(resource.published_at), else: nil),
            likes: Map.get(resource, :likes_count, 0),
            comments: Map.get(resource, :comments_count, 0)
          }
        )
    }
  end

  defp generate_compact_representation(resource, :team, display_config) do
    %{
      id: resource.id,
      name: truncate(resource.name, display_config.max_title_length),
      slug: resource.slug,
      avatar_url:
        if(resource.avatar_url,
          do: optimize_image_url(resource.avatar_url, display_config.image_size),
          else: nil
        ),
      description:
        if(display_config.show_metadata && resource.description,
          do: truncate(resource.description, display_config.max_description_length),
          else: nil
        ),
      metadata:
        if(display_config.show_metadata,
          do: %{
            members: Map.get(resource, :member_count, 0),
            projects: Map.get(resource, :projects_count, 0)
          }
        )
    }
  end

  defp generate_compact_representation(resource, _, display_config) do
    # Generic fallback representation
    %{
      id: resource.id,
      name: truncate(Map.get(resource, :name, "Unnamed"), display_config.max_title_length),
      description:
        Map.get(resource, :description) &&
          truncate(resource.description, display_config.max_description_length),
      image_url:
        Map.get(resource, :image_url) &&
          optimize_image_url(resource.image_url, display_config.image_size)
    }
  end

  defp truncate(nil, _), do: nil
  defp truncate(text, max_length) when byte_size(text) <= max_length, do: text

  defp truncate(text, max_length) do
    String.slice(text, 0, max_length - 3) <> "..."
  end

  defp optimize_image_url(nil, _), do: nil

  defp optimize_image_url(url, size) do
    # In a real implementation, this would use a service like Imgix or Cloudinary
    # to dynamically resize images based on the device needs
    case size do
      :thumbnail -> "#{url}?size=thumbnail&width=80&height=80"
      :small -> "#{url}?size=small&width=150&height=150"
      :medium -> "#{url}?size=medium&width=300&height=300"
      :large -> "#{url}?size=large&width=600&height=600"
      _ -> url
    end
  end

  defp format_date(datetime) do
    # Simple date formatting, would use a proper formatting library in production
    Calendar.strftime(datetime, "%b %d, %Y")
  end

  defp get_client_info(socket) do
    # Extract client info from socket assigns or session
    Map.get(socket.assigns, :client_info, %{
      user_agent: get_in(socket.assigns, [:client_info, :user_agent]) || "",
      viewport_width: get_in(socket.assigns, [:client_info, :viewport_width]) || 375,
      connection_type: get_in(socket.assigns, [:client_info, :connection_type]) || :unknown
    })
  end

  defp default_loader(_resource_type, _params) do
    # This would be replaced with an actual data loading function
    # in a real implementation
    {:ok,
     [
       %{id: "1", title: "Sample Resource 1", description: "This is a sample resource"},
       %{id: "2", title: "Sample Resource 2", description: "This is another sample resource"}
     ]}
  end

  defp organize_fields_for_mobile(resource, resource_type, viewport_width) do
    # Organize fields into groups based on importance and viewport width
    base_fields =
      case resource_type do
        :user ->
          %{
            primary: [:id, :name, :avatar_url, :role],
            secondary: [:email, :bio, :website],
            details: [:location, :joined_at, :last_active]
          }

        :post ->
          %{
            primary: [:id, :title, :author, :published_at],
            secondary: [:summary, :image_url, :tags],
            details: [:content, :likes, :comments]
          }

        :team ->
          %{
            primary: [:id, :name, :avatar_url, :slug],
            secondary: [:description, :member_count, :owner],
            details: [:created_at, :projects, :members]
          }

        _ ->
          %{
            primary: [:id, :name],
            secondary: [:description],
            details: Map.keys(resource) -- [:id, :name, :description]
          }
      end

    # Adjust field groups based on viewport width
    cond do
      viewport_width < 360 ->
        Map.put(base_fields, :visible, base_fields.primary)

      viewport_width < 480 ->
        Map.put(base_fields, :visible, base_fields.primary ++ Enum.take(base_fields.secondary, 2))

      viewport_width < 768 ->
        Map.put(base_fields, :visible, base_fields.primary ++ base_fields.secondary)

      true ->
        Map.put(
          base_fields,
          :visible,
          base_fields.primary ++ base_fields.secondary ++ base_fields.details
        )
    end
  end

  defp determine_media_quality(client_info) do
    # Determine appropriate media quality based on client info
    cond do
      client_info.connection_type == :slow_3g -> :minimal
      client_info.connection_type == :cellular -> :low
      client_info.viewport_width < 480 -> :medium
      true -> :high
    end
  end

  defp determine_interaction_mode(client_info) do
    is_touch = Regex.match?(~r/(Android|iPhone|iPad|iPod)/, client_info.user_agent)

    cond do
      is_touch && client_info.viewport_width < 480 -> :touch_compact
      is_touch -> :touch_standard
      client_info.viewport_width < 768 -> :desktop_compact
      true -> :desktop_standard
    end
  end
end
