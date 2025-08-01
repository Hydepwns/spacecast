defmodule Spacecast.Resources.LoadingStrategy do
  @moduledoc """
  Handles client-aware loading strategies for resources.

  This module provides:
  - Device capability detection
  - Connection-aware loading strategies
  - Field selection based on client type
  - Prefetch strategies
  """

  @spec optimized_loading_strategy(map(), atom(), keyword()) :: map()
  def optimized_loading_strategy(client_info, resource_type, _opts \\ []) do
    # Determine device capabilities
    is_mobile = client_info.user_agent =~ ~r/(Android|iPhone|iPad|iPod)/
    connection_type = client_info.connection_type || :unknown

    # Create loading strategy based on device
    cond do
      is_mobile and connection_type in [:slow_3g, :cellular] ->
        %{
          initial_fields: essential_fields(resource_type),
          batch_size: 5,
          lazy_relationships: true,
          prefetch: false,
          # Longer cache for slow connections
          cache_ttl_seconds: 600,
          compression: true,
          image_quality: :low
        }

      is_mobile ->
        %{
          initial_fields: essential_fields(resource_type) ++ secondary_fields(resource_type),
          batch_size: 10,
          lazy_relationships: true,
          prefetch: limited_prefetch(resource_type),
          cache_ttl_seconds: 300,
          compression: true,
          image_quality: :medium
        }

      connection_type == :slow ->
        %{
          initial_fields: essential_fields(resource_type) ++ secondary_fields(resource_type),
          batch_size: 15,
          lazy_relationships: true,
          prefetch: limited_prefetch(resource_type),
          cache_ttl_seconds: 300,
          compression: false,
          image_quality: :medium
        }

      true ->
        %{
          initial_fields: :all,
          batch_size: 25,
          lazy_relationships: false,
          prefetch: full_prefetch(resource_type),
          # Shorter cache for fast connections
          cache_ttl_seconds: 120,
          compression: false,
          image_quality: :high
        }
    end
  end

  defp essential_fields(:user), do: [:id, :name, :avatar_url]
  defp essential_fields(:post), do: [:id, :title, :summary, :author_id]
  defp essential_fields(:team), do: [:id, :name, :slug]
  defp essential_fields(_), do: [:id, :name]

  defp secondary_fields(:user), do: [:email, :bio, :role]
  defp secondary_fields(:post), do: [:content, :published_at, :tags]
  defp secondary_fields(:team), do: [:description, :member_count]
  defp secondary_fields(_), do: []

  defp limited_prefetch(:user), do: [:teams]
  defp limited_prefetch(:post), do: [:author]
  defp limited_prefetch(:team), do: [:owner]
  defp full_prefetch(:user), do: [:teams, :posts, :followers]
  defp full_prefetch(:post), do: [:author, :comments, :likes]
  defp full_prefetch(:team), do: [:owner, :members, :projects]
  defp full_prefetch(_), do: []
end
