defmodule Spacecast.Resources.MediaResource do
  @moduledoc """
  Defines the Media resource for LiveViews.

  This resource demonstrates polymorphic relationships.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:filename, :string, required: true)
  attribute(:content_type, :string, required: true)
  attribute(:file_size, :integer)
  attribute(:url, :string)
  attribute(:alt_text, :string)
  attribute(:post_id, :string)
  attribute(:user_id, :string)

  # Demonstrate belongs_to relationships
  belongs_to(:post, Spacecast.Resources.PostResource, foreign_key: :post_id)
  belongs_to(:user, Spacecast.Resources.UserResource, foreign_key: :user_id)

  validate(:filename_not_empty, fn resource ->
    if resource.filename && String.length(resource.filename) > 0 do
      :ok
    else
      {:error, "Media filename cannot be empty"}
    end
  end)

  @doc """
  Loads a media resource by ID.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    media = %{
      id: id,
      filename: "media-#{id}.jpg",
      content_type: "image/jpeg",
      file_size: 1024,
      url: "/uploads/media-#{id}.jpg",
      alt_text: "Media #{id}",
      post_id: "post-123",
      user_id: "user-456",
      __resource_module__: __MODULE__
    }

    {:ok, media}
  end

  @doc """
  Returns the initial state for a media resource.
  """
  @spec initial_state() :: map()
  def initial_state do
    %{
      id: nil,
      filename: "",
      content_type: "",
      file_size: nil,
      url: "",
      alt_text: "",
      post_id: nil,
      user_id: nil,
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Validates a media resource.
  """
  @spec validate(map()) :: {:ok, map()} | {:error, [String.t()]}
  def validate(resource) do
    []
    |> validate_filename(resource)
    |> case do
      [] -> {:ok, resource}
      errors -> {:error, errors}
    end
  end

  defp validate_filename(errors, resource) do
    if !resource.filename || String.length(resource.filename) == 0 do
      ["Media filename cannot be empty" | errors]
    else
      errors
    end
  end

  # Event sourcing methods
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
