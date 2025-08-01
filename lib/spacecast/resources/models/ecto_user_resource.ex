defmodule Spacecast.Resources.EctoUserResource do
  @moduledoc """
  A resource that uses the EctoAdapter with the User schema.

  This demonstrates how to use the adapter system to create resources
  that are backed by Ecto schemas without having to manually define
  all the attributes.
  """

  use Spacecast.Utils.LiveViewResource

  # Use the EctoAdapter with the User schema
  adapter(Spacecast.Utils.EctoAdapter, schema: Spacecast.Schemas.User)

  @impl Spacecast.Utils.LiveViewResource
  def attributes do
    []
  end

  @impl Spacecast.Utils.LiveViewResource
  def relationships do
    []
  end

  @impl Spacecast.Utils.LiveViewResource
  def __resource_schema__ do
    %{
      id: :id,
      name: :string,
      email: :string,
      role: :string,
      active: :boolean,
      inserted_at: :naive_datetime,
      updated_at: :naive_datetime
    }
  end

  @doc """
  Creates a new EctoUserResource with default values.
  """
  def new do
    %{
      id: nil,
      name: "",
      email: "",
      role: "viewer",
      active: true,
      inserted_at: nil,
      updated_at: nil,
      __resource_module__: __MODULE__
    }
  end

  # We don't need to define attributes, relationships, or validations here
  # since they are extracted from the User schema by the EctoAdapter.
  # However, we can override or add to them if needed:

  # Define validations directly in the function
  @spec validations() :: [map()]
  @impl true
  def validations do
    [
      %{
        name: :ensure_valid_email_domain,
        validation_fn: fn resource ->
          if resource.email && String.contains?(resource.email, "@example.com") do
            :ok
          else
            {:error, "Email must be from the example.com domain"}
          end
        end
      }
    ]
  end

  @doc """
  Updates an ecto user resource with tracking (for audit/telemetry).

  ## Parameters
  * `resource` - The ecto user resource to update
  * `updates` - The update parameters
  * `metadata` - Additional metadata for the update
  * `opts` - Optional context/options (unused)

  ## Returns
  * `{:ok, updated_resource}` or `{:error, reason}`
  """
  @spec update_with_tracking(map(), map(), map(), map()) :: {:ok, map()} | {:error, any()}
  def update_with_tracking(resource, updates, metadata, _opts \\ %{}) do
    Spacecast.Utils.ChangeTracker.track_change(resource, updates, metadata)
  end
end
