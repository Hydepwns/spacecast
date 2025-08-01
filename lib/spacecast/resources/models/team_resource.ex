defmodule Spacecast.Resources.TeamResource do
  @moduledoc """
  Defines the Team resource for LiveViews.

  This is a stub implementation to satisfy relationships in UserResource.
  """

  use Spacecast.Utils.ResourceDSL
  use Spacecast.Events.ResourceIntegration.EventSourcedResource

  attribute(:id, :string, required: true)
  attribute(:name, :string, required: true)
  attribute(:description, :string)
  attribute(:created_at, :datetime)
  attribute(:active, :boolean, default: true)

  has_many(:members, Spacecast.Resources.UserResource)
  has_many(:posts, Spacecast.Resources.PostResource)
  has_one(:manager, Spacecast.Resources.UserResource, foreign_key: :manager_id)

  # Example of a through relationship to get all posts from team members
  has_many_through(:member_posts, through: [:members, :posts])

  validate(:name_not_empty, fn resource ->
    if resource.name && String.length(resource.name) > 0 do
      :ok
    else
      {:error, "Team name cannot be empty"}
    end
  end)

  @doc """
  Loads a team resource by ID.

  This is a stub implementation for testing purposes.
  In a real application, this would fetch the team from a database.
  """
  @spec load(String.t()) :: {:ok, map()} | {:error, any()}
  def load(id) do
    # This is a simple stub that always returns a team with the given ID
    # In a real application, this would query the database
    team = %{
      id: id,
      name: "Team #{id}",
      description: "A team with ID #{id}",
      active: true,
      created_at: DateTime.utc_now(),
      __resource_module__: __MODULE__
    }

    {:ok, team}
  end

  @doc """
  Updates a team resource with tracking (for audit/telemetry).

  ## Parameters
  * `resource` - The team resource to update
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

  @doc """
  Returns the initial state for a team resource as a struct.
  """
  def initial_state do
    %{
      id: nil,
      name: nil,
      description: nil,
      created_at: nil,
      active: true,
      __resource_module__: __MODULE__
    }
  end

  @doc """
  Applies an event to the team resource state, always returning a struct.
  """
  def apply_event(event, %__MODULE__{} = state) do
    case event.type do
      "team.created" ->
        struct(state, Map.merge(Map.from_struct(state), event.data))

      "team.updated" ->
        struct(state, Map.merge(Map.from_struct(state), event.data))

      "team.deleted" ->
        %{state | active: false}

      _ ->
        struct(state, Map.merge(Map.from_struct(state), event.data || %{}))
    end
  end

  @doc """
  Returns the resource type for this module.
  """
  def resource_type, do: "team"

  @doc """
  Validates a team resource map or struct. Returns {:ok, struct} or {:error, errors}.

  # NOTE: Do not use this directly in LiveView forms or controllers. Use `changeset/1` for form validation.
  """
  def validate(attrs) when is_map(attrs) do
    errors = []

    errors =
      if is_nil(attrs["name"]) or attrs["name"] == "",
        do: [{:name, "Team name cannot be empty"} | errors],
        else: errors

    if errors == [], do: {:ok, struct(__MODULE__, attrs)}, else: {:error, errors}
  end

  def changeset(attrs) when is_map(attrs) do
    attrs = for {k, v} <- attrs, into: %{}, do: {to_string(k), v}

    types = %{
      id: :string,
      name: :string,
      description: :string,
      created_at: :utc_datetime,
      active: :boolean
    }

    case validate(attrs) do
      {:ok, _struct} ->
        {%{}, types}
        |> Ecto.Changeset.cast(attrs, Map.keys(types))

      {:error, errors} ->
        changeset = {%{}, types} |> Ecto.Changeset.cast(attrs, Map.keys(types))

        Enum.reduce(errors, changeset, fn {field, msg}, cs ->
          Ecto.Changeset.add_error(cs, field, msg)
        end)
    end
  end

  def changeset(_), do: Ecto.Changeset.change(%{})

  # TODO: Stubbed event creation functions for EventSourcedResource compliance
  def create_events(_params, _metadata), do: {:ok, []}
  def create_update_events(_params, _metadata), do: {:ok, []}
  def create_delete_events(_params, _metadata), do: {:ok, []}
end
