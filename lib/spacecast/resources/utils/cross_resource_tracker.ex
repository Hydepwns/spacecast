defmodule Spacecast.Resources.CrossResourceTracker do
  @moduledoc """
  Tracks changes across related resources.

  This module provides functionality to track and propagate changes
  across related resources, including:
  - Tracking changes that affect multiple resources
  - Propagating changes to related resources
  - Visualizing relationships in change operations
  - Ensuring data consistency across related resources
  """

  require Logger
  alias Spacecast.Events.Core.Event
  alias Spacecast.Events.Core.TransactionalEventStore
  alias Spacecast.Repo
  import Ecto.Query

  # Schema for tracking change groups
  defmodule ChangeGroup do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @timestamps_opts [type: :utc_datetime_usec]
    schema "resource_change_groups" do
      field :name, :string
      field :description, :string
      field :user_id, :string
      field :correlation_id, :string
      field :metadata, :map, default: %{}

      timestamps()
    end

    def changeset(change_group, attrs) do
      attrs = for {k, v} <- attrs, into: %{}, do: {to_string(k), v}

      change_group
      |> cast(attrs, [:name, :description, :user_id, :correlation_id, :metadata])
      |> validate_required([:name, :user_id])
    end
  end

  # Schema for tracking individual changes within a change group
  defmodule Change do
    use Ecto.Schema
    import Ecto.Changeset

    @primary_key {:id, :binary_id, autogenerate: true}
    @timestamps_opts [type: :utc_datetime_usec]
    schema "resource_changes" do
      field :resource_type, :string
      field :resource_id, :string
      field :changes, :map
      field :change_group_id, :binary_id
      field :is_primary, :boolean, default: false
      field :metadata, :map, default: %{}
      field :user_id, :string

      timestamps()
    end

    def changeset(change, attrs) do
      attrs = for {k, v} <- attrs, into: %{}, do: {to_string(k), v}

      change
      |> cast(attrs, [
        :resource_type,
        :resource_id,
        :changes,
        :change_group_id,
        :is_primary,
        :metadata,
        :user_id
      ])
      |> validate_required([:resource_type, :resource_id, :changes, :change_group_id])
    end
  end

  @spec track_with_propagation(String.t(), String.t(), map(), String.t(), keyword()) ::
          {:ok, map()} | {:error, any()}
  @doc """
  Tracks a change and propagates it to related resources.

  ## Parameters
  * `resource_type` - The type of resource being changed
  * `resource_id` - The ID of the resource being changed
  * `changes` - The changes being made
  * `user_id` - The ID of the user making the change
  * `opts` - Additional options

  ## Returns
  * `{:ok, change_group}` - The change was tracked successfully
  * `{:error, reason}` - Failed to track the change
  """
  def track_with_propagation(resource_type, resource_id, changes, user_id, opts \\ []) do
    # Create a unique ID for this change group
    change_group_id = Ecto.UUID.generate()
    correlation_id = Keyword.get(opts, :correlation_id, change_group_id)

    # Create the change group
    {:ok, change_group} =
      create_change_group(%{
        name: Keyword.get(opts, :name, "Change to #{resource_type}:#{resource_id}"),
        description: Keyword.get(opts, :description, ""),
        user_id: user_id,
        correlation_id: correlation_id,
        metadata: Keyword.get(opts, :metadata, %{})
      })

    # Create the primary change
    primary_change = %{
      resource_type: resource_type,
      resource_id: resource_id,
      changes: changes,
      user_id: user_id,
      change_group_id: change_group.id,
      is_primary: true,
      metadata: %{
        initiated_at: DateTime.utc_now(),
        change_type: Keyword.get(opts, :change_type, "update")
      }
    }

    # Get related resources that should be updated
    related_configs = Keyword.get(opts, :related_resources, [])

    # Process the change group in a transaction
    TransactionalEventStore.transaction_with_events(fn ->
      # Store the primary change
      {:ok, stored_primary_change} = store_change(primary_change)

      # Process related resources
      propagated_changes =
        create_propagated_changes(
          related_configs,
          resource_type,
          resource_id,
          changes,
          change_group.id,
          user_id
        )

      # Store propagated changes
      stored_propagated_changes =
        Enum.map(propagated_changes, fn change ->
          {:ok, stored_change} = store_change(change)
          stored_change
        end)

      # Create events for all changes
      all_changes = [stored_primary_change | stored_propagated_changes]
      events = create_events_for_changes(all_changes, correlation_id)

      {:ok,
       %{
         change_group: change_group,
         primary_change: stored_primary_change,
         propagated_changes: stored_propagated_changes
       }, events}
    end)
  end

  @spec visualize_change_group(String.t()) :: {:ok, map()} | {:error, any()}
  @doc """
  Visualizes changes across related resources.

  ## Parameters
  * `change_group_id` - The ID of the change group to visualize

  ## Returns
  * `{:ok, visualization}` - The change visualization
  * `{:error, reason}` - Failed to create visualization
  """
  def visualize_change_group(change_group_id) do
    # Get the change group
    change_group = get_change_group(change_group_id)

    # Get all changes in the group
    changes = get_changes_for_group(change_group_id)

    # Find the primary change
    primary_change = Enum.find(changes, & &1.is_primary)

    # Find propagated changes
    propagated_changes = Enum.reject(changes, & &1.is_primary)

    # Build a relationship graph
    graph = build_relationship_graph(primary_change, propagated_changes)

    # Create a visual representation
    visualization = %{
      change_group: change_group,
      primary_change: primary_change,
      propagated_changes: propagated_changes,
      relationship_graph: graph,
      timestamp: change_group.inserted_at,
      total_changes: length(changes)
    }

    {:ok, visualization}
  end

  @doc """
  Gets a history of cross-resource changes.

  ## Parameters
  * `resource_type` - The type of resource
  * `resource_id` - The ID of the resource
  * `opts` - Additional options

  ## Returns
  * `{:ok, history}` - The change history
  * `{:error, reason}` - Failed to get history
  """
  def get_cross_resource_history(resource_type, resource_id, opts \\ []) do
    # Find all change groups where this resource was involved
    limit = Keyword.get(opts, :limit, 50)

    # Query to find change groups involving this resource
    change_group_ids =
      Change
      |> where([c], c.resource_type == ^resource_type and c.resource_id == ^resource_id)
      |> select([c], c.change_group_id)
      |> distinct(true)
      |> order_by([c], desc: c.inserted_at)
      |> limit(^limit)
      |> Repo.all()

    # Get details for each change group
    change_groups =
      Enum.map(change_group_ids, fn id ->
        get_change_group(id)
      end)

    # Get changes for each group
    changes_by_group =
      Enum.map(change_group_ids, fn id ->
        {id, get_changes_for_group(id)}
      end)
      |> Enum.into(%{})

    # Build history with related resources
    history =
      Enum.map(change_groups, fn group ->
        group_changes = Map.get(changes_by_group, group.id, [])
        primary_change = Enum.find(group_changes, & &1.is_primary)
        related_changes = Enum.reject(group_changes, & &1.is_primary)

        %{
          change_group: group,
          primary_change: primary_change,
          related_changes: related_changes,
          timestamp: group.inserted_at,
          related_resources:
            Enum.map(related_changes, fn change ->
              "#{change.resource_type}:#{change.resource_id}"
            end)
        }
      end)

    {:ok, history}
  end

  @doc """
  Tracks changes to multiple resources in a single operation.

  ## Parameters
  * `changes` - List of {resource_type, resource_id, changes} tuples
  * `user_id` - The ID of the user making the changes
  * `opts` - Additional options

  ## Returns
  * `{:ok, change_group}` - The changes were tracked successfully
  * `{:error, reason}` - Failed to track the changes
  """
  def track_multi_resource_changes(changes, user_id, opts \\ []) do
    # Create a unique ID for this change group
    change_group_id = Ecto.UUID.generate()
    correlation_id = Keyword.get(opts, :correlation_id, change_group_id)

    # Create the change group
    {:ok, change_group} =
      create_change_group(%{
        name: Keyword.get(opts, :name, "Multi-resource change"),
        description: Keyword.get(opts, :description, ""),
        user_id: user_id,
        correlation_id: correlation_id,
        metadata: Keyword.get(opts, :metadata, %{})
      })

    # Create change objects
    change_objects =
      Enum.map(changes, fn {resource_type, resource_id, changes} ->
        %{
          resource_type: resource_type,
          resource_id: resource_id,
          changes: changes,
          user_id: user_id,
          change_group_id: change_group.id,
          # No primary change in multi-resource changes
          is_primary: false,
          metadata: %{
            initiated_at: DateTime.utc_now(),
            change_type: Keyword.get(opts, :change_type, "update")
          }
        }
      end)

    # Process the change group in a transaction
    TransactionalEventStore.transaction_with_events(fn ->
      # Store all changes
      stored_changes =
        Enum.map(change_objects, fn change ->
          {:ok, stored_change} = store_change(change)
          stored_change
        end)

      # Create events for all changes
      events = create_events_for_changes(stored_changes, correlation_id)

      {:ok,
       %{
         change_group: change_group,
         changes: stored_changes
       }, events}
    end)
  end

  # Private helper functions

  defp create_change_group(attrs) do
    %ChangeGroup{}
    |> ChangeGroup.changeset(attrs)
    |> Repo.insert()
  end

  defp store_change(change) do
    %Change{}
    |> Change.changeset(change)
    |> Repo.insert()
  end

  defp get_change_group(id) do
    Repo.get!(ChangeGroup, id)
  end

  defp get_changes_for_group(change_group_id) do
    Change
    |> where([c], c.change_group_id == ^change_group_id)
    |> Repo.all()
  end

  defp create_propagated_changes(
         related_configs,
         source_type,
         source_id,
         source_changes,
         change_group_id,
         user_id
       ) do
    Enum.flat_map(related_configs, fn config ->
      # Extract configuration
      {target_type, relationship_type} = config[:resource]
      target_ids = config[:resource_ids]
      change_generator = config[:change_generator]

      # Generate changes for each target resource
      Enum.map(target_ids, fn target_id ->
        # Generate changes for this target
        target_changes =
          change_generator.(source_type, source_id, source_changes, target_type, target_id)

        # Create change object
        %{
          resource_type: target_type,
          resource_id: target_id,
          changes: target_changes,
          user_id: user_id,
          change_group_id: change_group_id,
          is_primary: false,
          metadata: %{
            initiated_at: DateTime.utc_now(),
            change_type: "propagated",
            source_resource_type: source_type,
            source_resource_id: source_id,
            relationship_type: relationship_type
          }
        }
      end)
    end)
  end

  defp create_events_for_changes(changes, correlation_id) do
    Enum.map(changes, fn change ->
      %Event{
        type:
          "resource.#{change.resource_type}.#{if change.is_primary, do: "changed", else: "propagated_change"}",
        resource_type: change.resource_type,
        resource_id: change.resource_id,
        data: change.changes,
        metadata:
          Map.merge(change.metadata, %{
            change_id: change.id,
            change_group_id: change.change_group_id,
            user_id: change.user_id
          }),
        correlation_id: correlation_id
      }
    end)
  end

  defp build_relationship_graph(primary_change, propagated_changes) do
    # Create nodes for the graph
    nodes =
      [
        %{
          id: "#{primary_change.resource_type}:#{primary_change.resource_id}",
          type: primary_change.resource_type,
          resource_id: primary_change.resource_id,
          is_primary: true
        }
      ] ++
        Enum.map(propagated_changes, fn change ->
          %{
            id: "#{change.resource_type}:#{change.resource_id}",
            type: change.resource_type,
            resource_id: change.resource_id,
            is_primary: false
          }
        end)

    # Create edges between nodes
    edges =
      Enum.map(propagated_changes, fn change ->
        source_type = get_in(change.metadata, ["source_resource_type"])
        source_id = get_in(change.metadata, ["source_resource_id"])
        relationship_type = get_in(change.metadata, ["relationship_type"])

        %{
          source: "#{source_type}:#{source_id}",
          target: "#{change.resource_type}:#{change.resource_id}",
          relationship: relationship_type
        }
      end)

    %{
      nodes: nodes,
      edges: edges
    }
  end
end
