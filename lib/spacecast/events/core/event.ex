defmodule Spacecast.Events.Core.Event do
  @moduledoc """
  Schema and struct for representing events in the system.

  Events are immutable records of things that have happened in the system.
  They are the core building blocks of the event-driven architecture.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @primary_key {:id, :binary_id, autogenerate: false}
  @timestamps_opts [type: :utc_datetime_usec]
  schema "events" do
    field :type, :string
    field :resource_id, :string
    field :resource_type, :string
    field :data, :map, default: %{}
    field :metadata, :map, default: %{}
    field :correlation_id, :binary_id
    field :causation_id, :binary_id
    field :timestamp, :utc_datetime_usec, default: nil

    timestamps()
  end

  @type t :: %__MODULE__{
          id: Ecto.UUID.t() | binary(),
          type: String.t(),
          resource_id: String.t(),
          resource_type: String.t(),
          data: map(),
          metadata: map(),
          correlation_id: Ecto.UUID.t() | binary(),
          causation_id: Ecto.UUID.t() | binary() | nil,
          timestamp: DateTime.t() | nil,
          inserted_at: NaiveDateTime.t() | nil,
          updated_at: NaiveDateTime.t() | nil
        }

  @doc """
  Creates a changeset for an event.

  ## Parameters

  * `event` - The event struct or changeset
  * `attrs` - Map of event attributes to change

  ## Returns

  * `changeset` - The changeset for the event
  """
  def changeset(event, attrs) when is_map(attrs) do
    # Pre-process attributes to set default IDs if not provided
    # Convert to atom keys first, then back to string keys to maintain consistency
    attrs =
      attrs
      |> convert_to_atom_keys()
      |> set_default_ids()
      |> convert_to_string_keys()

    event
    |> cast(attrs, [
      :id,
      :type,
      :resource_id,
      :resource_type,
      :data,
      :metadata,
      :correlation_id,
      :causation_id,
      :timestamp
    ])
    |> validate_length(:type, min: 3)
    |> validate_length(:resource_id, min: 1)
    |> validate_length(:resource_type, min: 1)
    |> validate_required([:type, :resource_id, :resource_type, :timestamp])
    |> validate_data()
    |> validate_metadata()
    |> validate_timestamp()
  end

  def changeset(_event, _invalid_attrs), do: {:error, :invalid_attributes}

  @doc """
  Creates a new event struct.

  ## Parameters

  * `type` - The type of event (required)
  * `attrs` - Map of event attributes
    * `:resource_id` - ID of the resource this event relates to (required)
    * `:resource_type` - Type of the resource this event relates to (required)
    * `:data` - The event payload data (default: %{})
    * `:metadata` - Additional metadata about the event (default: %{})
    * `:correlation_id` - ID to correlate related events (default: new UUID)
    * `:causation_id` - ID of the event that caused this one (default: nil)
    * `:timestamp` - When the event occurred (default: now)

  ## Returns

  * `{:ok, event}` - The event was created successfully
  * `{:error, changeset}` - The event failed validation
  """
  @spec create(String.t(), map()) :: {:ok, __MODULE__.t()} | {:error, Ecto.Changeset.t()}
  def create(type, attrs \\ %{})

  def create(type, attrs)
      when is_binary(type) and byte_size(type) >= 3 and
             is_map(attrs) do
    # Pre-process attributes
    attrs = Map.put(attrs, :type, type)
    attrs = set_default_timestamp(attrs)
    attrs = set_default_ids(attrs)
    attrs = for {k, v} <- attrs, into: %{}, do: {to_string(k), v}

    %__MODULE__{}
    |> cast(attrs, [
      :id,
      :type,
      :resource_id,
      :resource_type,
      :data,
      :metadata,
      :correlation_id,
      :causation_id,
      :timestamp
    ])
    |> validate_required([:type, :resource_id, :resource_type, :timestamp])
    |> validate_length(:type, min: 3)
    |> validate_length(:resource_id, min: 1)
    |> validate_length(:resource_type, min: 1)
    |> validate_data()
    |> validate_metadata()
    |> validate_timestamp()
    |> apply_action(:create)
  end

  def create(_invalid_type, _invalid_attrs), do: {:error, :invalid_parameters}

  @doc """
  Creates a new event struct, raising an error if validation fails.

  See `create/2` for parameters.

  ## Returns

  * `event` - The event struct

  ## Raises

  * `Ecto.InvalidChangesetError` - If the event is invalid
  """
  @spec create!(String.t(), map()) :: __MODULE__.t()
  def create!(type, attrs \\ %{})

  def create!(type, attrs)
      when is_binary(type) and byte_size(type) >= 3 and
             is_map(attrs) do
    case create(type, attrs) do
      {:ok, event} ->
        event

      {:error, changeset} ->
        raise Ecto.InvalidChangesetError, action: :create, changeset: changeset
    end
  end

  def create!(_invalid_type, _invalid_attrs), do: raise(ArgumentError, "Invalid event parameters")

  @doc """
  Creates a follow-up event that preserves correlation context.

  This is useful for creating events that are causally related to another event.

  ## Parameters

  * `original_event` - The event that this new event follows from
  * `type` - The type of the new event
  * `attrs` - Additional attributes for the new event

  ## Returns

  * `{:ok, event}` - The event was created successfully
  * `{:error, changeset}` - The event failed validation
  """
  @spec create_follow_up(__MODULE__.t(), String.t(), map()) ::
          {:ok, __MODULE__.t()} | {:error, Ecto.Changeset.t()}
  def create_follow_up(original_event, type, attrs \\ %{}) do
    # Maintain the correlation ID but set the causation ID to the original event's ID
    attrs =
      Map.merge(attrs, %{
        correlation_id: original_event.correlation_id,
        causation_id: original_event.id
      })

    create(type, attrs)
  end

  # Private functions

  defp set_default_timestamp(attrs) do
    case Map.get(attrs, :timestamp) do
      nil -> Map.put(attrs, :timestamp, DateTime.utc_now())
      _ -> attrs
    end
  end

  defp set_default_ids(attrs) do
    attrs
    |> set_default_id()
    |> set_default_correlation_id()
    |> set_default_causation_id()
  end

  defp set_default_id(attrs) do
    case Map.get(attrs, :id) do
      nil -> Map.put(attrs, :id, Ecto.UUID.generate())
      id when is_binary(id) and byte_size(id) > 0 -> attrs
      _ -> Map.put(attrs, :id, Ecto.UUID.generate())
    end
  end

  defp set_default_correlation_id(attrs) do
    case Map.get(attrs, :correlation_id) do
      nil -> Map.put(attrs, :correlation_id, Ecto.UUID.generate())
      _ -> attrs
    end
  end

  defp set_default_causation_id(attrs) do
    case Map.get(attrs, :causation_id) do
      nil -> attrs
      _ -> attrs
    end
  end

  defp validate_data(changeset) do
    case get_change(changeset, :data) do
      nil -> changeset
      data when is_map(data) -> changeset
      _ -> add_error(changeset, :data, "must be a map")
    end
  end

  defp validate_metadata(changeset) do
    case get_change(changeset, :metadata) do
      nil -> changeset
      metadata when is_map(metadata) -> changeset
      _ -> add_error(changeset, :metadata, "must be a map")
    end
  end

  defp validate_timestamp(changeset) do
    case get_change(changeset, :timestamp) do
      nil -> put_change(changeset, :timestamp, DateTime.utc_now())
      timestamp when is_struct(timestamp, DateTime) -> changeset
      _ -> add_error(changeset, :timestamp, "must be a DateTime")
    end
  end

  defp convert_to_atom_keys(attrs) do
    for {k, v} <- attrs, into: %{}, do: {to_atom(k), v}
  end

  defp convert_to_string_keys(attrs) do
    for {k, v} <- attrs, into: %{}, do: {to_string(k), v}
  end

  defp to_atom(key) when is_atom(key), do: key
  defp to_atom(key) when is_binary(key), do: String.to_existing_atom(key)
  defp to_atom(key), do: to_string(key) |> String.to_existing_atom()
end
