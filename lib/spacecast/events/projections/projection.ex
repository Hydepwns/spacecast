defmodule Spacecast.Events.Projections.Projection do
  @moduledoc """
  Behavior for event projections that build derived state from events.

  Projections transform event streams into derived state models. They are
  an essential part of event sourcing and provide a way to build different
  views of the data based on the event history.

  ## Example

  ```elixir
  defmodule Spacecast.Events.Projections.UserStats do
    use Spacecast.Events.Projection

    @impl true
    def init do
      {:ok, %{
        total_users: 0,
        active_users: 0,
        logins_today: 0
      }}
    end

    @impl true
    def interested_in do
      [:user_created, :user_login, :user_logout]
    end

    @impl true
    def apply_event(%{type: :user_created}, state) do
      {:ok, %{state | total_users: state.total_users + 1}}
    end

    @impl true
    def apply_event(%{type: :user_login}, state) do
      {:ok, %{state |
        active_users: state.active_users + 1,
        logins_today: state.logins_today + 1
      }}
    end

    @impl true
    def apply_event(%{type: :user_logout}, state) do
      {:ok, %{state | active_users: state.active_users - 1}}
    end

    @impl true
    def get_state do
      current_state()
    end
  end
  """

  @doc """
  Initializes the projection state.

  ## Returns

  * `{:ok, state}` - The initial state of the projection
  * `{:error, reason}` - An error occurred during initialization
  """
  @callback init() :: {:ok, state :: term()} | {:error, reason :: term()}

  @doc """
  Applies an event to the projection state.

  ## Parameters

  * `event` - The event to apply
  * `state` - The current state of the projection

  ## Returns

  * `{:ok, new_state}` - The updated state after applying the event
  * `{:error, reason}` - An error occurred when applying the event
  """
  @callback apply_event(event :: map(), state :: term()) ::
              {:ok, new_state :: term()} | {:error, reason :: term()}

  @doc """
  Returns the current state of the projection.

  ## Returns

  * The current state of the projection
  """
  @callback get_state() :: term()

  @doc """
  Returns the event types that this projection is interested in.

  ## Returns

  * `[atom()]` - A list of event types
  * `:all` - All event types
  """
  @callback interested_in() :: [atom()] | :all

  defmacro __using__(_opts) do
    quote do
      @behaviour Spacecast.Events.Projections.Projection
      require Logger

      # GenServer name for this projection
      @server_name __MODULE__

      @doc """
      Starts the projection process.

      ## Options

      * `:name` - The name to register the projection under. Defaults to the module name.
      """
      def start_link(opts \\ []) do
        name = Keyword.get(opts, :name, @server_name)
        Spacecast.Events.ProjectionProcess.start_link(__MODULE__, opts)
      end

      @doc """
      Returns the current state of the projection.

      ## Returns

      * The current state of the projection
      * `nil` - The projection is not running
      """
      def current_state do
        try do
          GenServer.call(@server_name, :get_state)
        catch
          :exit, _ ->
            # Process not running
            Logger.warning("Projection \\#{inspect(__MODULE__)} is not running")
            nil
        end
      end

      @doc """
      Applies an event to the projection.

      ## Parameters

      * `event` - The event to apply

      ## Returns

      * `:ok` - The event was applied successfully
      * `{:error, reason}` - An error occurred
      """
      def apply(event) do
        GenServer.call(@server_name, {:apply_event, event})
      end

      @doc """
      Resets the projection to its initial state.

      ## Returns

      * `:ok` - The projection was reset successfully
      * `{:error, reason}` - An error occurred
      """
      def reset do
        GenServer.call(@server_name, :reset)
      end

      @doc """
      Rebuilds the projection from all events.

      ## Returns

      * `:ok` - The projection was rebuilt successfully
      * `{:error, reason}` - An error occurred
      """
      def rebuild do
        GenServer.call(@server_name, :rebuild)
      end

      # Default implementation that handles all events
      def interested_in, do: :all

      defoverridable interested_in: 0
    end
  end
end
