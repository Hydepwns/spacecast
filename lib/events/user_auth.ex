defmodule UserAuth do
  @moduledoc """
  This module is responsible for managing the user authentication system.
  """

  use GenServer
  require Logger

  # Client API
  @spec start_link(any()) :: :ignore | {:error, any()} | {:ok, pid()}
  @spec start_link(any()) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @spec init(any()) :: {:ok, any()}
  def init(opts) do
    {:ok, %{opts: opts}}
  end

  @spec get_events_for_resource(String.t() | atom(), String.t()) :: {:ok, [map()]} | {:error, any()}
  def get_events_for_resource(resource_type, resource_id) do
    result = Spacecast.Events.EventStore.get_events_for_resource(resource_type, resource_id)

    case result do
      {:ok, events} ->
        {:ok, events}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @spec handle_call({:get_events_for_resource, String.t() | atom(), String.t()}, {pid(), any()}, map()) ::
          {:reply, {:ok, [map()]} | {:error, any()}, map()}
  def handle_call({:get_events_for_resource, resource_type, resource_id}, _from, state) do
    result = Spacecast.Events.EventStore.get_events_for_resource(resource_type, resource_id)
    {:reply, result, state}
  end
end
