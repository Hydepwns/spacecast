defmodule Spacecast.RateLimitCleanup do
  use GenServer

  # 1 minute
  @cleanup_interval 60_000

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    schedule_cleanup()
    {:ok, %{}}
  end

  @impl true
  def handle_info(:cleanup, state) do
    SpacecastWeb.Plugs.RateLimitPlug.cleanup_old_entries()
    schedule_cleanup()
    {:noreply, state}
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @cleanup_interval)
  end
end
