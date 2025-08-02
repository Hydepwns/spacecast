defmodule Spacecast.Utils.Logger do
  require Logger

  @spec warning(any()) :: :ok
  def warning(message) do
    Logger.warning(message)
  end
end
