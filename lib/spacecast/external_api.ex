defmodule Spacecast.ExternalAPI do
  @moduledoc """
  Behaviour for external API integration.
  """

  @callback fetch_data(id :: String.t()) :: {:ok, map()} | {:error, term()}
  @callback update_resource(id :: String.t(), data :: map()) :: {:ok, map()} | {:error, term()}
end
