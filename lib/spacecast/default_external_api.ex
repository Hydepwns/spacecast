unless Mix.env() == :test do
  defmodule Spacecast.DefaultExternalAPI do
    @moduledoc """
    Stub implementation of the DefaultExternalAPI for production/dev environments.
    Replace with real API integration as needed.
    """

    @doc """
    Fetches resource data by id. Not implemented in this stub.
    """
    def fetch_data(_id), do: {:error, :not_implemented}
  end
end
