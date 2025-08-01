unless Mix.env() == :test do
  defmodule Spacecast.MockExternalAPI do
    @moduledoc """
    Mock implementation of the ExternalAPI for testing.
    """

    @behaviour Spacecast.ExternalAPI

    @impl true
    def fetch_data(id) do
      {:ok,
       %{
         "id" => id,
         "name" => "Test Resource",
         "status" => "active",
         "type" => "test_type",
         "description" => "Test Description",
         "content" => %{
           "text" => "Test content"
         }
       }}
    end

    @impl true
    def update_resource(id, data) do
      {:ok,
       Map.merge(
         %{
           "id" => id,
           "name" => "Test Resource",
           "status" => "active",
           "type" => "test_type",
           "description" => "Test Description",
           "content" => %{
             "text" => "Test content"
           }
         },
         data
       )}
    end
  end
end
