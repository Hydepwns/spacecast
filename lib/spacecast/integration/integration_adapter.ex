defmodule Spacecast.Integration.IntegrationAdapter do
  @moduledoc """
  Behavior defining the interface for integration adapters.

  Integration adapters are responsible for the actual communication with external systems.
  They handle the details of API calls, data formatting, authentication, and error handling
  specific to each external service.

  To implement a new adapter:
  1. Create a module that uses this behavior
  2. Implement all the callback functions
  3. Register it with the IntegrationBridge
  """

  @doc """
  Validates the adapter configuration.

  ## Parameters
  * `config` - The configuration map for the adapter

  ## Returns
  * `:ok` - Configuration is valid
  * `{:error, reason}` - Configuration is invalid with reason
  """
  @callback validate_config(config :: map()) :: :ok | {:error, term()}

  @doc """
  Fetches data from the external system.

  ## Parameters
  * `options` - Options for the fetch operation

  ## Returns
  * `{:ok, data}` - Successfully fetched data
  * `{:error, reason}` - Fetch failed
  """
  @callback fetch_data(options :: map()) :: {:ok, list(map())} | {:error, term()}

  @doc """
  Exports data to the external system.

  ## Parameters
  * `data` - List of data items to export
  * `options` - Options for the export operation

  ## Returns
  * `{:ok, external_ids}` - Successfully exported data with external IDs
  * `{:error, reason}` - Export failed
  """
  @callback export_data(data :: list(map()), options :: map()) ::
              {:ok, list(term())} | {:error, term()}

  @doc """
  Tests the connection to the external system.

  ## Parameters
  * `config` - The configuration map for the adapter

  ## Returns
  * `{:ok, response}` - Connection successful with response
  * `{:error, reason}` - Connection failed
  """
  @callback test_connection(config :: map()) :: {:ok, term()} | {:error, term()}

  @doc """
  Gets the schema for data mapping between the external system and internal resources.

  ## Returns
  * Map describing the field mappings
  """
  @callback get_mapping_schema() :: map()

  @optional_callbacks [test_connection: 1, get_mapping_schema: 0]
end
