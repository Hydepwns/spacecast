defmodule Spacecast.Utils.ResourceAdapter do
  @moduledoc """
  Defines a behavior for adapting different data sources to LiveViewResource.

  This module provides an abstraction layer that allows LiveViewResource to
  work with various data sources such as Ecto schemas, Ash resources, or
  custom data structures. It defines a common interface for validating,
  loading, and saving data across different persistence mechanisms.

  ## Adapters

  The following adapters are implemented:

  - `Spacecast.Utils.EctoAdapter` - For Ecto schemas
  - `Spacecast.Utils.AshAdapter` - For Ash resources (planned)
  - `Spacecast.Utils.MemoryAdapter` - For in-memory data (for testing)

  ## Usage

  ```elixir
  defmodule MyApp.UserResource do
    use Spacecast.Utils.LiveViewResource
    
    # Use the EctoAdapter with the User schema
    adapter Spacecast.Utils.EctoAdapter, schema: MyApp.User
    
    # Alternatively, define attributes manually (falls back to this if no adapter)
    attributes do
      attribute :id, :string, required: true
      attribute :name, :string, required: true
    end
  end
  ```
  """

  @type resource :: module()
  @type schema :: module()
  @type data_source :: module()
  @type validation_result :: {:ok, map()} | {:error, String.t()}
  @type load_result :: {:ok, map()} | {:error, String.t()}
  @type save_result :: {:ok, map()} | {:error, String.t()}

  @doc """
  Generates type specifications from a data source schema.

  This callback should extract type information from the underlying data source
  and convert it to a format that can be used by LiveViewResource for validation.
  """
  @callback generate_type_specs(data_source) :: map()

  @doc """
  Validates a map of values against a data source schema.

  This callback should validate that the provided values conform to the
  constraints of the underlying data source schema.
  """
  @callback validate(data_source, map()) :: validation_result

  @doc """
  Loads data from the data source based on a primary key.

  This callback should retrieve data from the underlying data source
  based on the provided primary key value.
  """
  @callback load(data_source, term()) :: load_result

  @doc """
  Saves data to the data source.

  This callback should persist the provided data to the underlying data source.
  """
  @callback save(data_source, map()) :: save_result

  @doc """
  Gets the module for a specific adapter.
  """
  @spec get_adapter(atom()) :: module()
  def get_adapter(:ecto), do: Spacecast.Utils.EctoAdapter
  def get_adapter(:ash), do: Spacecast.Utils.AshAdapter
  def get_adapter(:memory), do: Spacecast.Utils.MemoryAdapter

  @doc """
  Extracts attribute information from a data source schema.

  This helper function can be used by adapter implementations to extract
  attribute information from different types of schemas.
  """
  @spec extract_attributes(module(), atom()) :: [map()]
  def extract_attributes(schema, adapter_type) do
    adapter = get_adapter(adapter_type)
    adapter.extract_attributes(schema)
  end

  @doc """
  Validates a map of values against a data source schema.

  This helper function can be used to validate values against a schema
  using the appropriate adapter.
  """
  @spec validate(module(), map(), atom()) :: validation_result
  def validate(schema, values, adapter_type) do
    adapter = get_adapter(adapter_type)
    adapter.validate(schema, values)
  end

  @doc """
  Loads data from a data source based on a primary key.

  This helper function can be used to load data from a schema
  using the appropriate adapter.
  """
  @spec load(module(), term(), atom()) :: load_result
  def load(schema, id, adapter_type) do
    adapter = get_adapter(adapter_type)
    adapter.load(schema, id)
  end

  @doc """
  Saves data to a data source.

  This helper function can be used to save data to a schema
  using the appropriate adapter.
  """
  @spec save(module(), map(), atom()) :: save_result
  def save(schema, values, adapter_type) do
    adapter = get_adapter(adapter_type)
    adapter.save(schema, values)
  end
end
