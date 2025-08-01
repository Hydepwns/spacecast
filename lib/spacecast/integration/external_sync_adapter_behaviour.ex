defmodule Spacecast.Integration.ExternalSyncAdapterBehaviour do
  @moduledoc """
  Behaviour for external sync adapter implementations.
  """

  @callback sync_resource(map()) :: {:ok, map()} | {:error, term()}
  @callback handle_sync_conflict(map(), map(), String.t()) :: {:ok, map()} | {:error, term()}
  @callback incremental_sync(DateTime.t()) :: {:ok, list()} | {:error, term()}
  @callback handle_sync_failure(function(), non_neg_integer()) :: {:ok, map()} | {:error, term()}
end
