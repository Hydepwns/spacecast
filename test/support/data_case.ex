defmodule Spacecast.DataCase do
  @moduledoc """
  This module defines the setup for tests requiring
  access to the application's data layer.

  You may define functions here to be used as helpers in
  your tests.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use Spacecast.DataCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      alias Spacecast.Repo

      import Ecto
      import Ecto.Changeset
      import Ecto.Query
      import Spacecast.DataCase
    end
  end

  setup(_tags) do
    # Always use manual mode for better control with LiveView processes
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Spacecast.Repo)
    Ecto.Adapters.SQL.Sandbox.mode(Spacecast.Repo, :manual)

    # Always allow the current process
    Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), self())

    :ok
  end

  @doc """
  Sets up the sandbox based on the test tags.
  """
    def setup_sandbox(tags) do
    # Use shared mode for async tests, manual for sync tests
    shared = tags[:async] || false

    # Try to start the sandbox owner
    try do
      pid = Ecto.Adapters.SQL.Sandbox.start_owner!(Spacecast.Repo, shared: shared)
      on_exit(fn -> Ecto.Adapters.SQL.Sandbox.stop_owner(pid) end)
      # Allow the current process to use the connection
      Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), pid)
      pid
    rescue
      e in RuntimeError ->
        # If it's an "already started" error, try to get the existing owner
        if String.contains?(e.message, "already started") or String.contains?(e.message, "already_shared") do
          # For shared mode, we can just allow the current process
          Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), self())
          self()
        else
          reraise e, __STACKTRACE__
        end
      e in MatchError ->
        # Handle various sandbox errors
        case e.term do
          {:error, {{:badmatch, :already_shared}, _}} ->
            # For shared mode, we can just allow the current process
            Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), self())
            self()
          {:error, {{:badmatch, {:already, :allowed}}, _}} ->
            # Sandbox is already allowed, continue
            self()
          {:error, {{:badmatch, :not_found}, _}} ->
            # Handle not_found error - try to use manual mode instead
            :ok = Ecto.Adapters.SQL.Sandbox.checkout(Spacecast.Repo)
            Ecto.Adapters.SQL.Sandbox.mode(Spacecast.Repo, :manual)
            Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), self())
            self()
          _ ->
            reraise e, __STACKTRACE__
        end
    end
  end

  @doc """
  A helper that transforms changeset errors into a map of messages.

      assert {:error, changeset} = Accounts.create_user(%{password: "short"})
      assert "password is too short" in errors_on(changeset).password
      assert %{password: ["password is too short"]} = errors_on(changeset)

  """
  def errors_on(changeset) do
    Ecto.Changeset.traverse_errors(changeset, fn {message, opts} ->
      Regex.replace(~r"%{(\w+)}", message, fn _, key ->
        opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
      end)
    end)
  end
end
