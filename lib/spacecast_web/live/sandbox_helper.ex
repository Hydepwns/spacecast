defmodule SpacecastWeb.Live.SandboxHelper do
  @moduledoc """
  Helper module for managing LiveView sandbox connections in test environment.
  This module provides robust sandbox joining functionality to fix "unable to join" errors.
  """

  require Logger

  @doc """
  Attempts to join the LiveView sandbox using the provided sandbox PID string.
  Returns :ok on success or {:error, reason} on failure.
  """
  def join_sandbox(sandbox_pid_str) when is_binary(sandbox_pid_str) do
    try do
      case parse_sandbox_pid(sandbox_pid_str) do
        {:ok, pid} ->
          case Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, pid, self()) do
            :ok ->
              Logger.debug("✅ Successfully joined LiveView sandbox")
              :ok
            {:error, reason} ->
              Logger.debug("❌ Failed to join LiveView sandbox: #{inspect(reason)}")
              {:error, reason}
          end
        {:error, reason} ->
          Logger.debug("❌ Failed to parse sandbox PID: #{inspect(reason)}")
          {:error, reason}
      end
    rescue
      e ->
        Logger.debug("❌ Exception joining sandbox: #{inspect(e)}")
        {:error, "Failed to join sandbox: #{inspect(e)}"}
    end
  end

  def join_sandbox(_), do: {:error, "Invalid sandbox PID format"}

  @doc """
  Parses a sandbox PID string using multiple strategies for maximum compatibility.
  """
  def parse_sandbox_pid(pid_str) do
    cond do
      Regex.match?(~r/#PID<(\d+)\.(\d+)\.(\d+)>/, pid_str) ->
        parse_pid_with_regex(pid_str, ~r/#PID<(\d+)\.(\d+)\.(\d+)>/)

      # Format: "<0.123.0>"
      Regex.match?(~r/<(\d+)\.(\d+)\.(\d+)>/, pid_str) ->
        parse_pid_with_regex(pid_str, ~r/<(\d+)\.(\d+)\.(\d+)>/)

      # Try direct evaluation as fallback
      true ->
        try do
          {pid, _} = Code.eval_string(pid_str)
          if is_pid(pid), do: {:ok, pid}, else: {:error, "Not a PID: #{pid_str}"}
        rescue
          _ -> {:error, "Failed to evaluate PID: #{pid_str}"}
        end
    end
  end

  defp parse_pid_with_regex(pid_str, regex) do
    case Regex.run(regex, pid_str) do
      [_, node_id, process_id, serial] ->
        try do
          pid_str_parsed = "<#{node_id}.#{process_id}.#{serial}>"
          pid = :erlang.list_to_pid(String.to_charlist(pid_str_parsed))
          {:ok, pid}
        rescue
          _ -> {:error, "Failed to parse PID: #{pid_str}"}
        end
      _ ->
        {:error, "Invalid PID format: #{pid_str}"}
    end
  end

  @doc """
  Attempts to join sandbox from session or cookies data.
  """
  def join_sandbox_from_session(session, connect_info \\ %{}) do
    # Extract session and cookies from connect_info
    cookies = Map.get(connect_info, :cookies, %{})

    # Check for sandbox cookie in both session and cookies
    sandbox_cookie = Map.get(session, "_phoenix_liveview_sandbox") ||
                    Map.get(cookies, "_phoenix_liveview_sandbox")

    if sandbox_cookie do
      Logger.debug("🔍 Found sandbox cookie: #{inspect(sandbox_cookie)}")
      join_sandbox(sandbox_cookie)
    else
      Logger.debug("⚠️  No sandbox cookie found in session or cookies")
      {:error, "No sandbox cookie found"}
    end
  end

  @doc """
  Sets up sandbox connection for a LiveView process in test environment.
  This should be called from the mount function of LiveViews that need database access.
  """
  def setup_sandbox_connection(session, connect_info \\ %{}) do
    if Mix.env() == :test do
      case join_sandbox_from_session(session, connect_info) do
        :ok ->
          Logger.debug("✅ LiveView sandbox connection established")
          :ok
        {:error, reason} ->
          Logger.debug("⚠️  LiveView sandbox connection failed: #{inspect(reason)}")
          # Don't fail the mount, just log the warning
          :ok
      end
    else
      :ok
    end
  end
end
