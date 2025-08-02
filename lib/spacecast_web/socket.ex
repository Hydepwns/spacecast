defmodule SpacecastWeb.Socket do
  use Phoenix.Socket

  def connect(params, socket, connect_info) do
    if Mix.env() == :test do
      case handle_test_connection(params, socket, connect_info) do
        {:ok, socket} -> {:ok, socket}
        {:error, reason} -> {:error, reason}
      end
    else
      {:ok, socket}
    end
  end

  def id(socket), do: "user_socket:#{socket.assigns[:user_id] || "anonymous"}"

  defp handle_test_connection(_params, socket, connect_info) do
    session = Map.get(connect_info, :session, %{})
    cookies = Map.get(connect_info, :cookies, %{})

    sandbox_cookie = Map.get(session, "_phoenix_liveview_sandbox") ||
                    Map.get(cookies, "_phoenix_liveview_sandbox")

    if sandbox_cookie do
      case join_sandbox(sandbox_cookie) do
        :ok ->
          {:ok, socket}
        {:error, reason} ->
          {:error, reason}
      end
    else
      {:ok, socket}
    end
  end

  defp join_sandbox(sandbox_pid_str) when is_binary(sandbox_pid_str) do
    try do
      case parse_sandbox_pid(sandbox_pid_str) do
        {:ok, pid} ->
          case Ecto.Adapters.SQL.Sandbox.allow(Spacecast.Repo, self(), pid) do
            :ok -> :ok
            {:error, reason} -> {:error, reason}
          end
        {:error, reason} ->
          {:error, reason}
      end
    rescue
      e ->
        {:error, "Failed to join sandbox: #{inspect(e)}"}
    end
  end

  defp join_sandbox(_), do: {:error, "Invalid sandbox PID format"}

  defp parse_sandbox_pid(pid_str) do
    cond do
      Regex.match?(~r/#PID<(\d+)\.(\d+)\.(\d+)>/, pid_str) ->
        case Regex.run(~r/#PID<(\d+)\.(\d+)\.(\d+)>/, pid_str) do
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

      Regex.match?(~r/<(\d+)\.(\d+)\.(\d+)>/, pid_str) ->
        case Regex.run(~r/<(\d+)\.(\d+)\.(\d+)>/, pid_str) do
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

      true ->
        try do
          {pid, _} = Code.eval_string(pid_str)
          if is_pid(pid), do: {:ok, pid}, else: {:error, "Not a PID: #{pid_str}"}
        rescue
          _ -> {:error, "Failed to evaluate PID: #{pid_str}"}
        end
    end
  end
end
