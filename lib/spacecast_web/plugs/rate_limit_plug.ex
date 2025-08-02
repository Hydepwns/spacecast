defmodule SpacecastWeb.Plugs.RateLimitPlug do
  @moduledoc """
  Rate limiting plug for controlling request frequency and preventing abuse.
  """

  import Plug.Conn
  import Phoenix.Controller

  # Rate limit configuration - more aggressive for testing
  @requests_per_minute 10
  @requests_per_hour 100

  def init(opts), do: opts

  def call(conn, _opts) do
    # Skip rate limiting in test environment
    if Mix.env() == :test do
      conn
    else
      client_id = get_client_id(conn)

      case check_rate_limit(client_id) do
        :ok ->
          conn

        {:error, :rate_limit_exceeded} ->
          conn
          |> put_status(:too_many_requests)
          |> put_resp_header("retry-after", "60")
          |> json(%{error: "Rate limit exceeded. Please try again later."})
          |> halt()
      end
    end
  end

  defp get_client_id(conn) do
    # Try to get from X-Forwarded-For header first (for proxied requests)
    case get_req_header(conn, "x-forwarded-for") do
      [forwarded_for | _] ->
        # Take the first IP from the list
        forwarded_for
        |> String.split(",")
        |> List.first()
        |> String.trim()

      [] ->
        # Fall back to remote address
        conn.remote_ip
        |> :inet.ntoa()
        |> to_string()
    end
  end

  defp check_rate_limit(client_id) do
    _table_name = :rate_limits

    # Assume table exists (created at app startup)
    now = System.system_time(:second)
    minute = div(now, 60)
    hour = div(now, 3600)
    minute_key = {client_id, :minute, minute}
    hour_key = {client_id, :hour, hour}
    minute_count = get_request_count(minute_key)
    hour_count = get_request_count(hour_key)

    if minute_count >= @requests_per_minute or hour_count >= @requests_per_hour do
      {:error, :rate_limit_exceeded}
    else
      increment_request_count(minute_key)
      increment_request_count(hour_key)
      :ok
    end
  end

  defp get_request_count(key) do
    case :ets.lookup(:rate_limits, key) do
      [{^key, count, _timestamp}] -> count
      [] -> 0
    end
  end

  defp increment_request_count(key) do
    now = System.system_time(:second)

    case :ets.lookup(:rate_limits, key) do
      [{^key, count, _timestamp}] -> :ets.insert(:rate_limits, {key, count + 1, now})
      [] -> :ets.insert(:rate_limits, {key, 1, now})
    end
  end

  # Clean up old entries periodically
  def cleanup_old_entries do
    table_name = :rate_limits

    case :ets.info(table_name) do
      :undefined ->
        :ok

      _ ->
        now = System.system_time(:second)
        # Keep last 2 minutes
        cutoff_minute = div(now, 60) - 2
        # Keep last 2 hours
        cutoff_hour = div(now, 3600) - 2

        :ets.select_delete(table_name, [
          {{:_, :_, :"$1"}, [{:<, :"$1", cutoff_minute * 60}], [true]},
          {{:_, :_, :"$1"}, [{:<, :"$1", cutoff_hour * 3600}], [true]}
        ])
    end
  end
end
