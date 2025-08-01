defmodule Spacecast.Events.Events do
  @moduledoc false

  def change_event_reminder(_), do: %{}
  def get_event_reminder!(_), do: %{}
  def update_event_reminder(_reminder, _params), do: {:ok, %{}}
  def create_event_reminder(_params), do: {:ok, %{}}
  def list_events, do: []
  def change_event_settings(_), do: %{}
  def get_event_settings!(_), do: %{}
  def update_event_settings(_settings, _params), do: {:ok, %{}}
  def create_event_settings(_params), do: {:ok, %{}}
  def get_events(_params), do: {:ok, []}
  def create_event(_type, _params), do: {:ok, %{}}
end
