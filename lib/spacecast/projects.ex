defmodule Spacecast.Projects do
  @moduledoc """
  Module for handling project-related functionality.
  """

  @doc """
  Returns a project diagram for visualization.
  """
  def get_project_diagram do
    %{
      nodes: [
        %{id: "start", label: "Start", type: "start"},
        %{id: "process", label: "Process", type: "process"},
        %{id: "end", label: "End", type: "end"}
      ],
      edges: [
        %{from: "start", to: "process"},
        %{from: "process", to: "end"}
      ]
    }
  end
end
