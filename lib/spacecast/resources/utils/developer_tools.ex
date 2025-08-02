defmodule Spacecast.Resources.DeveloperTools do
  @moduledoc """
  Developer tools for the resource system.

  This module provides utilities for developers working with the resource system, including:
  - Interactive debugging tools for resource events
  - Resource state inspection and manipulation
  - Testing utilities for resource workflows
  - Development mode features for faster iteration
  - Diagnostic tools for event-sourced resources
  - Simulation tools for load and stress testing

  ## Usage

  This module now delegates to specialized modules for better organization:

  - `Spacecast.Resources.DebugSession` - Debug session management
  - `Spacecast.Resources.ResourceSandbox` - Sandbox environment for testing
  - `Spacecast.Resources.LoadTester` - Load testing functionality
  - `Spacecast.Resources.EventVisualizer` - Event flow visualization
  - `Spacecast.Resources.EventDocumentation` - Event documentation generation
  - `Spacecast.Resources.TestEventGenerator` - Test event generation
  """

  # Re-export functions from specialized modules for backward compatibility

  # Debug Session functions
  defdelegate start_debug_session(resource_module, id, opts \\ []),
    to: Spacecast.Resources.DebugSession,
    as: :start_session

  defdelegate get_debug_session(session_id), to: Spacecast.Resources.DebugSession, as: :get_session
  defdelegate create_debug_snapshot(session_id, label), to: Spacecast.Resources.DebugSession, as: :create_snapshot

  defdelegate compare_debug_snapshots(session_id, snapshot1_index, snapshot2_index),
    to: Spacecast.Resources.DebugSession,
    as: :compare_snapshots

  defdelegate set_breakpoint(session_id, event_type, condition \\ nil), to: Spacecast.Resources.DebugSession
  defdelegate handle_debug_event(event, session_id, pid), to: Spacecast.Resources.DebugSession

  # Resource Sandbox functions
  defdelegate create_resource_sandbox(resource_modules, opts \\ []),
    to: Spacecast.Resources.ResourceSandbox,
    as: :create_sandbox

  defdelegate apply_sandbox_event(sandbox_id, resource_type, instance_id, event),
    to: Spacecast.Resources.ResourceSandbox,
    as: :apply_event

  # Load Testing functions
  defdelegate run_load_test(resource_module, operation_fn, opts \\ []), to: Spacecast.Resources.LoadTester

  # Event Visualization functions
  defdelegate visualize_event_flow(resource_module, id, opts \\ []), to: Spacecast.Resources.EventVisualizer

  # Event Documentation functions
  defdelegate generate_event_documentation(resource_module),
    to: Spacecast.Resources.EventDocumentation,
    as: :generate_documentation

  # Test Event Generation functions
  defdelegate generate_test_events(resource_module, id, event_specs, opts \\ []),
    to: Spacecast.Resources.TestEventGenerator

  @doc """
  Convenience function to get a quick overview of all developer tools.

  Returns a map with information about available tools and their purposes.
  """
  def tools_overview do
    %{
      debug_session: %{
        description: "Interactive debugging for resource events",
        module: Spacecast.Resources.DebugSession,
        functions: [
          :start_session,
          :get_session,
          :create_snapshot,
          :compare_snapshots,
          :set_breakpoint,
          :remove_breakpoint,
          :toggle_breakpoint,
          :list_breakpoints,
          :step_through_events,
          :replay_events,
          :export_session,
          :import_session
        ]
      },
      resource_sandbox: %{
        description: "Isolated testing environment for resources",
        module: Spacecast.Resources.ResourceSandbox,
        functions: [
          :create_sandbox,
          :get_sandbox,
          :apply_event,
          :get_instance_state,
          :list_instances,
          :reset_sandbox
        ]
      },
      load_tester: %{
        description: "Load and stress testing for resource systems",
        module: Spacecast.Resources.LoadTester,
        functions: [
          :run_load_test,
          :run_stress_test,
          :run_endurance_test
        ]
      },
      event_visualizer: %{
        description: "Event flow visualization and analysis",
        module: Spacecast.Resources.EventVisualizer,
        functions: [
          :visualize_event_flow,
          :create_event_timeline,
          :create_event_graph,
          :analyze_event_patterns,
          :export_visualization
        ]
      },
      event_documentation: %{
        description: "Automatic event documentation generation",
        module: Spacecast.Resources.EventDocumentation,
        functions: [
          :generate_documentation,
          :generate_markdown_documentation,
          :generate_json_schema,
          :generate_event_catalog,
          :validate_event_against_schema
        ]
      },
      test_event_generator: %{
        description: "Test event generation for resource testing",
        module: Spacecast.Resources.TestEventGenerator,
        functions: [
          :generate_test_events,
          :generate_event_sequence,
          :generate_random_events,
          :generate_scenario_events,
          :validate_event_specs
        ]
      }
    }
  end

  @doc """
  Gets help information for a specific tool or function.

  ## Parameters
  * `tool_name` - The name of the tool (atom)
  * `function_name` - Optional function name (atom)

  ## Returns
  * `{:ok, help_info}` - Help information
  * `{:error, :not_found}` - Tool or function not found
  """
  def help(tool_name, function_name \\ nil) do
    overview = tools_overview()

    case Map.get(overview, tool_name) do
      nil ->
        {:error, :tool_not_found}

      tool_info ->
        if function_name do
          # Get specific function help
          module = tool_info.module

          case Code.ensure_loaded(module) do
            {:module, ^module} ->
              case module.__info__(:functions) do
                functions when is_list(functions) ->
                  if function_name in Enum.map(functions, &elem(&1, 0)) do
                    {:ok,
                     %{
                       tool: tool_name,
                       function: function_name,
                       module: module,
                       description: "Use `#{inspect(module)}.#{function_name}/arity` for detailed information"
                     }}
                  else
                    {:error, :function_not_found}
                  end

                _ ->
                  {:error, :function_not_found}
              end

            _ ->
              {:error, :module_not_loaded}
          end
        else
          # Get tool overview
          {:ok, tool_info}
        end
    end
  end
end
