defmodule SpacecastWeb.Examples.CodeFlowVisualizerLive do
  use SpacecastWeb, :live_view
  import Phoenix.Component
  alias SpacecastWeb.Components.MonoGrid

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket,
      code: sample_code(),
      execution_path: [],
      current_line: 0,
      variables: %{},
      call_stack: [],
      is_running: false,
      execution_speed: 1000,
      breakpoints: MapSet.new()
    )}
  end

  @impl true
  def handle_event("step", _params, socket) do
    if socket.assigns.current_line < length(socket.assigns.code) do
      new_state = execute_step(socket.assigns)
      {:noreply, assign(socket, new_state)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("run", _params, socket) do
    if socket.assigns.is_running do
      {:noreply, assign(socket, is_running: false)}
    else
      if connected?(socket) do
        :timer.send_interval(socket.assigns.execution_speed, self(), :auto_step)
      end
      {:noreply, assign(socket, is_running: true)}
    end
  end

  @impl true
  def handle_event("reset", _params, socket) do
    {:noreply, assign(socket,
      execution_path: [],
      current_line: 0,
      variables: %{},
      call_stack: [],
      is_running: false
    )}
  end

  @impl true
  def handle_event("set_speed", %{"speed" => speed}, socket) do
    speed = String.to_integer(speed)
    {:noreply, assign(socket, execution_speed: speed)}
  end

  @impl true
  def handle_event("toggle_breakpoint", %{"line" => line}, socket) do
    line = String.to_integer(line)
    breakpoints = if MapSet.member?(socket.assigns.breakpoints, line) do
      MapSet.delete(socket.assigns.breakpoints, line)
    else
      MapSet.put(socket.assigns.breakpoints, line)
    end
    {:noreply, assign(socket, breakpoints: breakpoints)}
  end

  @impl true
  def handle_info(:auto_step, socket) do
    if socket.assigns.is_running and socket.assigns.current_line < length(socket.assigns.code) do
      new_state = execute_step(socket.assigns)

      # Check for breakpoints
      if MapSet.member?(socket.assigns.breakpoints, new_state.current_line) do
        {:noreply, assign(socket, Map.merge(new_state, %{is_running: false}))}
      else
        {:noreply, assign(socket, new_state)}
      end
    else
      if socket.assigns.current_line >= length(socket.assigns.code) do
        {:noreply, assign(socket, is_running: false)}
      else
        {:noreply, socket}
      end
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div class="code-flow-visualizer">
      <div class="visualizer-header">
        <h1>Code Flow Visualizer</h1>
        <div class="execution-controls">
          <button phx-click="step" class="btn btn-secondary" disabled={@is_running}>
            Step
          </button>
          <button phx-click="run" class="btn btn-primary">
            <%= if @is_running, do: "Pause", else: "Run" %>
          </button>
          <button phx-click="reset" class="btn btn-secondary">Reset</button>

          <div class="speed-control">
            <label>Speed:</label>
            <select phx-change="set_speed" value={@execution_speed}>
              <option value="500">Fast</option>
              <option value="1000">Normal</option>
              <option value="2000">Slow</option>
              <option value="5000">Very Slow</option>
            </select>
          </div>
        </div>
      </div>

      <MonoGrid.grid cols={120} rows={30} bordered class="flow-grid">
        <!-- Code Display -->
        <MonoGrid.cell row={1} col={1} colspan={60} rowspan={25}>
          <div class="code-display">
            <h3>Source Code</h3>
            <div class="code-container">
              <%= for {line, idx} <- Enum.with_index(@code) do %>
                <div class={"code-line #{if idx == @current_line, do: "highlighted"}"}
                     phx-click="toggle_breakpoint"
                     phx-value-line={idx}>
                  <span class="line-number"><%= String.pad_leading("#{idx + 1}", 3) %></span>
                  <span class="breakpoint #{if MapSet.member?(@breakpoints, idx), do: 'active'}">●</span>
                  <span class="code-content"><%= line %></span>
                </div>
              <% end %>
            </div>
          </div>
        </MonoGrid.cell>

        <!-- Execution Flow -->
        <MonoGrid.cell row={1} col={61} colspan={60} rowspan={25}>
          <div class="flow-visualization">
            <h3>Execution Flow</h3>
            <div class="flow-container">
              <%= for {step, idx} <- Enum.with_index(Enum.reverse(@execution_path)) do %>
                <div class="flow-step">
                  <span class="step-number">#<%= length(@execution_path) - idx %></span>
                  <span class="step-line">Line <%= step.line + 1 %></span>
                  <span class="step-action"><%= step.action %></span>
                  <%= if step.variable_changes != %{} do %>
                    <div class="variable-changes">
                      <%= for {var, value} <- step.variable_changes do %>
                        <span class="var-change"><%= var %> = <%= value %></span>
                      <% end %>
                    </div>
                  <% end %>
                </div>
              <% end %>
            </div>
          </div>
        </MonoGrid.cell>

        <!-- Variables Panel -->
        <MonoGrid.cell row={26} col={1} colspan={60} rowspan={4}>
          <div class="variables-panel">
            <h3>Variables</h3>
            <div class="variables-list">
              <%= for {name, value} <- @variables do %>
                <div class="variable-item">
                  <span class="var-name"><%= name %></span>
                  <span class="var-value"><%= inspect(value) %></span>
                </div>
              <% end %>
            </div>
          </div>
        </MonoGrid.cell>

        <!-- Call Stack -->
        <MonoGrid.cell row={26} col={61} colspan={60} rowspan={4}>
          <div class="call-stack-panel">
            <h3>Call Stack</h3>
            <div class="call-stack-list">
              <%= for {function, line} <- Enum.reverse(@call_stack) do %>
                <div class="stack-frame">
                  <span class="function-name"><%= function %></span>
                  <span class="line-number">line <%= line %></span>
                </div>
              <% end %>
            </div>
          </div>
        </MonoGrid.cell>
      </MonoGrid.grid>
    </div>
    """
  end

  # Helper functions
  defp sample_code() do
    [
      "def fibonacci(n) do",
      "  if n <= 1 do",
      "    n",
      "  else",
      "    fibonacci(n - 1) + fibonacci(n - 2)",
      "  end",
      "end",
      "",
      "def main() do",
      "  result = fibonacci(5)",
      "  IO.puts(\"Fibonacci(5) = \\\#{result}\")",
      "  result",
      "end",
      "",
      "main()"
    ]
  end

  defp execute_step(state) do
    current_line = state.current_line
    code = state.code
    variables = state.variables
    execution_path = state.execution_path
    call_stack = state.call_stack

    if current_line < length(code) do
      line = Enum.at(code, current_line)
      {new_variables, action, new_call_stack} = interpret_line(line, variables, call_stack)

      step = %{
        line: current_line,
        action: action,
        variable_changes: Map.merge(new_variables, variables, fn _k, v1, v2 -> if v1 != v2, do: v1 end)
      }

      %{
        current_line: current_line + 1,
        variables: new_variables,
        execution_path: [step | execution_path],
        call_stack: new_call_stack
      }
    else
      state
    end
  end

  defp interpret_line(line, variables, call_stack) do
    line = String.trim(line)

    cond do
      String.starts_with?(line, "def ") ->
        {variables, "Function definition", call_stack}

      String.starts_with?(line, "if ") ->
        {variables, "Condition check", call_stack}

      String.starts_with?(line, "else") ->
        {variables, "Else branch", call_stack}

      String.starts_with?(line, "end") ->
        {variables, "End block", call_stack}

      String.contains?(line, "=") ->
        [var_name, value] = String.split(line, "=", parts: 2)
        var_name = String.trim(var_name)
        value = String.trim(value)

        # Simple variable assignment simulation
        new_value = case value do
          "5" -> 5
          "fibonacci(5)" -> 5
          _ -> value
        end

        {Map.put(variables, var_name, new_value), "Variable assignment: #{var_name} = #{new_value}", call_stack}

      String.contains?(line, "fibonacci(") ->
        {variables, "Function call: fibonacci", [{"fibonacci", 1} | call_stack]}

      String.contains?(line, "IO.puts") ->
        {variables, "Output operation", call_stack}

      String.contains?(line, "main()") ->
        {variables, "Program entry point", [{"main", 9} | call_stack]}

      true ->
        {variables, "Statement execution", call_stack}
    end
  end
end
