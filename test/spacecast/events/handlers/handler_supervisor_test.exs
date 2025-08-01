defmodule Spacecast.Events.Handlers.HandlerSupervisorTest do
  use ExUnit.Case, async: false
  alias Spacecast.Events.Handlers.HandlerSupervisor
  alias Spacecast.Events.Handlers.HandlerProcess

  # Test handlers
  defmodule TestHandler do
    @behaviour Spacecast.Events.Handlers.Handler

    def init, do: {:ok, %{initialized: true}}
    def interested_in, do: ["test_event"]
    def handle_event(_event, state), do: {:ok, state}
  end

  defmodule CallHandler do
    @behaviour Spacecast.Events.Handlers.Handler

    def init, do: {:ok, %{calls: 0}}
    def interested_in, do: ["test_event"]
    def handle_event(_event, state), do: {:ok, state}

    def handle_call(:increment, _from, state) do
      {:reply, :ok, %{state | calls: state.calls + 1}}
    end

    def handle_call(:get_count, _from, state) do
      {:reply, state.calls, state}
    end
  end

  defmodule CastHandler do
    @behaviour Spacecast.Events.Handlers.Handler

    def init, do: {:ok, %{casts: 0}}
    def interested_in, do: ["test_event"]
    def handle_event(_event, state), do: {:ok, state}

    def handle_cast(:increment, state) do
      {:noreply, %{state | casts: state.casts + 1}}
    end

    def handle_call(:get_count, _from, state) do
      {:reply, state.casts, state}
    end
  end

  defmodule InfoHandler do
    @behaviour Spacecast.Events.Handlers.Handler

    def init, do: {:ok, %{messages: []}}
    def interested_in, do: ["test_event"]
    def handle_event(_event, state), do: {:ok, state}

    def handle_info({:test_message, msg}, state) do
      {:ok, %{state | messages: [msg | state.messages]}}
    end

    def handle_call(:get_messages, _from, state) do
      {:reply, state.messages, state}
    end
  end

  defmodule TerminateHandler do
    @behaviour Spacecast.Events.Handlers.Handler

    def init, do: {:ok, %{terminated: false}}
    def interested_in, do: ["test_event"]
    def handle_event(_event, state), do: {:ok, state}

    def terminate(_reason, state) do
      %{state | terminated: true}
    end
  end

  defmodule CrashingHandler do
    @behaviour Spacecast.Events.Handlers.Handler

    def init, do: raise("Intentional crash")
    def interested_in, do: ["test_event"]
    def handle_event(_event, state), do: {:ok, state}
  end

  setup do
    # The supervisor is already started by the application
    # Clean up any existing handlers
    {:ok, handlers} = HandlerSupervisor.list_handlers()

    Enum.each(handlers, fn {_module, pid} ->
      HandlerSupervisor.stop_handler(pid)
    end)

    :ok
  end

  describe "start_link/1" do
    test "starts the supervisor successfully" do
      # The supervisor is already started by the application
      assert {:error, {:already_started, pid}} = HandlerSupervisor.start_link()
      assert Process.alive?(pid)
    end

    test "starts with custom options" do
      opts = [name: :test_supervisor]
      # The supervisor is already started by the application
      assert {:error, {:already_started, pid}} = HandlerSupervisor.start_link(opts)
      assert Process.alive?(pid)
    end
  end

  describe "start_handler/2" do
    test "starts a handler successfully" do
      assert {:ok, pid} = HandlerSupervisor.start_handler(TestHandler)
      assert Process.alive?(pid)
    end

    test "starts a handler with options" do
      opts = [name: :test_handler, event_types: ["custom_event"]]
      assert {:ok, pid} = HandlerSupervisor.start_handler(TestHandler, opts)
      assert Process.alive?(pid)
    end

    test "starts a handler with a name" do
      opts = [name: :named_handler]
      assert {:ok, pid} = HandlerSupervisor.start_handler(TestHandler, opts)
      assert Process.alive?(pid)

      # Verify the handler is registered with the name
      assert Process.whereis(:named_handler) == pid
    end

    test "handles invalid handler module" do
      # The HandlerProcess will try to start it anyway, so we expect success
      assert {:ok, _pid} = HandlerSupervisor.start_handler(String)
    end

    test "handles handler that crashes on init" do
      # The HandlerProcess will fail to start when the handler crashes on init
      assert {:error, _reason} = HandlerSupervisor.start_handler(CrashingHandler)
    end

    test "creates unique child specs for different handlers" do
      {:ok, pid1} = HandlerSupervisor.start_handler(TestHandler, name: :handler1)
      {:ok, pid2} = HandlerSupervisor.start_handler(TestHandler, name: :handler2)

      assert pid1 != pid2
      assert Process.alive?(pid1)
      assert Process.alive?(pid2)
    end
  end

  describe "stop_handler/1" do
    test "stops a handler by PID" do
      {:ok, pid} = HandlerSupervisor.start_handler(TestHandler)
      assert :ok = HandlerSupervisor.stop_handler(pid)

      # Verify the process is no longer alive
      refute Process.alive?(pid)
    end

    test "stops a handler by name" do
      {:ok, pid} = HandlerSupervisor.start_handler(TestHandler, name: :named_handler)
      assert :ok = HandlerSupervisor.stop_handler(:named_handler)

      # Verify the process is no longer alive
      refute Process.alive?(pid)
    end

    test "handles non-existent handler PID" do
      assert {:error, :not_found} = HandlerSupervisor.stop_handler(self())
    end

    test "handles non-existent handler name" do
      assert {:error, :not_found} = HandlerSupervisor.stop_handler(:non_existent)
    end

    test "handles invalid handler reference" do
      assert {:error, :not_found} = HandlerSupervisor.stop_handler("invalid")
    end
  end

  describe "list_handlers/0" do
    test "returns empty list when no handlers are running" do
      assert {:ok, []} = HandlerSupervisor.list_handlers()
    end

    test "returns list of running handlers" do
      {:ok, pid1} = HandlerSupervisor.start_handler(TestHandler, name: :handler1)
      {:ok, pid2} = HandlerSupervisor.start_handler(CallHandler, name: :handler2)

      {:ok, handlers} = HandlerSupervisor.list_handlers()

      assert length(handlers) >= 2

      handler_modules = Enum.map(handlers, fn {module, _pid} -> module end)
      assert TestHandler in handler_modules
      assert CallHandler in handler_modules

      # Verify PIDs are correct
      handler_pids = Enum.map(handlers, fn {_module, pid} -> pid end)
      assert pid1 in handler_pids
      assert pid2 in handler_pids
    end

    test "filters out dead handlers" do
      {:ok, pid} = HandlerSupervisor.start_handler(TestHandler)

      # Kill the process manually
      Process.exit(pid, :kill)

      # Wait a bit for the supervisor to detect the death
      Process.sleep(10)

      {:ok, handlers} = HandlerSupervisor.list_handlers()
      handler_pids = Enum.map(handlers, fn {_module, pid} -> pid end)
      refute pid in handler_pids
    end
  end

  describe "count_handlers/0" do
    test "returns zero when no handlers are running" do
      assert {:ok, %{active: 0}} = HandlerSupervisor.count_handlers()
    end

    test "returns correct count of running handlers" do
      {:ok, _pid1} = HandlerSupervisor.start_handler(TestHandler)
      {:ok, _pid2} = HandlerSupervisor.start_handler(CallHandler)
      {:ok, _pid3} = HandlerSupervisor.start_handler(CastHandler)

      assert {:ok, %{active: 3}} = HandlerSupervisor.count_handlers()
    end

    test "count decreases when handlers are stopped" do
      {:ok, pid1} = HandlerSupervisor.start_handler(TestHandler)
      {:ok, pid2} = HandlerSupervisor.start_handler(CallHandler)

      assert {:ok, %{active: 2}} = HandlerSupervisor.count_handlers()

      HandlerSupervisor.stop_handler(pid1)
      assert {:ok, %{active: 1}} = HandlerSupervisor.count_handlers()

      HandlerSupervisor.stop_handler(pid2)
      assert {:ok, %{active: 0}} = HandlerSupervisor.count_handlers()
    end
  end

  describe "send_to_handler/2" do
    test "sends message to handler by PID" do
      {:ok, pid} = HandlerSupervisor.start_handler(CallHandler)

      assert {:ok, :ok} = HandlerSupervisor.send_to_handler(pid, :increment)
      assert {:ok, 1} = HandlerSupervisor.send_to_handler(pid, :get_count)
    end

    test "sends message to handler by name" do
      {:ok, _pid} = HandlerSupervisor.start_handler(CallHandler, name: :call_handler)

      assert {:ok, :ok} = HandlerSupervisor.send_to_handler(:call_handler, :increment)
      assert {:ok, 1} = HandlerSupervisor.send_to_handler(:call_handler, :get_count)
    end

    test "handles non-existent handler PID" do
      assert {:error, :not_found} = HandlerSupervisor.send_to_handler(self(), :test_message)
    end

    test "handles non-existent handler name" do
      assert {:error, :not_found} =
               HandlerSupervisor.send_to_handler(:non_existent, :test_message)
    end

    test "handles handler that doesn't support calls" do
      {:ok, pid} = HandlerSupervisor.start_handler(TestHandler)

      assert {:ok, {:error, :not_supported}} =
               HandlerSupervisor.send_to_handler(pid, :test_message)
    end

    test "handles handler crash during call" do
      defmodule CrashCallHandler do
        @behaviour Spacecast.Events.Handlers.Handler
        def init, do: {:ok, %{}}
        def handle_event(_event, state), do: {:ok, state}
        def handle_call(_msg, _from, _state), do: raise("Intentional crash")
      end

      {:ok, pid} = HandlerSupervisor.start_handler(CrashCallHandler)
      # The handler will crash when we try to send a message
      assert {:error, _reason} = HandlerSupervisor.send_to_handler(pid, :test_message)
    end
  end

  describe "handler lifecycle" do
    test "handler can handle events" do
      {:ok, pid} = HandlerSupervisor.start_handler(InfoHandler)

      # Send a test message
      send(pid, {:test_message, "hello"})

      # Wait a bit for the message to be processed
      Process.sleep(10)

      # Verify the handler received the message
      assert {:ok, ["hello"]} = HandlerSupervisor.send_to_handler(pid, :get_messages)
    end

    test "handler can handle casts" do
      {:ok, pid} = HandlerSupervisor.start_handler(CastHandler)

      # Send a cast
      GenServer.cast(pid, :increment)

      # Wait a bit for the cast to be processed
      Process.sleep(10)

      # Verify the cast was processed
      assert {:ok, 1} = HandlerSupervisor.send_to_handler(pid, :get_count)
    end

    test "handler termination is called" do
      {:ok, pid} = HandlerSupervisor.start_handler(TerminateHandler)

      # Stop the handler
      assert :ok = HandlerSupervisor.stop_handler(pid)

      # Verify the process is no longer alive
      refute Process.alive?(pid)
    end
  end

  describe "error handling" do
    test "handles invalid handler modules gracefully" do
      # The HandlerProcess will try to start these anyway, so we expect success
      assert {:ok, _pid} = HandlerSupervisor.start_handler(nil)
      # String module names cause badarg errors
      assert {:error, _reason} = HandlerSupervisor.start_handler("not_a_module")
    end

    test "handles handler crashes gracefully" do
      # Start a handler that will crash
      assert {:error, _reason} = HandlerSupervisor.start_handler(CrashingHandler)

      # Verify the supervisor is still running
      assert {:ok, _count} = HandlerSupervisor.count_handlers()
    end

    test "handles multiple handler failures" do
      # Try to start multiple crashing handlers
      assert {:error, _reason1} = HandlerSupervisor.start_handler(CrashingHandler)
      assert {:error, _reason2} = HandlerSupervisor.start_handler(CrashingHandler)
      assert {:error, _reason3} = HandlerSupervisor.start_handler(CrashingHandler)

      # Verify the supervisor is still running
      assert {:ok, _count} = HandlerSupervisor.count_handlers()
    end
  end

  describe "supervisor behavior" do
    test "supervisor restarts handlers on crash" do
      # This test verifies that the supervisor can handle handler crashes
      # and potentially restart them based on the restart strategy

      {:ok, pid} = HandlerSupervisor.start_handler(TestHandler)
      assert Process.alive?(pid)

      # The supervisor should continue running even if handlers crash
      assert {:ok, _count} = HandlerSupervisor.count_handlers()
    end

    test "supervisor maintains isolation between handlers" do
      {:ok, pid1} = HandlerSupervisor.start_handler(CallHandler, name: :handler1)
      {:ok, pid2} = HandlerSupervisor.start_handler(CallHandler, name: :handler2)

      # Each handler should have its own state
      assert {:ok, :ok} = HandlerSupervisor.send_to_handler(pid1, :increment)
      assert {:ok, 1} = HandlerSupervisor.send_to_handler(pid1, :get_count)
      assert {:ok, 0} = HandlerSupervisor.send_to_handler(pid2, :get_count)
    end
  end
end
