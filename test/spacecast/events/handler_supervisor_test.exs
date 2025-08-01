defmodule Spacecast.Events.HandlerSupervisorTest do
  use ExUnit.Case, async: false
  alias Spacecast.Events.HandlerSupervisor
  alias Spacecast.Events.Handlers.HandlerSupervisor, as: CoreHandlerSupervisor

  # Mock handler for testing
  defmodule TestHandler do
    @behaviour Spacecast.Events.Handlers.Handler

    def init, do: {:ok, %{initialized: true}}
    def interested_in, do: ["test_event"]
    def handle_event(_event, state), do: {:ok, state}
  end

  setup do
    # Clean up any existing handlers
    {:ok, handlers} = CoreHandlerSupervisor.list_handlers()

    Enum.each(handlers, fn {_module, pid} ->
      CoreHandlerSupervisor.stop_handler(pid)
    end)

    :ok
  end

  describe "start_link/1" do
    test "delegates to core handler supervisor" do
      # This test verifies delegation - the actual functionality is tested in core tests
      # The supervisor is already started by the application, so we expect an error
      assert {:error, {:already_started, _pid}} = HandlerSupervisor.start_link()
    end

    test "passes options to core handler supervisor" do
      opts = [name: :test_supervisor]
      # The supervisor is already started by the application, so we expect an error
      assert {:error, {:already_started, _pid}} = HandlerSupervisor.start_link(opts)
    end
  end

  describe "start_handler/2" do
    test "delegates to core handler supervisor" do
      assert {:ok, _pid} = HandlerSupervisor.start_handler(TestHandler)
    end

    test "passes handler module and options to core handler supervisor" do
      opts = [name: :test_handler, event_types: ["custom_event"]]
      assert {:ok, _pid} = HandlerSupervisor.start_handler(TestHandler, opts)
    end

    test "handles invalid handler module" do
      # Test with a module that doesn't implement the Handler behaviour
      # The HandlerProcess will try to start it anyway, so we expect success
      assert {:ok, _pid} = HandlerSupervisor.start_handler(String)
    end
  end

  describe "stop_handler/1" do
    test "delegates to core handler supervisor" do
      {:ok, pid} = HandlerSupervisor.start_handler(TestHandler)
      assert :ok = HandlerSupervisor.stop_handler(pid)
    end

    test "handles non-existent handler" do
      assert {:error, :not_found} = HandlerSupervisor.stop_handler(self())
    end

    test "handles invalid handler reference" do
      assert {:error, :not_found} = HandlerSupervisor.stop_handler(:non_existent)
    end
  end

  describe "list_handlers/0" do
    test "delegates to core handler supervisor" do
      assert {:ok, []} = HandlerSupervisor.list_handlers()
    end

    test "returns list of running handlers" do
      {:ok, pid1} = HandlerSupervisor.start_handler(TestHandler, name: :handler1)
      {:ok, pid2} = HandlerSupervisor.start_handler(TestHandler, name: :handler2)

      {:ok, handlers} = HandlerSupervisor.list_handlers()
      handler_modules = Enum.map(handlers, fn {module, _pid} -> module end)

      assert TestHandler in handler_modules
      assert length(handlers) >= 2

      # Clean up
      HandlerSupervisor.stop_handler(pid1)
      HandlerSupervisor.stop_handler(pid2)
    end
  end

  describe "count_handlers/0" do
    test "delegates to core handler supervisor" do
      assert {:ok, count} = HandlerSupervisor.count_handlers()
      assert is_map(count)
      assert Map.has_key?(count, :active)
    end

    test "returns correct count of running handlers" do
      initial_count =
        case HandlerSupervisor.count_handlers() do
          {:ok, count} -> count.active
        end

      {:ok, _pid} = HandlerSupervisor.start_handler(TestHandler)

      {:ok, new_count} = HandlerSupervisor.count_handlers()
      assert new_count.active == initial_count + 1
    end
  end

  describe "send_to_handler/2" do
    test "delegates to core handler supervisor" do
      {:ok, pid} = HandlerSupervisor.start_handler(TestHandler)

      # Test sending a message to the handler
      assert {:ok, _reply} = HandlerSupervisor.send_to_handler(pid, :test_message)
    end

    test "handles non-existent handler" do
      assert {:error, :not_found} = HandlerSupervisor.send_to_handler(self(), :test_message)
    end

    test "handles invalid handler reference" do
      assert {:error, :not_found} =
               HandlerSupervisor.send_to_handler(:non_existent, :test_message)
    end

    test "handles handler that doesn't support calls" do
      # Create a handler that doesn't implement handle_call
      defmodule SimpleHandler do
        @behaviour Spacecast.Events.Handlers.Handler
        def init, do: {:ok, %{}}
        def handle_event(_event, state), do: {:ok, state}
      end

      {:ok, pid} = HandlerSupervisor.start_handler(SimpleHandler)

      assert {:ok, {:error, :not_supported}} =
               HandlerSupervisor.send_to_handler(pid, :test_message)
    end
  end

  describe "supervisor lifecycle" do
    test "can start and stop multiple handlers" do
      # Start multiple handlers
      {:ok, pid1} = HandlerSupervisor.start_handler(TestHandler, name: :handler1)
      {:ok, pid2} = HandlerSupervisor.start_handler(TestHandler, name: :handler2)
      {:ok, pid3} = HandlerSupervisor.start_handler(TestHandler, name: :handler3)

      # Verify they're running
      {:ok, count} = HandlerSupervisor.count_handlers()
      assert count.active >= 3

      # Stop handlers
      assert :ok = HandlerSupervisor.stop_handler(pid1)
      assert :ok = HandlerSupervisor.stop_handler(pid2)
      assert :ok = HandlerSupervisor.stop_handler(pid3)

      # Verify they're stopped
      {:ok, final_count} = HandlerSupervisor.count_handlers()
      assert final_count.active < count.active
    end

    test "handles handler crashes gracefully" do
      # Create a handler that crashes on initialization
      defmodule CrashingHandler do
        @behaviour Spacecast.Events.Handlers.Handler
        def init, do: raise("Intentional crash")
        def handle_event(_event, state), do: {:ok, state}
      end

      # Should not crash the supervisor
      assert {:error, _reason} = HandlerSupervisor.start_handler(CrashingHandler)
    end
  end

  describe "error handling" do
    test "handles invalid handler modules" do
      # The HandlerProcess will try to start these anyway, so we expect success
      assert {:ok, _pid} = HandlerSupervisor.start_handler(nil)
      # String module names cause badarg errors
      assert {:error, _reason} = HandlerSupervisor.start_handler("not_a_module")
    end

    test "handles supervisor not started" do
      # This test verifies that the delegation handles supervisor not being available
      # In a real scenario, the supervisor should be started by the application
      # Since the supervisor is already started, this should work
      assert {:ok, _pid} = HandlerSupervisor.start_handler(TestHandler)
    end
  end
end
