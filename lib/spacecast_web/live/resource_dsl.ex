defmodule SpacecastWeb.ResourceDSL do
  @moduledoc """
  Provides a DSL for defining resource-oriented socket assigns in LiveViews.
  """

  defmacro __using__(_opts) do
    quote do
      use SpacecastWeb, :live_view
      import SpacecastWeb.ResourceDSL

      Module.register_attribute(__MODULE__, :resource_attributes, accumulate: true)
      Module.register_attribute(__MODULE__, :resource_relationships, accumulate: true)

      @before_compile SpacecastWeb.ResourceDSL

      defoverridable do_mount: 3,
                     do_handle_params: 3,
                     do_handle_event: 3,
                     get_resource: 2,
                     update_resource: 3

      def assigns(block) do
        block
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def do_mount(params, session, socket) do
        case super(params, session, socket) do
          {:ok, socket} ->
            validate_socket(socket)

          {:error, _reason} = error ->
            error
        end
      end

      defp validate_socket(socket) do
        attributes = Module.get_attribute(__MODULE__, :resource_attributes)
        relationships = Module.get_attribute(__MODULE__, :resource_relationships)
        schema = build_schema(attributes, relationships)

        case SocketValidator.validate(socket, schema) do
          :ok ->
            {:ok, socket}

          {:error, errors} ->
            raise "Invalid socket assigns: #{inspect(errors)}"
        end
      end

      defp build_schema(attributes, relationships) do
        Enum.into(attributes, %{}, fn {name, opts} -> {name, Keyword.get(opts, :type, :any)} end)
      end
    end
  end

  defmacro attribute(name, type, options \\ []) do
    quote do
      @resource_attributes {unquote(name), [type: unquote(type)] ++ unquote(options)}
    end
  end

  defmacro relationship(name, type, resource, options \\ []) do
    quote do
      @resource_relationships {unquote(name),
                               [type: unquote(type), resource: unquote(resource)] ++
                                 unquote(options)}
    end
  end
end
