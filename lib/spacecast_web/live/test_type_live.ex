defmodule SpacecastWeb.TestTypeLive do
  use SpacecastWeb.BaseLive

  def do_mount(_params, session, socket) do
    assign(socket,
       page_title: "Test",
       theme_class: "system-theme",
       string_value: session["string_value"] || "default",
       integer_value: session["integer_value"] || 42,
       theme: session["theme"] || "dark",
       tags: session["tags"] || [],
       id_or_name: session["id_or_name"] || "default",
       user: session["user"] || %{}
     )
  end

  def render(assigns) do
    ~H"""
    <div>
      <p>String value: {@string_value}</p>
      <p>Integer value: {@integer_value}</p>
      <p>Theme: {@theme}</p>
      <p>Tags: {@tags}</p>
      <p>ID or Name: {@id_or_name}</p>
      <p>User: {@user["name"]}</p>
    </div>
    """
  end
end
