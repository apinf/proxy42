defmodule P42Admin.PageController do
  use P42Admin, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
