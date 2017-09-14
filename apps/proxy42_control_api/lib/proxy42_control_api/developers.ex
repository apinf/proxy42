defmodule Proxy42.ControlApi.Developers do
  use Plug.Router
  import Plug.Conn
  require Logger
  require IEx

  alias Proxy42.Store

  plug :match
  plug Plug.Parsers, parsers: [:urlencoded, :json],
    pass:  ["application/x-www-form-urlencoded", "application/json"],
    json_decoder: Poison
  plug :dispatch

  get "/" do
    conn = fetch_query_params(conn)
    Store.get_developers(conn.query_params)
    |> Poison.encode!
    |> (&send_resp(conn, 200, &1)).()
  end

  post "/" do
    id = Store.add_developer!(conn.body_params)
    send_resp(conn, 201, ~s({"id": "#{id}"))
  end
end
