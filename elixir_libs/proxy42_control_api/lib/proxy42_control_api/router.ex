defmodule Proxy42.ControlApi.Router do
  use Plug.Router
  import Plug.Conn

  plug Plug.Logger, log: :debug
  plug :match
  plug Plug.Parsers, parsers: [:urlencoded, :json],
    pass:  ["application/x-www-form-urlencoded", "application/json"],
    json_decoder: Poison
  plug :dispatch

  @moduledoc """
  Documentation for Proxy42ControlApi.
  """

  forward "/apis", to: Proxy42.ControlApi.Apis

  match _ do
    IO.inspect conn
    send_resp(conn, 404, "Not Found")
  end
end
