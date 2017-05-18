defmodule Proxy42.ControlApi.Router do
  use Plug.Router
  import Plug.Conn

  plug :match
  plug Plug.Parsers, parsers: [:urlencoded, :json],
    pass:  ["application/x-www-form-urlencoded", "application/json"],
    json_decoder: Poison
  plug :dispatch

  @moduledoc """
  Documentation for Proxy42ControlApi.
  """

  forward 
  get "/admin/apis" do
    json = {}
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, json)
  end

  match _ do
    send_resp(conn, 404, "Not Found")
  end
end
