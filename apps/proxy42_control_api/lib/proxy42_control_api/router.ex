defmodule Proxy42.ControlApi.Router do
  use Plug.Router
  import Plug.Conn

  plug Plug.Logger, log: :debug
  plug :match
  plug :dispatch

  @moduledoc """
  Documentation for Proxy42ControlApi.
  """

  forward "/apis", to: Proxy42.ControlApi.Apis
  forward "/developers", to: Proxy42.ControlApi.Developers
  forward "/authorizations", to: Proxy42.ControlApi.Authorizations
  forward "/plugins", to: Proxy42.ControlApi.Plugins

  match _ do
    send_resp(conn, 404, "Not Found")
  end
end
