defmodule Proxy42.ControlApi.Authorizations do
  use Plug.Router
  use Plug.ErrorHandler
  import Plug.Conn
  require Logger

  alias Proxy42.Store

  plug :match
  plug Plug.Parsers, parsers: [:urlencoded, :json],
    pass:  ["application/x-www-form-urlencoded", "application/json"],
    json_decoder: Poison
  plug :validate_and_transform
  plug :dispatch

  post "/" do
    case Store.add_authorization(conn.body_params) do
      :ok ->
        "Authorization created"
        |> Poison.encode!
        |> (&send_resp(conn, 201, &1)).()
      {:error, _reason} -> send_resp(conn, 500, "")
    end
  end

  defp validate_and_transform(conn = %Plug.Conn{method: "POST"}, _opts) do
    params = conn.body_params
    cond do
      Map.has_key?(params, "developer_id") == false -> send_error(conn, "developer_id must be provided")
      Map.has_key?(params, "api_id") == false -> send_error(conn, "api_id must be provided")
      Store.exists?(:developer, params["developer_id"]) == false -> send_error(conn, "Developer doesn't exist.")
      Store.exists?(:api, params["api_id"]) == false -> send_error(conn, "API doesn't exist")
      true -> conn
    end
  end

  defp send_error(conn, reason) do
    send_resp(conn, 400, ~s({"error": "#{reason}"}))
    |> halt()
  end
end
