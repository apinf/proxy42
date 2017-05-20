defmodule Proxy42.ControlApi.Apis do
  use Plug.Router
  import Plug.Conn
  require Logger

  alias Proxy42.Store

  plug :match
  plug :dispatch

  get "/" do
    conn = fetch_query_params(conn)
    Store.get_apis(conn.query_params)
    |> send_json(conn)
  end

  post "/" do
    Store.add_api(conn.assigns[:body_params])
    |> send_json(conn)
  end

  get "/:id" do
    Logger.warn("Works")
    IO.inspect conn
    case Store.get_api(id) do
      {:ok, api} -> api |> send_json(conn)
      {:error, :notfound} -> conn |> send_resp(404, "")
    end
  end

  patch "/:id" do
    Store.patch_api(id, conn.assigns[:body_params])
    |> send_json(conn)
  end

  put "/:id" do
    Store.add_api(id, conn.assigns[:body_params])
    |> send_json(conn)
  end

  delete "/:id" do
    Store.delete_api(id)
    |> send_json(conn)
  end

  def send_json(content, conn, status \\ 200) do
    IO.inspect conn
    json = "" #Poison.Encoder.encode(content, [])
    send_resp(conn, status, json)
  end

  match _ do
    send_resp(conn, 404, "Not Found")
  end
end
