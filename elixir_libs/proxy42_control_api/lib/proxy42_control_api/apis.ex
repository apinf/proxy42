defmodule Proxy42.ControlApi.Apis do
  use Plug.Router

  alias Proxy42.Store

  get "/" do
    params = conn |> fetch_query_params
    Store.get_apis(params)
    |> send_json
  end

  post "/" do
    params = conn |> 
    Store.add_api(params)
  end

  get "/:id" do
  end

  patch "/:id" do
  end

  put "/:id" do
  end

  delete "/:id" do
  end

  def send_json(content, conn, status \\ 200) do
    json = Poison.Encoder.encode(content)
    send_resp(conn, status, json)
  end
end
