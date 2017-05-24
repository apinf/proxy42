defmodule Proxy42.ControlApi.Apis do
  use Plug.Router
  use Plug.ErrorHandler
  import Plug.Conn
  require Logger
  require IEx

  alias Proxy42.Store

  plug :match
  plug Plug.Parsers, parsers: [:urlencoded, :json],
    pass:  ["application/x-www-form-urlencoded", "application/json"],
    json_decoder: Poison
  plug :validate_and_transform
  plug :dispatch

  get "/" do
    conn = fetch_query_params(conn)
    Store.get_apis(conn.query_params)
    |> send_json(conn)
  end

  post "/" do
    IEx.pry
   Store.add_api!(conn.body_params)
   |> (fn x -> send_resp(conn,
                         201,
                         "{\"id\": \"#{x}\"}") end).()
  end

  get "/:id" do
    case Store.get_api(id) do
      {:ok, api} -> api |> send_json(conn)
      {:error, :notfound} -> conn |> send_resp(404, "")
    end
  end

  patch "/:id" do
    if Store.exists?(id) do
      Store.update_api!(id, conn.body_params)
      send_resp(conn, 204, "")
    else
      send_resp(conn, 404, "")
    end
  end

  put "/:id" do
    if Store.exists?(id) do
      Store.update_api(id, conn.body_params)
      send_resp(conn, 204, "")
    else
      send_resp(conn, 404, "")
    end
  end

  delete "/:id" do
    Store.delete_api(id)
    send_resp(conn, 204, "")
  end

  def send_json(content, conn, status \\ 200) do
    json = "" #Poison.Encoder.encode(content, [])
    send_resp(conn, status, json)
  end

  match _ do
    send_resp(conn, 404, "")
  end

  # TODO: whitelist reasons to pass on, send something went wrong and 500 for others.
  # I know, sorry.
  def handle_errors(conn, %{kind: _kind, reason: reason, stack: stack}) do
    send_resp(conn, conn.status, ~s({"error": "#{inspect(reason)}"}))
  end

  def validate_api(params) do
    required_params = Store.get_all_domain_group_fields() -- ["id"]
    Enum.all?(required_params, fn x -> Map.has_key?(params, x) end)
  end

  def validate_and_transform(conn = %Plug.Conn{method: "POST"}, _opts) do
    params = conn.body_params
    with_no_pipe_stupidity =
      with false <- Map.has_key?(params, "id"),
           {:ok, hostname} <- validate_and_transform({:hostname, params["hostname"]}),
           {:ok, servers} <- validate_and_transform({:servers, params["servers"]}),
           {:ok, frontend_prefix} <- validate_and_transform({:frontend_prefix, params["frontend_prefix"]}),
           {:ok, backend_prefix} <- validate_and_transform({:backend_prefix, params["backend_prefix"]}),
           {:ok, strategy} <- validate_and_transform({:strategy, params["strategy"]}),
           {:ok, additional_headers} <- validate_and_transform({:additional_headers, params["additional_headers"]}),
           transformed_params = %{:hostname => hostname,
             :servers => servers,
             :frontend_prefix => frontend_prefix,
             :backend_prefix => backend_prefix,
             :strategy => strategy,
             :additional_headers => additional_headers
           },
        do: {:ok, transformed_params}
    IEx.pry
    case with_no_pipe_stupidity do
      true ->  # some idiot supplied an id
        send_resp(conn, 400, ~s({"error": "id should not be present"})) |> halt
      {:ok, params} -> %{conn | body_params: params}
      {:error, reason} ->
        send_resp(conn, 400, ~s({"error": "#{reason}"})) |> halt
      stuff ->
        Logger.warn "EIMPOSSIBLE: validate_and_transform: got #{inspect(stuff)}"
        send_resp(conn, 400, ~s({"error": "Malformed input"})) |> halt
    end
  end

  def validate_and_transform(conn, _opts), do: conn

  defp validate_and_transform({key, nil}) do
    {:error, "#{key} missing"}
  end
  # XXX: Terrible idea but I'm lazy for now.
  defp validate_and_transform({_, val}) do
    {:ok, val}
  end
end
