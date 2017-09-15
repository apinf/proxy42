defmodule Proxy42.ControlApi.Apis do
  use Plug.Router
  use Plug.ErrorHandler
  import Plug.Conn
  require Logger

  alias Proxy42.Store
  alias Proxy42.DomainGroup, as: DG

  plug :match
  plug Plug.Parsers, parsers: [:urlencoded, :json],
    pass:  ["application/x-www-form-urlencoded", "application/json"],
    json_decoder: Poison
  plug :validate_and_transform
  plug :dispatch

  get "/" do
    conn = fetch_query_params(conn)
    Store.get_apis(conn.query_params)
    |> Enum.map(&DG.record_to_struct/1)
    |> Poison.encode!
    |> (&send_resp(conn, 200, &1)).()
  end

  post "/" do
   id = Store.add_api!(conn.body_params)
   send_resp(conn, 201, ~s({"id": "#{id}"}))
  end

  get "/:id" do
    case Store.get_api(id) do
      {:ok, api} ->
        api |> DG.record_to_struct |> Poison.encode! |> (&send_resp(conn, 200, &1)).()
      {:error, :notfound} ->
        conn |> send_resp(404, "")
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

  delete "/:id" do
    Store.delete_api(id)
    send_resp(conn, 204, "")
  end

  match _ do
    send_resp(conn, 404, "")
  end

  # TODO: whitelist reasons to pass on, send something went wrong and 500 for others.
  # I know, sorry.
  def handle_errors(conn, %{kind: _kind, reason: reason, stack: stack}) do
    send_resp(conn, conn.status, ~s[{"error": "#{inspect(reason)}"])
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
           {:ok, rate_limit} <- validate_and_transform({:rate_limit, params["rate_limit"]}),
           {:ok, auth_config} <- validate_and_transform({:auth_config, params["auth_config"]}),
           transformed_params = %{
             :hostname => hostname,
             :servers => servers,
             :frontend_prefix => frontend_prefix,
             :backend_prefix => backend_prefix,
             :strategy => strategy,
             :additional_headers => additional_headers,
             :rate_limit => rate_limit,
             :auth_config => auth_config
           },
        do: {:ok, transformed_params}
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
  # TODO: Validate and transform PATCH requests
  # TODO: Move validation into its own module
  def validate_and_transform(conn, _opts), do: conn

  defp validate_and_transform({:auth_config, nil}) do
    {:ok, default_auth_config()}
  end
  defp validate_and_transform({key, nil}) do
    {:error, "#{key} missing"}
  end
  defp validate_and_transform({key, p})
  when key in [:frontend_prefix, :backend_prefix] do
    if String.starts_with?(p, "/") and String.ends_with?(p, "/") do
      {:ok, p}
    else
      {:error, "#{key} must begin and end with a /"}
    end
  end

  defp validate_and_transform({:servers, servers}) do
    Enum.reduce(servers, {:ok, []},
      fn
        (server, {:ok, validated_servers}) ->
          with {protocol, host, port} <- parse_server(server),
               {:ok, _hostentry} <- :inet.gethostbyname(host),
               do: {:ok, [{protocol, host, port} | validated_servers]}
        (server, {:error, e}) when is_binary(e) -> {:error, e}
        (server, {:error, _}) -> {:error, "Invalid server: #{server}"}
      end
    )
  end

  defp validate_and_transform({:auth_config, auth_config})
  when is_map(auth_config) do
    auth_strategy = auth_config["strategy"]
    params = Map.get(auth_config, "params", [])
    allowed_strategies = registered_auth_strategies()
    if Map.has_key?(allowed_strategies, auth_strategy) do
      {:ok, {allowed_strategies[auth_strategy], params}}
    else
      {:error, "Unrecognised authentication strategy #{auth_strategy}"}
    end
  end
  defp validate_and_transform({:auth_config, auth_config})
  when is_binary(auth_config) do
    validate_and_transform({:auth_config, %{"strategy" => auth_config, "params" => []}})
  end

  defp validate_and_transform({:strategy, "random"}), do: {:ok, :random}
  defp validate_and_transform({:strategy, _}), do: {:error, "Invalid strategy"}

  defp validate_and_transform({:rate_limit, r}) do
    case :erlang.is_number(r) do
      true -> {:ok, r}
      false -> {:error, "Invalid rate limit"}
    end
  end

  # XXX: Terrible idea but I'm lazy for now.
  defp validate_and_transform({_, val}) do
    {:ok, val}
  end

  defp parse_server(server = <<"http://", _::binary>>), do: server |> String.to_charlist |> parse_server!
  defp parse_server(server = <<"https://", _::binary>>), do: server |> String.to_charlist |> parse_server!
  defp parse_server(server), do: {:error, "server #{server} has missing or invalid scheme"}
  defp parse_server!(server) when is_list(server) do
    with {:ok, {proto, _creds, host, port, _path, _qs}} <- :http_uri.parse(server),
    do: {proto, host, port}
  end

  defp registered_auth_strategies() do
    %{"auth_always" => :auth_always, "auth_key" => :auth_key}
  end

  # Hoping this record doesn't change shape.
  defp parse_hostentry(hostentry), do: elem(hostentry, 1)

  defp default_auth_config() do
    # Allow everyone
    {:auth_allow, []}
  end
end
