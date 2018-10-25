defmodule Proxy42.Plugins.OAuthIntrospection do
  @moduledoc """
  OAuth plugin for Proxy42
  """
  # use Proxy42.Plugin, :authenticate

  def init() do
    opts = [strategies: [{:auth, "oauth", __MODULE__}], slug: "oauth"]
    {:ok, opts}
  end

  defp get_config(id) do
    # TODO: build internal config table and lookup
    %{authentication_info: [], #[header: {"Authorization", "Bearer abcdefg"}],
      token_introspection_url: "http://84.20.150.158:4445/oauth2/introspect",
    }
  end

  def auth(config_id, req, ctx) do
    config = get_config(config_id)
    %{authentication_info: authentication_info,
      token_introspection_url: url,
    } = config
    token = get_bearer_token(req)
    headers = [{"Accept", "application/json"},
               {"Content-Type", "application/x-www-form-urlencoded"}
               | get_headers(authentication_info)]
    body = "token=#{token}"
    # Crashes cause deny anyway
    HTTPoison.post!(url, body, headers)
    |> process_introspection_response()
  end

  def get_bearer_token(req) do
    hdrval = :p42_req.get_header("authorization", req)
    <<"Bearer ", token::binary>> = hdrval
    token
  end

  defp get_headers(authentication_info) do
    authentication_info
    |> Enum.filter(fn({t,_}) -> t == :header end)
    |> Enum.map(fn({:header, {h, v}}) -> {h, v} end)
  end

  defp process_introspection_response(%HTTPoison.Response{status_code: 200, body: body}) do
    resp = Poison.decode!(body)
    if resp["active"] == "true" || resp["active"] == true do
      issuer = resp["iss"]
      client_id = resp["client_id"]
      developer_id = get_developer_id(issuer, client_id)
      app_user = resp["username"] || resp["sub"]
      {developer_id, app_user}
      # Either can be nil, but whatever.
      # We assume:
      #  the user who got the token is the ultimate application user
      #  id of client that requested the token is the devloper identifier (???) - should we have a clientid to dev id mapping?
    else
      :deny
    end
  end
  defp process_introspection_response(_) do
    :deny
  end

  defp get_developer_id(iss, cid) do
    # TODO: fill this up in a proper way.
    Proxy42.Store.get_developers(%{})
    |> hd()
    |> Map.get(:id)
  end
end

defmodule Proxy42.Plugins.OAuthIntrospection.Application do
  use Application

  @plugin Proxy42.Plugins.OAuthIntrospection
  def start(_mode, _opts) do
    :ok = :proxy42_plugins.register_plugin(@plugin)
    Supervisor.start_link([], strategy: :one_for_one)
  end

  def prep_stop(state) do
    :proxy42_plugins.unregister_plugin(@plugin)
    state
  end

end
