defmodule Proxy42.ControlApi.Plugins do
  use Plug.Router

  plug :match
  plug Plug.Parsers, parsers: [:urlencoded, :json],
    pass:  ["application/x-www-form-urlencoded", "application/json"],
    json_decoder: Poison
  plug :dispatch

  match "/:slug/*remaining_path" do
    plugin = get_plugin_for_slug(slug)
    apply(plugin, :handle_http, [remaining_path, conn])
  end

  def get_plugin_for_slug(slug) do
    # TODO: Create an table for indexing plugins and getting slugs. Move this to proxy42_plugins. handle_opts should create this reverse map and store it. This function should just access it.
    plugins = :proxy42_plugins.get_registered_plugins()
    slug_to_plugin_map = for {p, opts} <- plugins, into: %{}, do: {opts[:slug], p}
    slug_to_plugin_map[slug]
  end

end
