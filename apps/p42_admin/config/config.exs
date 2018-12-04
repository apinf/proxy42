# Since configuration is shared in umbrella projects, this file
# should only configure the :p42_admin application itself
# and only for organization purposes. All other config goes to
# the umbrella root.
use Mix.Config

# General application configuration
config :p42_admin,
  ecto_repos: [P42Admin.Repo],
  generators: [context_app: false]

# Configures the endpoint
config :p42_admin, P42Admin.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "84Ta5Hlwx7MXHfvzuxjsSZGNkVcOIi58q8hER/6T475j+2MJDcxJtRSBcjmDniYe",
  render_errors: [view: P42Admin.ErrorView, accepts: ~w(html json)],
  pubsub: [name: P42Admin.PubSub, adapter: Phoenix.PubSub.PG2]

config :phoenix,
  json_library: Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
