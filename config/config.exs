# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
use Mix.Config

# By default, the umbrella project as well as each child
# application will require this configuration file, ensuring
# they all use the same configuration. While one could
# configure all applications here, we prefer to delegate
# back to each application for organization purposes.
import_config "../apps/*/config/config.exs"
import_config "#{Mix.env}.exs"

# Sample configuration (overrides the imported configuration above):
#
config :logger, :console,
  level: :debug,
  format: "$date $time [$level] $metadata$message\n"
#       metadata: [:user_id]

config :mix_docker, image: "apinf/proxy42"

config :p42_log_plugin_es,
  # es_url: "http://hap.cinfra.fi:9200/proxy42/logs/_bulk"
  es_url: "http://localhost:9200/proxy42/logs/_bulk"
