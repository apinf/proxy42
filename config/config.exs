use Mix.Config

# Common default config

# Can be overridden by app specific config
import_config "../apps/*/config/config.exs"

# which may be overridden per environment
import_config "#{Mix.env}.exs"

# Absolute overrides. Avoid unless testing
config :logger, :console,
  level: :debug,
  format: "$date $time [$level] $metadata$message\n"
#       metadata: [:user_id]

