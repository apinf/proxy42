use Mix.Config

config :p42_log_plugin_es,
  es_url: System.get_env("P42_ES_URL")
