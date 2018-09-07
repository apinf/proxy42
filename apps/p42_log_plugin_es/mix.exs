defmodule Proxy42.Plugins.Logging.Elasticsearch.Mixfile do
  use Mix.Project

  def project do
    [
      app: :p42_log_plugin_es,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
    ]
  end

  defp deps do
    [ {:poolboy, "~> 1.5"},
      {:hackney, "~> 1.13"},
      {:jsx, "~> 2.9"}
    ]
  end

  defp env() do
    [ 
      es_url: "http://localhost:9200/proxy42/logs/_bulk",
    ]
  end

  def application do
    [
      mod: {:p42_log_plugin_es, []},
      env: env(),
      applications: [
        :kernel,
        :stdlib,
        :poolboy,
        :hackney,
        :jsx,
      ],
      registered: [ ]
    ]
  end
end
