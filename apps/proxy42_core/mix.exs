defmodule Proxy42.Core.Mixfile do
  use Mix.Project

  def project do
    [
      app: :proxy42_core,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      language: :erlang,
      deps: deps(),
    ]
  end

  defp deps do
    [
      {:vegur, github: "heroku/vegur"},
    ]
  end

  defp env() do
    [
      {:port, 8080},
    ]
  end

  def application do
    [
      mod: {:proxy42_core_app, []},
      env: env(),
      applications: [
        :kernel,
        :stdlib,
        :vegur,
      ],
      registered: [
        :proxy42_sup,
        :proxy42_endpoint,
      ]
    ]
  end
end
