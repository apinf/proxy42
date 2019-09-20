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
      deps: deps(),
    ]
  end

  defp deps do
    [
      {:vegur, in_umbrella: true},
    ]
  end

  defp env() do
    [
      {:port, 8080},
      {:plugins, [
          :p42_auth_always,
          :p42_auth_key,
          :p42_auth_vienna,
          :p42_rl_ftbucket,
        ]},
    ]
  end

  def application do
    [
      # mod: {:proxy42_core_app, []},
      env: env(),
      applications: [
        :kernel,
        :stdlib,
        :vegur,
        :mnesia
      ],
      registered: [
        :proxy42_sup,
        :proxy42_endpoint,
      ]
    ]
  end
end
