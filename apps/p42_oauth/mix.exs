defmodule P42Oauth.MixProject do
  use Mix.Project

  def project do
    [
      app: :p42_oauth,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.7",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Proxy42.Plugins.OAuthIntrospection.Application, []},
      extra_applications: [:logger],
    ]
  end

  defp deps do
    [
      {:httpoison, "~> 1.0"},
      {:proxy42, in_umbrella: true},
    ]
  end
end
