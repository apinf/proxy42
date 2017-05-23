defmodule Proxy42.App.Mixfile do
  use Mix.Project

  def project do
    [
      app: :proxy42,
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

  def application do
    [
      applications: [
        :kernel,
        :stdlib,
        :vegur,
      ],
    ]
  end
end
