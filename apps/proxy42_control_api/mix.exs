defmodule Proxy42.ControlApi.Mixfile do
  use Mix.Project

  def project do
    [app: :proxy42_control_api,
     version: "0.1.0",
     elixir: "~> 1.2",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [
      extra_applications: [:logger],
      mod: {Proxy42.ControlApi.Application, []}
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [
      {:cowboy, "~>1.0.0"},
      {:plug, "~> 1.0"},
      {:poison, "~>3.0"},
      {:uuid, github: "okeuday/uuid", tag: "v1.5.1.1"},
      {:proxy42_core, in_umbrella: true}
    ]
  end
end
