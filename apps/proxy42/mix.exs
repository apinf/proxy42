defmodule Proxy42.Mixfile do
  use Mix.Project

  def project do
    [app: :proxy42,
     version: "0.1.0",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    []
  end

  defp deps do
    overrides() ++ proxy42_apps()
  end

  defp overrides do
    [
      {:cowlib,
       github: "ninenines/cowlib", tag: "1.0.0", override: true,
       compile: ~s(ERLC_OPTS="+'{nowarn_deprecated_function, [{crypto, rand_bytes, 1}]}'" make all)
      },
      {:ranch, "==1.1.0", override: true},
    ]
  end

  defp proxy42_apps do
    [
      {:proxy42_core, in_umbrella: true},
      {:proxy42_control_api, in_umbrella: true},
    ]
  end
end
