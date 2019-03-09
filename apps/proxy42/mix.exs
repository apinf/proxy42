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
     test_paths: test_paths(),
     deps: deps()]
  end

  def application do
    [
      mod: {Proxy42.Application, []},
    ]
  end

  defp deps do
    overrides() ++ proxy42_components()
  end

  defp overrides do
    [
      {:cowlib, "~>1.0.0", override: true},
      {:ranch, "~>1.0", override: true},
      # {:cowlib,
      #  github: "ninenines/cowlib", tag: "1.0.0", override: true,
      #  compile: ~s(ERLC_OPTS="+'{nowarn_deprecated_function, [{crypto, rand_bytes, 1}]}'" make all)
      # },
      # {:ranch, "==1.1.0", override: true},
      # {:ranch, github: "ninenines/ranch", tag: "1.4.0", override: true},
      # erequest_id wants uuid from hex, vegur and controlapi take from git.
      {:uuid, "~>1.7.3", hex: "uuid_erl", override: true},
    ]
  end

  @component_apps [ :proxy42_core, :proxy42_control_api ]
  defp proxy42_components do
    @component_apps
    |> Enum.map(&app_to_dep/1)
  end

  defp test_paths do
    @component_apps
    |> Enum.map(&("../#{&1}/test"))
  end

  # Component otp applications are listed here to generate compilation
  # dependencies.
  defp app_to_dep(app) do
    {app, in_umbrella: true, runtime: true}
  end
end
