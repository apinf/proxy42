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
    [
      included_applications: proxy42_components(),
      mod: {Proxy42.Application, []},
    ]
  end

  defp deps do
    overrides() ++ proxy42_component_deps()
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

  defp proxy42_components do
    [ :proxy42_core, :proxy42_control_api ]
  end

  # Component otp applications are listed here to generate compilation
  # dependencies However, they will be managed entirely by proxy42 under its own
  # supervision tree, and hence shouldd not be started by the erlang vm
  # automatically. runtime: false ensures that they are not added to
  # :applications key in application resource file (.app). They will be listed
  # under `included_applications` instead.

  defp proxy42_component_deps do
    proxy42_components()
    |> Enum.map(fn x -> {x, in_umbrella: true, runtime: false} end)
  end
end
