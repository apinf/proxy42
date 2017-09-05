defmodule Proxy42.Umbrella.Mixfile do
  use Mix.Project

  def project do
    [apps: [:proxy42],
     apps_path: "apps",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  defp deps do
    [
      {:recon, ">= 2.3.0"},
    ]
  end
end
