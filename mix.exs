defmodule Proxy42.Umbrella.Mixfile do
  use Mix.Project

  def project do
    [apps: [:proxy42 | plugin_apps()],
     apps_path: "apps",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  defp deps do
    [
      {:recon, ">= 2.3.0"},
      {:distillery, "~> 2.0"},
    ]
  end

  defp plugin_apps() do
    [:p42_log_plugin_es, :p42_oauth, :p42_admin]
  end
end
