defmodule Proxy42.Mixfile do
  use Mix.Project

  def project do
    [apps_path: "apps",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     erlc_options: [:debug_info, {:nowarn_deprecated_function, [{:crypto, :rand_bytes, 1}]}],
     deps: deps()]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options.
  #
  # Dependencies listed here are available only for this project
  # and cannot be accessed from applications inside the apps folder
  defp deps do
    [
      {:recon, ">= 2.3.0"},
      {:cowlib, github: "ninenines/cowlib", tag: "1.0.0", override: true},
      {:ranch, "==1.1.0", override: true},
    ]
  end
end
