defmodule Proxy42.Umbrella.Mixfile do
  use Mix.Project

  def project do
    [apps_path: "apps",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  defp deps do
    [
      {:recon, ">= 2.3.0"},
      {:cowlib,
        github: "ninenines/cowlib", tag: "1.0.0", override: true,
        compile: ~s(ERLC_OPTS="+'{nowarn_deprecated_function, [{crypto, rand_bytes, 1}]}'" make all)
      },
      {:ranch, "==1.1.0", override: true},
    ]
  end
end
