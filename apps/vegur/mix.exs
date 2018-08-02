defmodule Proxy42.Vegur.Mixfile do
  use Mix.Project

  @rebar_config "rebar.config"
  @app_src "src/vegur.app.src"

  def project do
    [
      app: :vegur,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
    ]
  end

  defp deps do
    {:ok, contents} = :file.consult(@rebar_config)
    contents[:deps]
    |> Enum.map(fn({d,_,{:git, url, {:tag, tag}}}) -> {d, git: List.to_string(url) , tag: List.to_string(tag)} end)
    # midjan has no makefile, rebar.config or mix.exs. rebar can compile bare projects, let's use it
    |> update_in([:midjan, :manager], fn _ -> :rebar end)
  end


  def application do
    {:ok, contents} = :file.consult(@app_src)
    app_src = List.last(contents)
    {:application, _name, details} = app_src
    details
  end
end
