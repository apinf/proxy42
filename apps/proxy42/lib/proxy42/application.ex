defmodule Proxy42.Application do
  use Application

  def start(type, args) do
    :proxy42
    |> get_included_applications()
    |> Enum.each(&start_dependent_applications/1)

    import Supervisor.Spec
    children = [
      supervisor(Proxy42.ControlApi.Application, [type,args],
        function: :start,
        id: :proxy42_control_api, restart: :permanent),
      supervisor(:proxy42_core_app, [type,args], function: :start,
        id: :proxy42_core, restart: :permanent)
    ]
    opts = [strategy: :one_for_one, name: __MODULE__]
    Supervisor.start_link(children, opts)
  end

  defp get_included_applications(app) do
    Application.spec(app)[:included_applications]
  end

  defp start_dependent_applications(app) do
    Application.spec(app)[:applications]
    |> Enum.map(fn x -> Application.ensure_all_started(x) end)
  end
end
