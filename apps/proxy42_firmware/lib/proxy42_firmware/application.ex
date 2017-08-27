defmodule Proxy42.Firmware.Application do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec

    # Define workers and child supervisors to be supervised
    children = [
      worker(Proxy42.Firmware.Networking, [], restart: :transient),
      # worker(Proxy42.Firmware.Bootstrap, []),
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Proxy42.Firmware.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
