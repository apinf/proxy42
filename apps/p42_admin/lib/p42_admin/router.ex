defmodule P42Admin.Router do
  use P42Admin, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", P42Admin do
    pipe_through :browser

    get "/", PageController, :index

  end

  scope "/api" do
    pipe_through :api

    get "/stats", P42Admin.StatsController, :index

    forward "/", Proxy42.ControlApi.Router
  end

end
