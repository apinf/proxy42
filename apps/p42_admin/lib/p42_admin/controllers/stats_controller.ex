defmodule P42Admin.StatsController do
  use P42Admin, :controller

  alias HTTPoison.Response, as: Res

  def index(conn, params) do
    interval = Map.get(params, "interval", "day")
    filter = if params["api_id"] do
      %{ "term" => %{ "api_id.keyword" => params["api_id"] } }
    else %{ "match_all" => %{} }
    end
    IO.inspect filter
    ## TODO: refactor this out to config
    url = "http://localhost:9200/proxy42/_search?size=0"
    req_body = %{
      "size" => 0,
      "aggs" => %{
        "matched_calls" => %{
          "filter" => filter,
          "aggs" => %{
            "calls_over_time" => %{
              "date_histogram" => %{
                "field" => "events.init",
                "interval" => interval,
              }
            }
          }
        }
      }
    }
    headers = [{"Content-Type", "application/json"}]
    {:ok, %Res{status_code: 200, body: body} }= HTTPoison.post(url, Jason.encode!(req_body), headers)
    calls_over_time = body
                      |> Jason.decode!
                      |> get_in(["aggregations", "matched_calls", "calls_over_time", "buckets"])
                      |> Enum.map(fn (b) -> {b["key"], b["doc_count"]} end)
                      |> Enum.into(%{})
    response = if params["debug"] == "true" do
      body |> Jason.decode! |> Map.put("req", req_body)
    else
      %{ calls_over_time: calls_over_time }
    end
    conn |> json(response)
  end
end
