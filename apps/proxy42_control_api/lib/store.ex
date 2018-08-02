defmodule Proxy42.Store do
  require Proxy42.API
  alias Proxy42.API, as: API

  def get_apis(params) do
    API.pattern(params)
    |> :mnesia.dirty_match_object
    |> IO.inspect
  end

  def get_api(id) do
    API.pattern(id: id)
    |> :mnesia.dirty_match_object
    |> case do
      [] -> {:error, :notfound}
      [h | _] -> {:ok, h}
    end
  end

  def add_api!(params) do
    {:ok, res} = add_api(params)
    res
  end

  def add_api(params) do
    id = :uuid.uuid_to_string(:uuid.get_v4(), :binary_standard)
    new_params = Map.put(params, :id, id)
    transaction(fn ->
      if is_api_unique_enough(params) do
        API.api() # creates empty record
        |> API.update_api(new_params)
        |> :mnesia.write
      else
        # TODO: Find a better message, include conflicting api?
        :mnesia.abort("conflicts with existing apis")
      end
    end)
    |> case do
      {:ok, :ok} -> {:ok, id}
      {:error, reason} -> {:error, reason}
    end
  end

  def update_api!(id, params) do
    {:ok, res} = update_api(id, params)
    res
  end

  def update_api(id, params) do
    transaction(fn ->
      case :mnesia.read(:api, id, :write) do
        [] -> :mnesia.abort("No api with id #{id}")
        [api] ->
          API.update_api(api, params)
          |> :mnesia.write
        _ -> :mnesia.abort("EIMPOSSIBLE: Too many apis with id #{id}")
      end
    end)
  end

  def delete_api(id) do
    transaction(fn ->
      :mnesia.delete(:api, id, :write)
    end)
  end

  # TODO: Cleanup exists/1
  def exists?(id) do
    case get_api(id) do
      {:ok, _} -> true
      _ -> false
    end
  end

  def exists?(:developer, developer) do
    # FIXME: Don't write patterns directly
    case :mnesia.dirty_match_object({:developer, developer, :_}) do
      [_] -> true
      [] -> false
    end
  end

  def exists?(:api, api) do
    case :mnesia.dirty_match_object(API.pattern(%{:id => api})) do
      [_] -> true
      [] -> false
    end
  end

  # TODO: Rethink this
  def get_developers(_params) do
    :mnesia.dirty_match_object({:developer, :_, :_})
    |> Enum.map(fn({:developer, id, email}) ->
      %{"id": id, "email": email} end)
  end

  def add_developer!(params) do
    id = :uuid.uuid_to_string(:uuid.get_v4(), :binary_standard)
    {:atomic, :ok} = :mnesia.transaction( fn ->
      {:developer, id, params["email"]}
      |> :mnesia.write
    end)
    id
  end

  def add_authorization(params) do
    developer = params["developer_id"]
    api = params["api_id"]
    case :mnesia.transaction(fn ->
        :mnesia.write({:authorization, developer, api})
        end
        ) do
      {:atomic, _} -> :ok
      {:aborted, reason} -> {:error, reason}
    end
  end

  @doc false
  def is_api_unique_enough(params) do
    fp = params[:frontend_prefix]
    :mnesia.dirty_match_object(API.pattern(%{frontend_prefix: fp}))
    |> case do
         [] -> true
         _ -> false
       end
  end

  defp transaction(t) do
    case :mnesia.transaction(t) do
      {:atomic, res} -> {:ok, res}
      {:aborted, reason} -> {:error, reason}
    end
  end

end
