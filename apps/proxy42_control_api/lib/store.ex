defmodule Proxy42.Store do
  import Proxy42.DomainGroup

  def get_apis(params) do
    pattern(params)
    |> :mnesia.dirty_match_object
    |> IO.inspect
  end

  def get_api(id) do
    pattern(id: id)
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
    # TODO Get config from user.
    default_auth_config = {:authorization_needed, :header, <<"authorization">>, :strip}
    new_params = Map.put(params, :id, id)
    |> Map.put(:auth_config, default_auth_config)
    transaction(fn ->
      domain_group() # creates empty record
      |> update_domain_group(new_params)
      |> :mnesia.write
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
      case :mnesia.read(:domain_group, id, :write) do
        [] -> :mnesia.abort("No api with id #{id}")
        [dg] ->
          update_domain_group(dg, params)
          |> :mnesia.write
        _ -> :mnesia.abort("EIMPOSSIBLE: Too many apis with id #{id}")
      end
    end)
  end

  def delete_api(id) do
    transaction(fn ->
      :mnesia.delete(:domain_group, id, :write)
    end)
  end

  def exists?(id) do
    case get_api(id) do
      {:ok, _} -> true
      _ -> false
    end
  end

  # TODO: Rethink this
  def get_developers(_params) do
    :mnesia.dirty_match_object({:developer, :_,:_,:_})
    |> Enum.map(fn(x) -> %{"id": :erlang.element(2, x)} end)
  end

  # TODO: Rethink format of id and key. Both are uuid v4 for now.
  def add_developer!(_params) do
    id = :uuid.uuid_to_string(:uuid.get_v4(), :binary_standard)
    key = :uuid.uuid_to_string(:uuid.get_v4(), :binary_standard)
    {:atomic, :ok} = :mnesia.transaction( fn ->
      {:developer, id, key, {:nopassword}}
      |> :mnesia.write
    end)
    {id, key}
  end

  defp transaction(t) do
    case :mnesia.transaction(t) do
      {:atomic, res} -> {:ok, res}
      {:aborted, reason} -> {:error, reason}
    end
  end

end
