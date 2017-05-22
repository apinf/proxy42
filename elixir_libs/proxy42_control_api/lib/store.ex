defmodule Proxy42.Store do
  @domain_group_hrl Path.expand("../../apps/proxy42/include/domain_group.hrl")
  @domain_group Record.extract(:domain_group, from: @domain_group_hrl)
  require Record
  Record.defrecord :domain_group, @domain_group

  def pattern(params) do
    # A pattern is a record with all fields either having constraints or the
    # atom :_. So we create a match all pattern with :_ everywhere and
    # then update it with constraints from user supplied params.
    match_all = :proxy42_storage_app.match_all_pattern()
    update_domain_group(match_all, params)
  end

  @doc """
  Update a given domain_group record with values from kwlist params

  domain_group/{1,2} is a macro created by Record.defrecord/2. It requires
  the second argument, a keyword list to update the record with, to have
  keys known at compile time.

  However, we would like to update the record at runtime from user supplied
  parameters. One way would have been to extract the params, match them against
  known good keys, and somehow construct an expanded keyword list listing all
  keys. A nicer way (IMO) is to (ab)use pattern matching to define one function
  head per valid field in the record, and a catchall to ignore others.  Macros
  enable doing just that without having to hardcode the fields.

  Long story short, the following contraption generates function heads one each
  for every valid field in the record domain_group, updating the record
  appropriately
  """
  def update_domain_group(dg, updates) when is_map(updates) do
    update_domain_group(dg, Map.to_list(updates))
  end

  def update_domain_group(dg, []) do
    dg
  end

  @domain_group
  |> Keyword.keys
  |> Enum.each fn key ->
    def update_domain_group(dg, [{unquote(key), val}| rest]) do
      domain_group(dg, [{unquote(key), val}])
      |> update_domain_group(rest)
    end
  end

  def update_domain_group(dg, [_| rest]) do
    update_domain_group(dg, rest)
  end

  def get_apis(params) do
    pattern(params)
    |> :mnesia.dirty_match_object
  end

  def get_api(id) do
    pattern(id: id)
    |> :mnesia.dirty_match_object
    |> case do
      [] -> {:error, :notfound}
      [h | _] -> {:ok, h}
    end
  end

  def add_api(params) do
    id = :uuid.to_string(:uuid.uuid4())
    new_params = Map.put(params, :id, id)
    :mnesia.transaction(fn ->
      domain_group() # creates empty record
      |> update_domain_group(new_params)
      |> :mnesia.write
    end)
  end

  def put_api(id, params)
  def patch_api(id, params)
  def delete_api(id)
end
