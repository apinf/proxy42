defmodule Proxy42.API do
  # Find the header, extract record definition
  @api_hrl Path.expand("../../apps/proxy42_core/include/api.hrl")
  @api Record.extract(:api, from: @api_hrl)
  @api_fields @api |> Keyword.keys

  # mark this module for recompilation when hrl changes
  @external_resource @api_hrl

  # Define record in elixir-land
  require Record
  Record.defrecord :api, @api

  defstruct @api

  def get_all_api_fields() do
    @api_fields
  end

  # This creates a "record" api with all values as :_
  # which is precisely the match all pattern for mnesia
  def match_all_pattern() do
    unquote({ :{}, [], [:api |
                        for { key, _ } <- @api do
                          :_
                        end
                       ] })
  end

  @type api :: record(:api)
  @spec pattern(map()) :: api()
  def pattern(params) do
    # A pattern is a record with all fields either having constraints or the
    # atom :_. So we create a match all pattern with :_ everywhere and
    # then update it with constraints from user supplied params.
    match_all = match_all_pattern()
    update_api(match_all, params)
  end

  @doc """
  Update a given api record with values from kwlist params

  api/{1,2} is a macro created by Record.defrecord/2. It requires
  the second argument, a keyword list to update the record with, to have
  keys known at compile time.

  However, we would like to update the record at runtime from user supplied
  parameters. One way would have been to extract the params, match them against
  known good keys, and somehow construct an expanded keyword list listing all
  keys. A nicer way (IMO) is to (ab)use pattern matching to define one function
  head per valid field in the record, and a catchall to ignore others.  Macros
  enable doing just that without having to hardcode the fields.

  Long story short, the following contraption generates function heads one each
  for every valid field in the record api, updating the record
  appropriately
  """
  def update_api(api, updates) when is_map(updates) do
    update_api(api, Map.to_list(updates))
  end

  def update_api(api, []) do
    api
  end

  @api_fields
  |> Enum.each(fn key ->
    def update_api(api, [{unquote(key), val}| rest]) do
      api(api, [{unquote(key), val}])
      |> update_api(rest)
    end
  end)

  def update_api(api, [_| rest]) do
    update_api(api, rest)
  end

  @doc false
  def struct_to_record(unquote({ :%, [], [{ :__MODULE__, [], nil },
                                          { :%{}, [],
                                            for { key, _ } <- @api do
                                              { key, { key, [], nil } }
                                            end
                                          }] })) do
    unquote({ :{}, [], [:api |
                        for { key, _ } <- @api do
                          { key, [], nil }
                        end
                       ] })
  end

  def record_to_struct(unquote({ :{}, [], [:api |
                                           for { key, _ } <- @api do
                                             { key, [], nil }
                                           end
                                          ] })) do
    unquote({ :%, [], [{ :__MODULE__, [], nil },
                       { :%{}, [],
                         for { key, _ } <- @api do
                           { key, { key, [], nil } }
                         end
                       }] })
  end


end

defimpl Poison.Encoder, for: Proxy42.API do
  @api_fields Proxy42.API.get_all_api_fields
  @mod Proxy42.API

  def encode(unquote({ :%, [], [@mod,
                                { :%{}, [],
                                  for key <- @api_fields do
                                    { key, { key, [], nil } }
                                  end
                                }] }), options \\ []) do
    # Macro magic allows any key of API to be used as a variable
    # directly. Accessing it will give actual value and updating it
    # will modify json representation
    servers = for {proto, host, port} <- servers do
      "#{proto}://#{host}:#{port}"
    end

    {auth_strategy, params} = auth_config
    auth_config = %{strategy: auth_strategy, params: params}

    updated_struct_as_map = unquote({ :%{}, [],
                               for key <- @api_fields do
                                 { key, { key, [], nil } }
                               end
                             })
    Poison.encode_to_iodata!(updated_struct_as_map, options)
  end
end
