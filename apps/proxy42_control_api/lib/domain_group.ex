defmodule Proxy42.DomainGroup do
  # Find the header, extract record definition
  @domain_group_hrl Path.expand("../../apps/proxy42_core/include/domain_group.hrl")
  @domain_group Record.extract(:domain_group, from: @domain_group_hrl)
  @domain_group_fields @domain_group |> Keyword.keys

  # mark this module for recompilation when hrl changes
  @external_resource @domain_group_hrl

  # Define record in elixir-land
  require Record
  Record.defrecord :domain_group, @domain_group

  defstruct @domain_group

  def get_all_domain_group_fields() do
    @domain_group_fields
  end

  # This creates a "record" domain_group with all values as :_
  # which is precisely the match all pattern for mnesia
  def match_all_pattern() do
    unquote({ :{}, [], [:domain_group |
                        for { key, _ } <- @domain_group do
                          :_
                        end
                       ] })
  end

  @type domain_group :: record(:domain_group)
  @spec pattern(map()) :: domain_group()
  def pattern(params) do
    # A pattern is a record with all fields either having constraints or the
    # atom :_. So we create a match all pattern with :_ everywhere and
    # then update it with constraints from user supplied params.
    match_all = match_all_pattern()
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

  @domain_group_fields
  |> Enum.each(fn key ->
    def update_domain_group(dg, [{unquote(key), val}| rest]) do
      domain_group(dg, [{unquote(key), val}])
      |> update_domain_group(rest)
    end
  end)

  def update_domain_group(dg, [_| rest]) do
    update_domain_group(dg, rest)
  end

  @doc false
  def struct_to_record(unquote({ :%, [], [{ :__MODULE__, [], nil },
                                          { :%{}, [],
                                            for { key, _ } <- @domain_group do
                                              { key, { key, [], nil } }
                                            end
                                          }] })) do
    unquote({ :{}, [], [:domain_group |
                        for { key, _ } <- @domain_group do
                          { key, [], nil }
                        end
                       ] })
  end

  def record_to_struct(unquote({ :{}, [], [:domain_group |
                                           for { key, _ } <- @domain_group do
                                             { key, [], nil }
                                           end
                                          ] })) do
    unquote({ :%, [], [{ :__MODULE__, [], nil },
                       { :%{}, [],
                         for { key, _ } <- @domain_group do
                           { key, { key, [], nil } }
                         end
                       }] })
  end


end

defimpl Poison.Encoder, for: Proxy42.DomainGroup do
  @domain_group_fields Proxy42.DomainGroup.get_all_domain_group_fields
  @mod Proxy42.DomainGroup

  def encode(unquote({ :%, [], [@mod,
                                { :%{}, [],
                                  for key <- @domain_group_fields do
                                    { key, { key, [], nil } }
                                  end
                                }] }), options \\ []) do
    # Macro magic allows any key of DG to be used as a variable
    # directly. Accessing it will give actual value and updating it
    # will modify json representation
    servers = for {proto, host, port} <- servers do
      "#{proto}://#{host}:#{port}"
    end

    {auth_strategy, params} = auth_config
    auth_config = %{strategy: auth_strategy, params: params}

    updated_struct_as_map = unquote({ :%{}, [],
                               for key <- @domain_group_fields do
                                 { key, { key, [], nil } }
                               end
                             })
    Poison.encode_to_iodata!(updated_struct_as_map, options)
  end
end
