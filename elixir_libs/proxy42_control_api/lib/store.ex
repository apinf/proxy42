defmodule Proxy42.Store do
  require Record
  Record.extract_all(from_lib: "proxy42/include/domain_group.hrl")
end
