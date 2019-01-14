compile:
	mix do deps.get, compile

shell:
	iex -S mix phx.server

.PHONY: compile, shell
