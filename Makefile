compile:
	mix do deps.get, compile

shell:
	iex -S mix

.PHONY: compile, shell
