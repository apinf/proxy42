compile:
	# cowlib has warnings as errors, and uses erlang.mk
	# So it won't build with erl 19.2 and elixir 1.4.0
	# We'll override erlc_opts to workaround that
	env ERLC_OPTS=+debug mix do deps.get, compile

