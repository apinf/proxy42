FROM bitwalker/alpine-erlang:19.2.1 as build

ENV HOME=/opt/app/ TERM=xterm

RUN apk --no-cache --update add \
      git bash make openssl grep
# openssl for getting stuff over https.
# git for git based mix deps. 
# grep for fixing erlang.mk issue #523
# make because we have a makefile.
# bash for some reason I don't remember

RUN mkdir /opt/elixir
WORKDIR /opt/elixir
RUN wget https://github.com/elixir-lang/elixir/releases/download/v1.7.4/Precompiled.zip
RUN unzip Precompiled.zip
ENV PATH=$PATH:/opt/elixir/bin

WORKDIR /opt/app

# Install Hex+Rebar
RUN mix local.hex --force && \
    mix local.rebar --force

ENV MIX_ENV=prod PROJECT=proxy42

# Make use of docker caching to avoid rebuilding deps.
# Saves time during development
COPY Makefile mix.exs mix.lock ./
COPY apps/vegur                       apps/vegur/
COPY apps/proxy42/mix.exs             apps/proxy42/mix.exs
COPY apps/proxy42_core/mix.exs        apps/proxy42_core/mix.exs
COPY apps/proxy42_control_api/mix.exs apps/proxy42_control_api/mix.exs
COPY apps/p42_admin/mix.exs           apps/p42_admin/mix.exs
COPY apps/p42_oauth/mix.exs           apps/p42_oauth/mix.exs
COPY apps/p42_log_plugin_es/mix.exs   apps/p42_log_plugin_es/mix.exs
# COPY apps/proxy42_firmware/mix.exs    apps/proxy42_firmware/mix.exs
RUN mix do deps.get, deps.compile


COPY rel rel/
COPY config config/
COPY apps apps/
RUN make

RUN mix release --env=prod --verbose --no-tar



FROM alpine:3.6
ENV INSTALLDIR=/opt/proxy42

RUN apk update && \
    apk --no-cache --update add bash openssl libgcc libstdc++ && \
    rm -rf /var/cache/apk/*

RUN mkdir -p $INSTALLDIR && chown -R nobody: $INSTALLDIR
WORKDIR $INSTALLDIR
USER nobody

ENV PORT=4000 MIX_ENV=prod REPLACE_OS_VARS=true SHELL=/bin/sh

COPY --from=build /opt/app/_build/prod/rel/proxy42 ./

EXPOSE 4000 4001 8080

ENTRYPOINT ["/opt/proxy42/bin/proxy42"]
CMD ["foreground"]
