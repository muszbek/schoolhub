#!/bin/bash
# Launcher script for the docker environment.

mix local.hex --force
mix do deps.get, compile
iex --name ${NAME} --cookie ${ERLANG_COOKIE} \
    --erl "-proto_dist inet_tls -ssl_dist_optfile ${SSL_DIST_OPTFILE}" -S mix
