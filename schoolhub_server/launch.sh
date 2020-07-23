#!/bin/bash
# Launcher script for the docker environment.

cat ${SSL_DIST_OPTFILE}
mix local.hex --force
mix do deps.get, compile
iex --name ${NAME} --cookie ${ERLANG_COOKIE} --erl "-ssl_dist_optfile ${SSL_DIST_OPTFILE}" -S mix
