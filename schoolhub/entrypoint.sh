#!/bin/bash
# Docker entrypoint script

# Setting generated secrets in env variables
export SECRET_KEY_BASE=$(mix phx.gen.secret)
export SESSION_SIGNING_SALT=$(tr -dc A-Za-z0-9 </dev/urandom | head -c 8)
export SESSION_CRYPT_SALT=$(tr -dc A-Za-z0-9 </dev/urandom | head -c 8)

exec "$@"
