#!/bin/bash
# Docker entrypoint script

# Wait until Postgres is ready
while ! pg_isready -q -h $PGHOST -p 5432 -U $PGUSER
do
    echo "$(date) - waiting for database to start..."
    sleep 2
done

mix ecto.migrate
mix run priv/repo/seeds.exs

exec mix phx.server
