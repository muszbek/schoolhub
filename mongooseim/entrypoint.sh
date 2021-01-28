#!/usr/bin/env bash
# entrypoint script for mongooseim docker image
# to fill in variables in the config with environment variables of the container
# then call the original entrypoint script /start.sh

cp /config_temp/* /member

CONFIG_PATH="/member/mongooseim.cfg"

put_variable()
{
    echo "Replacing $1 with $2 ..."
    sed -i "s/$1/$2/" $CONFIG_PATH
}

put_variable %DOMAIN% $DOMAIN

put_variable %POSTGRES_HOST% $POSTGRES_HOST
put_variable %POSTGRES_DB% $POSTGRES_DB
put_variable %POSTGRES_USER% $POSTGRES_USER
put_variable %POSTGRES_PASSWORD% $POSTGRES_PASSWORD

exec /start.sh
