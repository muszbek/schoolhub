#!/bin/bash
# script to generate locally trusted ssl certs for development environment

if [ $(id --user) != 0 ]; then
    echo "you need to be root to get certificates"
    echo "use:"
    echo "     sudo ./dev_gen_ssl.sh"
    exit 1
fi

mkdir -p letsencrypt/live/schoolhub.com
cd ./letsencrypt/live/schoolhub.com

docker run -v $PWD:/root/.local/share/mkcert brunopadz/mkcert-docker:latest /bin/sh -c "mkcert -install && mkcert -cert-file /root/.local/share/mkcert/fullchain.pem -key-file /root/.local/share/mkcert/privkey.pem schoolhub.com"

cat privkey.pem fullchain.pem | tee schoolhub.com.pem >/dev/null
mv rootCA.pem chain.pem

chown 999:999 privkey.pem
chmod 0600 privkey.pem
