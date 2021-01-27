#!/bin/bash
# script to generate locally trusted ssl certs for development environment

if [ $(id --user) != 0 ]; then
    echo "you need to be root to get certificates"
    echo "use:"
    echo "     sudo ./dev_gen_ssl.sh"
    exit 1
fi

DOMAIN=schoolhub.com
if [ $# -eq 1 ]
then
    DOMAIN=$1
fi

echo "Domain name is $DOMAIN ..."

docker build -t mkcert -f Dockerfile_dev_ssl .

mkdir -p letsencrypt/live/$DOMAIN
cd ./letsencrypt/live/$DOMAIN

docker run -v $PWD:/root/.local/share/mkcert -v /usr/bin/firefox:/usr/bin/firefox -v /usr/local/share/ca-certificates:/usr/local/share/ca-certificates -v $HOME/.mozilla/firefox:/root/.mozilla/firefox --name mkcert_temp mkcert /bin/sh -c "mkcert -install && mkcert -cert-file fullchain.pem -key-file privkey.pem $DOMAIN"

cat privkey.pem fullchain.pem | tee joined_cert.pem >/dev/null
mv rootCA.pem chain.pem

chown 999:999 privkey.pem
chmod 0600 privkey.pem

docker rm mkcert_temp >/dev/null
docker rmi mkcert >/dev/null
docker rmi golang >/dev/null

echo "Docker images cleaned up"

