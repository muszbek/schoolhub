#!/bin/bash
# script to fetch valid TLS certificates via ACME / letsencrypt / certbot

if [ $(id --user) != 0 ]; then
    echo "you need to be root to get certificates"
    echo "use:"
    echo "     sudo ./dev_gen_ssl.sh"
    exit 1
fi

# load DOMAIN from file:
source ./.env
if [ $# -eq 1 ]
then
    DOMAIN=$1
fi

echo "Domain name is $DOMAIN ..."

ROOT_DIR=$PWD

function get_selfsigned {
    echo "Creating self-signed certificates..."

    mkdir -p letsencrypt/selfsigned/$DOMAIN/certs
    cd ./letsencrypt/selfsigned/$DOMAIN/certs

    docker run --rm -v $PWD:/root/.local/share/mkcert --name mkcert_temp brunopadz/mkcert-docker /bin/sh -c "mkcert -install && mkcert -cert-file /root/.local/share/mkcert/cert.pem -key-file /root/.local/share/mkcert/privkey.pem $DOMAIN \"*.schoolhub.default.svc.cluster.local\""

    cat privkey.pem cert.pem | tee joined_cert.pem >/dev/null
    mv rootCA.pem chain.pem
    cp cert.pem fullchain.pem

    chown 999:999 privkey.pem
    chmod 0600 privkey.pem

    cd $ROOT_DIR
}


if [ ! -d "./letsencrypt/selfsigned/$DOMAIN" ]; then
    get_selfsigned
fi

apt-get update
apt-get install -y certbot

certbot certonly --standalone -d $DOMAIN --cert-name $DOMAIN --non-interactive --agree-tos -m muszbektamas@gmail.com --config-dir ./letsencrypt

cd ./letsencrypt/live/$DOMAIN
mkdir -p certs

# replacing cert symlinks in target folder with original certs
for link in ./*; do
    if [[ -L "$link" ]]; then
       orig="$(readlink -f "$link")"
       cp "$orig" "./certs/$link"
    fi
done

cd ./certs
cat privkey.pem fullchain.pem | tee joined_cert.pem >/dev/null

chown 999:999 privkey.pem
chmod 0600 privkey.pem

cd $ROOT_DIR

echo "Updating domain name in kustomization.yaml..."

CONFIG_PATH="./kustomization.yaml"
put_variable()
{
    sed -i "s/$1/$2/" $CONFIG_PATH
}

put_variable %DOMAIN% $DOMAIN
