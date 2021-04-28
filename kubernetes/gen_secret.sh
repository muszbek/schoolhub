#!/bin/bash
# script to generate kubernetes secrets from already existing files

kubectl create secret generic tls-selfsigned-secret \
	--from-file=CA=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/chain.pem \
	--from-file=cert=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/cert.pem \
	--from-file=fullcert=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/fullchain.pem \
	--from-file=key=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/privkey.pem \
	--from-file=joined=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/joined_cert.pem

kubectl create secret generic postgres-creds \
	--from-env-file=../postgres/postgres.env
