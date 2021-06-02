#!/bin/bash
# script to generate kubernetes secrets from already existing files

kubectl create secret generic tls-selfsigned-secret \
	--from-file=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/chain.pem \
	--from-file=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/cert.pem \
	--from-file=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/fullchain.pem \
	--from-file=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/privkey.pem \
	--from-file=../letsencrypt/selfsigned/schoolhub.ddns.net/certs/joined_cert.pem

kubectl create secret generic tls-letsencrypt-secret \
	--from-file=../letsencrypt/live/schoolhub.ddns.net/certs/joined_cert.pem

kubectl create secret generic postgres-creds \
	--from-env-file=../postgres/postgres.env
