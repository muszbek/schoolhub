#!/bin/bash
# script to generate kubernetes secrets from already existing files

source ../.env

kubectl create secret generic tls-selfsigned-secret \
	--from-file=../letsencrypt/selfsigned/$DOMAIN/certs/chain.pem \
	--from-file=../letsencrypt/selfsigned/$DOMAIN/certs/cert.pem \
	--from-file=../letsencrypt/selfsigned/$DOMAIN/certs/fullchain.pem \
	--from-file=../letsencrypt/selfsigned/$DOMAIN/certs/privkey.pem \
	--from-file=../letsencrypt/selfsigned/$DOMAIN/certs/joined_cert.pem

kubectl create secret generic tls-letsencrypt-secret \
	--from-file=../letsencrypt/live/$DOMAIN/certs/joined_cert.pem

kubectl create secret generic postgres-creds \
	--from-env-file=../postgres/postgres.env

kubectl create secret generic postgres-router-creds \
	--from-env-file=../postgres/postgres_router.env
