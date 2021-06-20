#!/bin/bash
# script to generate kubernetes configmaps from already existing files

kubectl create configmap domain-address \
	--from-env-file=../.env

kubectl create configmap postgres-schema \
	--from-file=../postgres/schema/pg.sql

kubectl create configmap haproxy-config \
	--from-file=../haproxy/haproxy_instance.cfg

kubectl create configmap mongooseim-config \
	--from-file=../mongooseim/member/mongooseim.cfg \
	--from-file=../mongooseim/member/app.config \
	--from-file=../mongooseim/member/vm.args \
	--from-file=../mongooseim/member/vm.dist.args

kubectl create configmap mongooseim-entrypoint \
	--from-file=../mongooseim/entrypoint.sh
