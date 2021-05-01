#!/bin/bash
# script to generate kubernetes configmaps from already existing files

kubectl create configmap postgres-schema \
	--from-file=../postgres/schema/pg.sql

kubectl create configmap haproxy-config \
	--from-file=../haproxy/haproxy.cfg
