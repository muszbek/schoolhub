#!/bin/bash
# Utility script to simplify starting the integrated test environment
# (which is a subset of the complete environment).

cd ..
docker-compose -f docker-compose-legacy.yml up postgres mongooseim-1 haproxy dist_tester_server dist_tester_client