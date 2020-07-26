#!/bin/bash
# Utility script to simplify starting the integrated test environment
# (which is a subset of the complete environment).

read -sp "Please enter sudo password:" SUDO_PW

cd ../schoolhub_server
echo $SUDO_PW | sudo -S chown -R $USER:$USER ./_build
mix compile

cd ../schoolhub_client
echo $SUDO_PW | sudo -S chown -R $USER:$USER ./_build
mix compile

cd ..
docker-compose up postgres mongooseim-1 haproxy dist_tester_server dist_tester_client
