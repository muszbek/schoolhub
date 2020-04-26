#!/bin/bash
# Entrypoint for distributed common test helper docker container running a server node.

mix local.hex --force

erl -detached -name tester@10.3.0.9 -setcookie $ERLANG_COOKIE -pa /root/test/util

/bin/bash
