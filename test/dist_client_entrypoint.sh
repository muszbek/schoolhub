#!/bin/bash
# Entrypoint for distributed common test helper docker container running client nodes.

for ((i=1;i<=$NODES_AMOUNT;i++)); do
    erl -detached -name client$i@10.3.0.10 -setcookie $ERLANG_COOKIE -pa /root/test/util
done

/bin/bash
