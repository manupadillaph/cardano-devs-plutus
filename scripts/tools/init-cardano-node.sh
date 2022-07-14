#!/bin/bash

$CARDANO_NODE/cardano-node run \
 --topology $CARDANO_NODE/configuration/testnet-topology.json \
 --database-path $CARDANO_NODE/db \
 --socket-path $CARDANO_NODE/db/node.socket \
 --port 3001 \
 --config $CARDANO_NODE/configuration/testnet-config.json
