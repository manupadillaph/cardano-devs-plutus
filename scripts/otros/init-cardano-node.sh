$CARDANO_NODE/cardano-node run \
 --topology $CARDANO_NODE/configuration/testnet-topology.json \
 --database-path $CARDANO_NODE/db \
 --socket-path $CARDANO_NODE/db/node.socket \
 --host-addr 127.0.0.1 \
 --port 3001 \
 --config $CARDANO_NODE/configuration/testnet-config.json
