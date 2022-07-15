#!/bin/bash

echo "Iniciando nodo con: "
echo "--topology: "$CARDANO_TESNET_TOPOLOGY 
echo "--database-path: "$CARDANO_NODE_DB_PATH 
echo "--socket-path: "$CARDANO_NODE_SOCKET_PATH 
echo "--port: "$CARDANO_NODE_PORT 
echo "--config: "$CARDANO_TESNET_CONFIG 
echo; read -rsn1 -p "Press any key to continue . . ."; echo

$CARDANO_NODE/cardano-node run \
 --topology $CARDANO_TESNET_TOPOLOGY \
 --database-path $CARDANO_NODE_DB_PATH \
 --socket-path $CARDANO_NODE_SOCKET_PATH \
 --port $CARDANO_NODE_PORT \
 --config $CARDANO_TESNET_CONFIG  


