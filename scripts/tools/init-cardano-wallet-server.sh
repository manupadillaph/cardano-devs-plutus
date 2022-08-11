#!/bin/bash

echo "Iniciando wallet con: "
echo "--testnet: "$CARDANO_TESNET_BYRON 
echo "--socket-path: "$CARDANO_NODE_SOCKET_PATH 
echo; read -rsn1 -p "Press any key to continue . . ."; echo

$CARDANO_WALLET/cardano-wallet serve \
    --testnet $CARDANO_TESNET_BYRON\
    --node-socket $CARDANO_NODE_SOCKET_PATH
