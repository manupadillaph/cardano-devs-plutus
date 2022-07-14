#!/bin/bash

$CARDANO_WALLET/cardano-wallet serve \
    --testnet $CARDANO_TESNET_BYRON\
    --node-socket $CARDANO_NODE_SOCKET_PATH
