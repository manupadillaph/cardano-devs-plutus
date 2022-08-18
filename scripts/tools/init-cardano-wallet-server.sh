#!/bin/bash

echo "Iniciando wallet con en: $CARDANO_NODE_NETWORK"

if [[ $CARDANO_NODE_NETWORK = "TESTNET" ]]; then

    echo "--port: 8090" 
    echo "--testnet: "$CARDANO_BYRON 
    echo "--node-socket $CARDANO_NODE_SOCKET_PATH" 
    echo "--database $CARDANO_WALLET/database-testnet"
    echo "--token-metadata-server https://metadata.cardano-testnet.iohkdev.io/"

    echo; read -rsn1 -p "Press any key to continue . . ."; echo

    $CARDANO_WALLET/cardano-wallet serve \
        --port 8090 \
        --testnet $CARDANO_BYRON\
        --node-socket $CARDANO_NODE_SOCKET_PATH
        --database $CARDANO_WALLET/database-testnet \
        --token-metadata-server https://metadata.cardano-testnet.iohkdev.io/
else

    echo "--port: 8090" 
    echo "--mainnet"
    echo "--node-socket $CARDANO_NODE_SOCKET_PATH"
    #echo "--light"
	#echo "--blockfrost-token-file $FALCON_DEVS_HASKELL_FILES_CONFIG/blockfrost/blockfrost-mainnet.key"
    echo "--database $CARDANO_WALLET/database-mainnet"
    echo "--token-metadata-server https://metadata.cardano-testnet.iohkdev.io/"
    echo; read -rsn1 -p "Press any key to continue . . ."; echo

    $CARDANO_WALLET/cardano-wallet serve \
        --port 8090 \
        --mainnet \
        --node-socket $CARDANO_NODE_SOCKET_PATH  \
        --database $CARDANO_WALLET/database-mainnet \
        --token-metadata-server https://metadata.cardano-testnet.iohkdev.io/

fi