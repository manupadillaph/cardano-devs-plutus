#!/bin/bash

#en [NIX-SHELL], para abrir ejecutar antes: bash $NIX_SHELL 

echo "Iniciando chain-index con: "
echo "--config: "$CARDANO_CHAIN_INDEX_CONFIG
echo; read -rsn1 -p "Press any key to continue . . ."; echo

cd $PLUTUS_APPS
cabal exec -- plutus-chain-index --config $CARDANO_CHAIN_INDEX_CONFIG start-index 
# --verbose