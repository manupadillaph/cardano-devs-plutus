#!/bin/bash

#en [NIX-SHELL], para abrir ejecutar antes: bash $NIX_SHELL 

cd $PLUTUS_APPS
cabal exec -- plutus-chain-index --config $CARDANO_CHAIN_INDEX_FILES/chain-index-config.json start-index 
# --verbose