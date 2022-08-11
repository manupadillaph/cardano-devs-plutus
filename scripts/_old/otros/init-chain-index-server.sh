#en [NIX-SHELL], para abrir ejecutar antes: bash $NIX_SHELL 

cd $PLUTUS_APPS/plutus-pab/test-node/
cabal exec -- plutus-chain-index --config testnet/chain-index-config.json start-index
