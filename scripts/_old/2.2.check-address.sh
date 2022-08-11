#!/bin/sh

echo "Nombre Wallet:"
read walletName

walletAddr=$(cat $FALCON_DEVS_HASKELL_FILES/${walletName}.addr)

echo $walletAddr

$CARDANO_NODE/cardano-cli query utxo\
	 --address $walletAddr --testnet-magic 1097911063 


