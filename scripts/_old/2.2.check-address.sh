#!/bin/sh

echo "Nombre Wallet:"
read walletName

walletAddr=$(cat files/${walletName}.addr)

echo $walletAddr

$CARDANO_NODE/cardano-cli query utxo\
	 --address $walletAddr --testnet-magic 1097911063 


