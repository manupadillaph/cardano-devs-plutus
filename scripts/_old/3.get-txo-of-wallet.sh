#!/bin/sh

echo "Nombre Wallet:"
read walletName

walletAddr=$(cat files/${walletName}.addr)

echo $walletAddr

result=$($CARDANO_NODE/cardano-cli query utxo\
	 --address $walletAddr --testnet-magic 1097911063)


TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+" | sed -n 4p)
TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+" | sed -n 5p)

echo $TxHash#$TxIx

echo $TxHash#$TxIx>files/${walletName}.utxo

