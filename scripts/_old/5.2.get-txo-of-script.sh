#!/bin/sh

#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

scriptAddr=$(cat files/${scriptName}.addr)

echo $scriptAddr

result=$($CARDANO_NODE/cardano-cli query utxo\
	 --address $scriptAddr --testnet-magic 1097911063)
echo $result



TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+" | sed -n 4p)
TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+" | sed -n 5p)

echo $TxHash#$TxIx

echo $TxHash#$TxIx>files/${scriptName}.utxo

