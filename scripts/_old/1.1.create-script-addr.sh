#!/bin/sh

#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

$CARDANO_NODE/cardano-cli address build  \
	 --payment-script-file files/${scriptName}.plutus --out-file files/${scriptName}.addr --testnet-magic 1097911063

