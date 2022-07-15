#!/bin/sh

#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

$CARDANO_NODE/cardano-cli address build  \
	 --payment-script-file $HASKELL_FILES/${scriptName}.plutus --out-file $HASKELL_FILES/${scriptName}.addr --testnet-magic 1097911063

