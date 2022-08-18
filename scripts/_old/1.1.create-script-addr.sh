#!/bin/sh

#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

$CARDANO_NODE/cardano-cli address build  \
	 --payment-script-file $FALCON_DEVS_HASKELL_FILES/${scriptName}.plutus --out-file $FALCON_DEVS_HASKELL_FILES/${scriptName}.addr --testnet-magic 1097911063

