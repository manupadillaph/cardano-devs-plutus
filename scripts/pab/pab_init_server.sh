#!/bin/bash


CARDANO_PAB_SERVER_CONFIG="${CARDANO_PAB_SERVER_CONFIG/VALIDATOR_SCRIPT_NAME/"$scriptName"}"
CARDANO_PAB_DATABASE="${CARDANO_PAB_DATABASE/VALIDATOR_SCRIPT_NAME/"$scriptName"}"


if ! [[ -f "$CARDANO_PAB_DATABASE" && -f "$CARDANO_PAB_SERVER_CONFIG" ]]
then
    printf "\nDatabase $CARDANO_PAB_DATABASE o Config $CARDANO_PAB_SERVER_CONFIG no existen, creandolos...\n"
    source "$FALCON_DEVS_SCRIPTS/pab/pab_init_database.sh"
 
fi

printf "\nIniciando Pab con:\n"

WALLET_ID=$(cat $FALCON_DEVS_HASKELL_FILES/wallets/$walletName.id)
echo "WALLET ID:" $WALLET_ID
echo "Passphrase:" $walletPassphrase
echo " "
echo "--Config: "$CARDANO_PAB_SERVER_CONFIG
echo " "
echo "--Database: "$CARDANO_PAB_DATABASE

echo " "

#Para poder ejecutar el cabal exec necesito estar en la carpeta $FALCON_DEVS_HASKELL donde hice el cabal build
CWD=$(pwd)
cd $FALCON_DEVS_HASKELL

printf "%s\n" "$scriptNumero"  | cabal exec -- pab-api-server-auto --config $CARDANO_PAB_SERVER_CONFIG webserver --passphrase $walletPassphrase

cd $CWD



