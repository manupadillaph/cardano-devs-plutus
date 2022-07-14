#!/bin/bash

if ! [[ -f "$SCRIPTS_FILES/pab/plutus-pab-$scriptName.db" ]]
then
    printf "\nDatabase $SCRIPTS_FILES/pab/plutus-pab-$scriptName.db no existe, creandola...\n"
else
    source "$SCRIPTS/pab/pab_init_database.sh"
fi

#Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
CWD=$(pwd)
cd $HASKELL

printf "%s\n" "$scriptNumero"  | cabal exec -- pab-api-server-auto --config $SCRIPTS_FILES/pab/pab-config-$scriptName.yml webserver --passphrase $walletPassphrase

cd $CWD



