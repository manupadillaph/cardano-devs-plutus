#!/bin/bash

#Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
CWD=$(pwd)
cd $HASKELL

printf "%s\n" "$scriptNumero"  | cabal exec -- pab-api-server-auto --config $SCRIPTS_FILES/pab/pab-config-$scriptName.yml migrate

cd $CWD

printf "\nDatabase $SCRIPTS_FILES/pab/plutus-pab-$scriptName.db created and migrated\n"



