#!/bin/bash

#Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
CWD=$(pwd)
cd $HASKELL

printf "%s\n" "$scriptNumero"  | cabal exec -- pab-api-server-auto --config $HASKELL_FILES/config/pab/pab-config-$scriptName.yml migrate

cd $CWD

printf "\nDatabase $HASKELL_FILES/pab/plutus-pab-$scriptName.db created and migrated\n"



