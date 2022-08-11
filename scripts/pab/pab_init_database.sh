#!/bin/bash

echo "Creando PAB Config File desde TEMPLATE: "

echo " "
echo "--template config: "$CARDANO_PAB_SERVER_TEMPLATE_CONFIG

# cat $CARDANO_PAB_SERVER_TEMPLATE_CONFIG
# echo " "

# $CARDANO_PAB_SERVER_CONFIG=$(echo "$CARDANO_PAB_SERVER_CONFIG" | sed 's,VALIDATOR_SCRIPT_NAME,'"$scriptName"',')
# $CARDANO_PAB_DATABASE=$(echo "$CARDANO_PAB_DATABASE" | sed 's,VALIDATOR_SCRIPT_NAME,'"$scriptName"',')

CARDANO_PAB_SERVER_CONFIG="${CARDANO_PAB_SERVER_CONFIG/VALIDATOR_SCRIPT_NAME/"$scriptName"}"
CARDANO_PAB_DATABASE="${CARDANO_PAB_DATABASE/VALIDATOR_SCRIPT_NAME/"$scriptName"}"

# echo " "
# echo "CARDANO_PAB_SERVER_CONFIG $CARDANO_PAB_SERVER_CONFIG"
# echo " "
# echo "CARDANO_PAB_DATABASE $CARDANO_PAB_DATABASE"

cp $CARDANO_PAB_SERVER_TEMPLATE_CONFIG $CARDANO_PAB_SERVER_CONFIG

sed -i 's,$CARDANO_NODE_SOCKET_PATH,'"$CARDANO_NODE_SOCKET_PATH"',' $CARDANO_PAB_SERVER_CONFIG
sed -i 's,$CARDANO_PAB_DATABASE,'"$CARDANO_PAB_DATABASE"',' $CARDANO_PAB_SERVER_CONFIG 

# echo "--config: "$CARDANO_PAB_SERVER_CONFIG
# cat $CARDANO_PAB_SERVER_CONFIG

echo " "
echo "Iniciando Pab con: "
echo " "
echo "--Config: "$CARDANO_PAB_SERVER_CONFIG
echo " "
echo "--Database: "$CARDANO_PAB_DATABASE



#Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
CWD=$(pwd)
cd $HASKELL


printf "%s\n" "$scriptNumero"  | cabal exec -- pab-api-server-auto --config $CARDANO_PAB_SERVER_CONFIG  migrate

cd $CWD

printf "\nDatabase created and migrated\n"



