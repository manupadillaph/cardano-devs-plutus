#!/bin/bash

#en [NIX-SHELL], para abrir ejecutar antes: bash $NIX_SHELL 

echo "Creando Config File desde TEMPLATE: "
echo " "
echo "--template config: "$CARDANO_CHAIN_INDEX_TEMPLATE_CONFIG
# cat $CARDANO_CHAIN_INDEX_TEMPLATE_CONFIG
# echo ""

cp $CARDANO_CHAIN_INDEX_TEMPLATE_CONFIG $CARDANO_CHAIN_INDEX_CONFIG

sed -i 's,$CARDANO_NODE_SOCKET_PATH,'"$CARDANO_NODE_SOCKET_PATH"',' $CARDANO_CHAIN_INDEX_CONFIG
sed -i 's,$CARDANO_CHAIN_INDEX_DB,'"$CARDANO_CHAIN_INDEX_DB"',' $CARDANO_CHAIN_INDEX_CONFIG 

# cecho "--config: "$CARDANO_CHAIN_INDEX_CONFIG
# cat $CARDANO_CHAIN_INDEX_CONFIG
echo " "
echo "Iniciando chain-index con: "
echo " "
echo "--config: "$CARDANO_CHAIN_INDEX_CONFIG
echo " "
echo; read -rsn1 -p "Press any key to continue . . ."; echo

cd $PLUTUS_APPS
#cabal exec -- plutus-chain-index --config $CARDANO_CHAIN_INDEX_CONFIG start-index --verbose

cabal exec -- plutus-chain-index --config $CARDANO_CHAIN_INDEX_CONFIG start-index 