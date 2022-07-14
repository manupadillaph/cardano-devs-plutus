#!/bin/bash

echo "Script Address:" $scriptAddr
            
result=$($CARDANO_NODE/cardano-cli query utxo\
    --address $scriptAddr --testnet-magic $TESTNET_MAGIC)

printf "\nUtxo at Scritp:\n"

echo "$result" 

echo; read -rsn1 -p "Press any key to continue . . ."; echo 

   

   