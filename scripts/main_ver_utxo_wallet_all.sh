#!/bin/bash

if ! [[ -f "$SCRIPTS_FILES/wallets/${walletName}.addrs"  ]]
then
    printf "\nNo existe el archivo addrs\n"

    echo; read -rsn1 -p "Press any key to continue . . ."; echo 

else
    while read address
    do
        printf "\nUtxo At Wallet address $address:\n"

        result=$($CARDANO_NODE/cardano-cli query utxo\
        --address $address --testnet-magic $TESTNET_MAGIC)

        echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl

    done < "$SCRIPTS_FILES/wallets/${walletName}.addrs"

    echo; read -rsn1 -p "Press any key to continue . . ."; echo 

fi


   echo "Wallet Address:" $walletAddr
            
            result=$($CARDANO_NODE/cardano-cli query utxo\
                --address $walletAddr --testnet-magic $TESTNET_MAGIC)

            printf "\nUtxo at Wallet:\n"

            echo "$result" 

            echo; read -rsn1 -p "Press any key to continue . . ."; echo 

         