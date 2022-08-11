#!/bin/bash


if ! [[ -f "$FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.addrs"  ]]
then
    printf "\nNo existe el archivo addrs. Debe cargar una wallet en el wallet server. Necesita archivo JSON.\n"

    echo; read -rsn1 -p "Press any key to continue . . ."; echo 

else

    echo ""
    echo "Wallet Address Actual:" $walletAddr

    printf "\nDesea elegir otra (y/n)?\n"
    read -n 1 -s opcion

    if [[ $opcion = "y" ]]; then 

        result=$(cat "$FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.addrs")

        echo "$result" | nl

        walletNroAddr=
        while [[ $walletNroAddr = "" ]]; do
            printf "\nIngrese Número de address: "
            read walletNroAddr
        done
        
        echo "$result" | sed -n ${walletNroAddr}p>$FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.addr

        walletAddr=$(cat $FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.addr)

        echo ""
        echo "Wallet Address Nueva:" $walletAddr

        echo; read -rsn1 -p "Press any key to continue . . ."; echo 
    fi

fi


