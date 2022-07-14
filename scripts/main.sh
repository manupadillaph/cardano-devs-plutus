#!/bin/bash



minimoADA="1800000"

opcionMenuPrincipal=""

while ! [[ $opcionMenuPrincipal = "0" ]]; do

    printf "\nOPERACIONES CON WALLET Y SCRIPT\n"

    echo "--"

    echo "1: Elegir o Crear Wallet (${walletName})"  
    echo "2: Elegir o Crear Scipt (${scriptName})"  

    echo "--"

    echo "3: Enviar ADA o Tokens a Wallet"

    echo "4: Enviar ADA o Tokens a Script"
    echo "5: Redeem ADA o Tokens de Script"

    echo "--"

    echo "6: Ver utxo de Wallet"

    echo "61: Ver utxo de todas las Addresses de Wallet"
    echo "62: Elegir otra Address de Wallet"

    echo "7: Ver utxo de Script"

    echo "--"

    echo "8: Balance de Wallet"
    echo "9: Balance de Script"

    echo "--"

    echo "10: Calcular Hash de Datum"
    echo "11: Buscar Datum de Hash"

    echo "--"

    echo "12: Mint Tokens"

    echo "--"
    
    echo "17: usar PAB de Locker Validator"

    echo "--"

    echo "16: Set minimo ADA en tx ($minimoADA)"

    echo "--"

    echo "0: Salir"

    echo "--"

    #read -n 1 -s opcionMenuPrincipal
    read  opcionMenuPrincipal

    if [[ $opcionMenuPrincipal = "1" ]]; then 
        source "$SCRIPTS/main_elegir_crear_wallet.sh"    
    fi

    if [[ $opcionMenuPrincipal = "2" ]]; then 
        source "$SCRIPTS/main_elegir_crear_script.sh"
    fi

    if [[ $opcionMenuPrincipal = "3" ]]; then 
        if [[ $walletName = ""  ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_send_to_wallet.sh"
        fi
    fi

    if [[ $opcionMenuPrincipal = "4" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
            printf "\nDebe elegir wallet y script primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_send_to_script.sh"
        fi
    fi


    if [[ $opcionMenuPrincipal = "5" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
            printf "\nDebe elegir wallet y script primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_redeem_from_script.sh"
        fi 
    fi  
    
    if [[ $opcionMenuPrincipal = "6" ]]; then 
        if [[ $walletAddr = "" ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_ver_utxo_wallet.sh"
        fi
    fi  

    if [[ $opcionMenuPrincipal = "61" ]]; then 
        if [[ $walletAddr = "" ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_ver_utxo_wallet_all.sh"
        fi

    fi  

    if [[ $opcionMenuPrincipal = "62" ]]; then 
        if [[ $walletAddr = "" ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_elegir_addr_wallet.sh"
        fi
    fi  

    if [[ $opcionMenuPrincipal = "7" ]]; then 
        if [[ $scriptName = "" ]]; then
            printf "\nDebe elegir script primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_ver_utxo_script.sh"
        fi
    fi  


    if [[ $opcionMenuPrincipal = "8" ]]; then 
        if [[ $walletName = ""  ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_ver_balance_wallet.sh"
        fi
    fi

    if [[ $opcionMenuPrincipal = "9" ]]; then 
        if [[  $scriptName = "" ]]; then
             printf "\nDebe elegir script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$SCRIPTS/main_ver_balance_script.sh"
        fi
    fi


    if [[ $opcionMenuPrincipal = "10" ]]; then 
        
        source "$SCRIPTS/main_datum_calcular_hash.sh"
        
    fi  

    if [[ $opcionMenuPrincipal = "11" ]]; then 
        
        source "$SCRIPTS/main_datum_buscar_hash.sh"

    fi

    if [[ $opcionMenuPrincipal = "12"    ]]; then 
        source "$SCRIPTS/mint/mint.sh"
        opcionMenuPrincipal=""
    fi

    if [[ $opcionMenuPrincipal = "16" ]]; then 
        printf "\nIngrese mínimo ADA por transacción:\n"
        minimoADA=
        while [[ $minimoADA = "" ]]; do
            read minimoADA
        done
    fi

    if [[ $opcionMenuPrincipal = "17" ]]; then 
        source "$SCRIPTS/pab/pab.sh"  
        opcionMenuPrincipal=""
    fi


done