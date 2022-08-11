#!/bin/bash



minimoADA="1800000"

opcionMenuPrincipal=""

while ! [[ $opcionMenuPrincipal = "0" ]]; do

    printf "\nOPERACIONES CON WALLET Y SCRIPT:\n"

    echo "--"

    echo "1: Elegir o Crear Wallet (${walletName})"  
    echo "2: Elegir o Crear Scipt (${scriptName})"  

    echo "--"

    echo "31: Enviar ADA o Tokens a Wallet"

    echo "32: Enviar ADA o Tokens a Script"
    echo "33: Redeem ADA o Tokens de Script"

    echo "--"

    echo "41: Ver utxo de Wallet"

    echo "42: Ver utxo de todas las Addresses de Wallet"
    echo "43: Elegir otra Address de Wallet"

    echo "44: Ver utxo de Script"

    echo "--"

    echo "51: Balance de Wallet"
    echo "52: Balance de Script"

    echo "--"

    echo "61: Calcular Hash de Datum"
    echo "62: Buscar Datum de Hash"

    echo "--"

    echo "7: Mint Tokens"

    echo "--"
    
    echo "8: PAB"

    echo "--"

    echo "9: TOOLS"

    echo "--"

    echo "99: Set minimo ADA en tx ($minimoADA)"

    echo "--"

    echo "0: Salir"

    echo "--"

    echo "Opcion: "
    read opcionMenuPrincipal

    if [[ $opcionMenuPrincipal = "1" ]]; then 
        source "$FALCON_DEVS_SCRIPTS/main/main_elegir_crear_wallet.sh"    
    fi

    if [[ $opcionMenuPrincipal = "2" ]]; then 
        source "$FALCON_DEVS_SCRIPTS/main/main_elegir_crear_script.sh"
    fi

    if [[ $opcionMenuPrincipal = "31" ]]; then 
        if [[ $walletName = ""  ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_send_to_wallet.sh"
        fi
    fi

    if [[ $opcionMenuPrincipal = "32" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
            printf "\nDebe elegir wallet y script primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_send_to_script.sh"
        fi
    fi


    if [[ $opcionMenuPrincipal = "33" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
            printf "\nDebe elegir wallet y script primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_redeem_from_script.sh"
        fi 
    fi  
    
    if [[ $opcionMenuPrincipal = "41" ]]; then 
        if [[ $walletAddr = "" ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_ver_utxo_wallet.sh"
        fi
    fi  

    if [[ $opcionMenuPrincipal = "42" ]]; then 
        if [[ $walletAddr = "" ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_ver_utxo_wallet_all.sh"
        fi

    fi  

    if [[ $opcionMenuPrincipal = "43" ]]; then 
        if [[ $walletAddr = "" ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_elegir_addr_wallet.sh"
        fi
    fi  

    if [[ $opcionMenuPrincipal = "44" ]]; then 
        if [[ $scriptName = "" ]]; then
            printf "\nDebe elegir script primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_ver_utxo_script.sh"
        fi
    fi  


    if [[ $opcionMenuPrincipal = "51" ]]; then 
        if [[ $walletName = ""  ]]; then
            printf "\nDebe elegir wallet primero\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_ver_balance_wallet.sh"
        fi
    fi

    if [[ $opcionMenuPrincipal = "52" ]]; then 
        if [[  $scriptName = "" ]]; then
             printf "\nDebe elegir script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/main/main_ver_balance_script.sh"
        fi
    fi


    if [[ $opcionMenuPrincipal = "61" ]]; then 
        
        source "$FALCON_DEVS_SCRIPTS/main/main_datum_calcular_hash.sh"
        
    fi  

    if [[ $opcionMenuPrincipal = "62" ]]; then 
        
        source "$FALCON_DEVS_SCRIPTS/main/main_datum_buscar_hash.sh"

    fi

    if [[ $opcionMenuPrincipal = "7"    ]]; then 
        source "$FALCON_DEVS_SCRIPTS/mint/mint.sh"
        opcionMenuPrincipal=""
    fi

    
    if [[ $opcionMenuPrincipal = "8" ]]; then 
        source "$FALCON_DEVS_SCRIPTS/pab/pab.sh"  
        opcionMenuPrincipal=""
    fi

    if [[ $opcionMenuPrincipal = "9"    ]]; then 
        source "$FALCON_DEVS_SCRIPTS/tools/tools.sh"
        opcionMenuPrincipal=""
    fi

    if [[ $opcionMenuPrincipal = "99" ]]; then 
        printf "\nIngrese mínimo ADA por transacción:\n"
        minimoADA=
        while [[ $minimoADA = "" ]]; do
            read minimoADA
        done
    fi



done