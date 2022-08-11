#!/bin/bash



minimoADA="1800000"

opcionMenuPab=""

while ! [[ $opcionMenuPab = "0" ]]; do

    printf "\nOPERACIONES EN PAB CON WALLET Y SCRIPT\n"
 
    echo "1: Elegir o Crear Wallet (${walletName})"  
    echo "2: Elegir Validador (${scriptName} - Id: $scriptNumero)"  

    echo "3: Iniciar Pab Database"
    echo "4: Iniciar Pab API Server"

    echo "5: Enviar ADA o Tokens a Script"
    echo "6: Redeem ADA o Tokens de Script"

    echo "--"

    echo "0: Regresar al Menu Principal"

    echo "--"

    echo "Opcion: "

    #read -n 1 -s opcionMenuPab
    read  opcionMenuPab

    if [[ $opcionMenuPab = "1" ]]; then 
        source "$FALCON_DEVS_SCRIPTS/main/main_elegir_crear_wallet.sh"    
    fi

    if [[ $opcionMenuPab = "2" ]]; then 
        source "$FALCON_DEVS_SCRIPTS/pab/pab_elegir_validador.sh"
    fi

    if [[ $opcionMenuPab = "3" ]]; then 
        if [[  $scriptName = "" || $scriptNumero = ""  ]]; then
            printf "\nDebe elegir validador\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/pab/pab_init_database.sh"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        fi
    fi

    if [[ $opcionMenuPab = "4" ]]; then 
        if [[ $walletName = "" || $scriptName = "" || $scriptNumero = ""  ]]; then
             printf "\nDebe elegir wallet y validador primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            if ! [[ -f "$FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.json" && -f "$FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.id"  ]]
            then
                printf "\nWallet ${walletName} JSON o id files no existen. No se pueden inicicar los servicios de PAB con esta wallet.\n"
                echo; read -rsn1 -p "Press any key to continue . . ."; echo
            else
                source "$FALCON_DEVS_SCRIPTS/pab/pab_init_server.sh"
            fi
        fi
    fi

    if [[ $opcionMenuPab = "5" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" || $scriptNumero = "" ]]; then
             printf "\nDebe elegir wallet y validador primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/pab/pab_send_to_cript.sh"
        fi
    fi


    if [[ $opcionMenuPab = "6" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
             printf "\nDebe elegir wallet y validador primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "$FALCON_DEVS_SCRIPTS/pab/pab-redeem_from_script.sh"
        fi 
    fi  



done