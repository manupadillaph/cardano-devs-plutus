#!/bin/bash

CWD=$(pwd)

opcionMenuTools=""

while ! [[ $opcionMenuTools = "0" ]]; do

    



    printf "\nOPERACIONES:\n"
    
    
    echo "--"

    echo "11: Iniciar Cardano Node"  
    echo "12: Check Cardano Node"  

    echo "--"

    echo "2: Iniciar Wallet Server"

    echo "--"

    printf "NIX SHELL:\n"

    echo "--"

    user=whoami
    if [[ $user = "root" ]]; then

        echo "31: Cambiar a usuario normal"
        echo "    No puede Iniciar NIX desde root"
        echo " "
    
    else

        echo "32: Iniciar Nix Shell"  

    fi

    echo "--"

    echo "4: Iniciar Chain Index Server"

    echo "--"

    echo "51: Inicicar Playgroud Server"
    echo "52: Inicicar Playground Client"

    echo "--"

    echo "6: Inicicar Plutus Docs"

    echo "--"

    echo "7: Exportar variables ENV"

    echo "Para set env en esta sesion ejecute en promt:"
    echo "source \"$FALCON_DEVS_SCRIPTS/tools/write-env-list.sh\""

    echo "--"

    echo "9: Arreglar Permisos de esta carpeta"
    echo "   Carpeta: "$CWD
    echo " "

    echo "--"

    echo "0: Regresar al Menu Principal"

    echo "--"

    echo "Opcion: "

    #read -n 1 -s opcionMenuTools
    read opcionMenuTools

    if [[ $opcionMenuTools = "11" ]]; then 
        bash "$INIT_NODE"    
    fi

    if [[ $opcionMenuTools = "12" ]]; then 
        bash "$CHECK_NODE" 
        
        echo; read -rsn1 -p "Press any key to continue . . ."; echo
    fi

    if [[ $opcionMenuTools = "2" ]]; then 
        bash "$INIT_WALLET"
    fi

    if [[ $opcionMenuTools = "31" ]]; then 
        sudo -su $USUARIO
    fi

    if [[ $opcionMenuTools = "32" ]]; then 
        bash "$NIX_SHELL"
    fi

    if [[ $opcionMenuTools = "4" ]]; then 
        bash "$INIT_CHAIN"
    fi

    if [[ $opcionMenuTools = "51" ]]; then 
        bash "$INIT_PLAY_SERVER"
    fi

    if [[ $opcionMenuTools = "52" ]]; then 
        bash "$INIT_PLAY_CLIENT"
    fi

    if [[ $opcionMenuTools = "6" ]]; then 
        bash "$INIT_DOCS"
    fi

    if [[ $opcionMenuTools = "7" ]]; then 
        bash "$WRITE_ENV"
        echo; read -rsn1 -p "Press any key to continue . . ."; echo
        
    fi

    if [[ $opcionMenuTools = "9" ]]; then 
        echo $FIX_PERMISOS

        bash "$FIX_PERMISOS"

    fi

done