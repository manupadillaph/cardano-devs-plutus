#!/bin/bash



minimoADA="1800000"

opcionMenuPrincipal=""

while ! [[ $opcionMenuPrincipal = "0" ]]; do

    printf "\nOPERACIONES CON WALLET Y SCRIPT\n"
 
    echo "1: Elegir o Crear Wallet (${walletName})"  
    echo "2: Elegir o Crear Scipt (${scriptName})"  

    echo "3: Enviar ADA o Tokens a Wallet"

    echo "4: Enviar ADA o Tokens a Script"
    echo "5: Redeem ADA o Tokens de Script"

    echo "6: Ver utxo de Wallet"
    echo "7: Ver utxo de Script"

    echo "8: Balance de Wallet"
    echo "9: Balance de Script"

    echo "10: Calcular Hash de Datum"
    echo "11: Buscar Datum de Hash"

    echo "12: Mint Tokens Free"
    echo "13: Mint Tokens NFT"
    echo "14: Mint Tokens Plus"
    echo "15: Mint Tokens Signed"

    echo "16: Set minimo ADA en tx ($minimoADA)"

    echo "0: Salir"

    #read -n 1 -s opcionMenuPrincipal
    read  opcionMenuPrincipal

    if [[ $opcionMenuPrincipal = "1" ]]; then 
        source "./elegir_crear_Wallet.sh"    
    fi

    if [[ $opcionMenuPrincipal = "2" ]]; then 
        source "./elegir_crear_Script.sh"
    fi

    if [[ $opcionMenuPrincipal = "3" ]]; then 
        if [[ $walletName = ""  ]]; then
             printf "\nDebe elegir wallet primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "./sendToWallet.sh"
        fi
    fi

    if [[ $opcionMenuPrincipal = "4" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
             printf "\nDebe elegir wallet y script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "./sendToScript.sh"
        fi
    fi


    if [[ $opcionMenuPrincipal = "5" ]]; then 
        if [[ $walletName = "" ||  $scriptName = "" ]]; then
             printf "\nDebe elegir wallet y script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "./redeemFromScript.sh"
        fi 
    fi  

    if [[ $opcionMenuPrincipal = "6" ]]; then 
        if [[ $walletAddr = "" ]]; then
             printf "\nDebe elegir wallet primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            echo "Wallet Address:" $walletAddr
            
            result=$($CARDANO_NODE/cardano-cli query utxo\
                --address $walletAddr --testnet-magic $TESTNET_MAGIC)

            printf "\nUtxo at Wallet:\n"

            echo "$result" 

            echo; read -rsn1 -p "Press any key to continue . . ."; echo 
        fi

    fi  

    if [[ $opcionMenuPrincipal = "7" ]]; then 
        if [[ $scriptName = "" ]]; then
             printf "\nDebe elegir script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            echo "Script Address:" $scriptAddr
            
            result=$($CARDANO_NODE/cardano-cli query utxo\
                --address $scriptAddr --testnet-magic $TESTNET_MAGIC)

            printf "\nUtxo at Scritp:\n"

            echo "$result" 

            echo; read -rsn1 -p "Press any key to continue . . ."; echo 
        fi

    fi  

    if [[ $opcionMenuPrincipal = "8" ]]; then 
        if [[ $walletName = ""  ]]; then
             printf "\nDebe elegir wallet primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "./balanceWallet.sh"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        fi
    fi

    if [[ $opcionMenuPrincipal = "9" ]]; then 
        if [[  $scriptName = "" ]]; then
             printf "\nDebe elegir script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else
            source "./balanceScript.sh"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo

        fi
    fi


    if [[ $opcionMenuPrincipal = "10" ]]; then 
        
        printf "\nDatum ---- \n"

        datumFile=""

        until [[ -f "files/datums/$datumFile.json"  ]]
        do

            printf "\nIngrese nombre del archivo Datum (DEF): "
            read datumFile
            if [[ $datumFile = "" ]]; then 
                datumFile="DEF"
            fi
            
            if ! [[ -f "files/datums/$datumFile.json"  ]]
            then
                printf "\nDatum file ${datumFile}.json no existe\n"
            fi

            printf "\nDesea crear datum file (y/n): "
            read -n 1 -s opcion
            if [[ $opcion = "y" ]]; then 

                #cd PATH_CABAL
                
                printf "\nIngrese creator ($walletSig): "
                read datumCreator
                if [[ $datumCreator = "" ]]; then 
                    datumCreator="$walletSig"
                fi
                printf "\nIngrese deadline (1656301816000): "
                read datumDeadline
                if [[ $datumDeadline = "" ]]; then 
                    datumDeadline="1656301816000"
                fi
                printf "\nIngrese name (55): "
                read datumName
                if [[ $datumName = "" ]]; then 
                    datumName="55"
                fi
                printf "\nIngrese qty (3000111): "
                read datumQty
                if [[ $datumQty = "" ]]; then 
                    datumQty="3000111"
                fi

                printf "%s\n%s\n%s\n" "1" "files/datums" "$datumFile" "$datumCreator" "$datumDeadline" "$datumName" "$datumQty" | cabal exec deploy-smart-contracts-auto-exe  
                #echo $'5\nttt\n' | cabal exec cliente-plazo-fijo-exe
                #cat - | cabal exec cliente-plazo-fijo-exe $@ | tee testnet/files/temp-outputcat

            # cd PATH_SCRIPTS

            fi

            
        done

        printf "\nDatum JSON: "
        cat files/datums/$datumFile.json | jq '.'

        datumHash=$($CARDANO_NODE/cardano-cli transaction hash-script-data --script-data-file "files/datums/$datumFile.json")
        
        printf "\nDatum Hash: "
        echo $datumHash   

        echo; read -rsn1 -p "Press any key to continue . . ."; echo
        
    fi  

    if [[ $opcionMenuPrincipal = "11" ]]; then 
        
        printf "\nDatum ---- \n"

        printf "\nIngrese Hash a Buscar: "
        datumHashBuscar=
        while [[ $datumHashBuscar = "" ]]; do
            read datumHashBuscar
        done

        FILE_encontrada=""

        for FILE in files/datums/*; 
        do 

            datumHash=$($CARDANO_NODE/cardano-cli transaction hash-script-data --script-data-file "$FILE")

            echo $FILE ": " $datumHash ; 

            if [[ "$datumHashBuscar" = "$datumHash" ]]; then 
                 printf "Econtrado HASH\n"
                FILE_encontrada=$FILE
            fi

        done 
        
        if ! [[ $FILE_encontrada = "" ]]; then 

            printf "\nEcontrado HASH en: %s \n" "$FILE_encontrada" ; 

            cat $FILE_encontrada | jq '.'

            echo; read -rsn1 -p "Press any key to continue . . ."; echo

        fi

    fi

    if [[ $opcionMenuPrincipal = "12" || $opcionMenuPrincipal = "13"  || $opcionMenuPrincipal = "14"  ||$opcionMenuPrincipal = "15"     ]]; then 
        
        if [[ $walletName = "" ]]; then
             printf "\nDebe elegir wallet primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else

            echo " "
            echo "Elija tx in: "

            tokens=()
            tokensTotal=()
            lovelaceTotal=0

            source "./getutxo.sh"

            echo "Txin Elegidas: "
            
            results=""
            while IFS= read -r txin
            do
                results="$results\n$($CARDANO_NODE/cardano-cli query utxo\
                --tx-in $txin --testnet-magic $TESTNET_MAGIC)"
            done < "files/wallets/${walletName}.utxo"

            echo "$results" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl 
                
            echo " "
            echo "Elija cual desea usar para collateral y como parametro de la mint: "
            read collateralIx

            walletTxIn=$(cat "files/wallets/${walletName}.utxo" | sed -n ${collateralIx}p)

            echo "walletTxIn: "$walletTxIn
            echo "walletTxInArray: "$walletTxInArray

            swChangeTokens=0
            walletTxOutArrayForChangeOfTokens="$walletAddr $minimoADA lovelace"

            for i in ${!tokens[@]}; do
                txout="${tokensTotal[$i]} ${tokens[$i]}"
                walletTxOutArrayForChangeOfTokens="$walletTxOutArrayForChangeOfTokens + $txout "
                swChangeTokens=1
            done

            if [[ $swChangeTokens = 1 ]]; then
                printf "\nwalletTxOutArrayForChangeOfTokens:\n"
                echo $walletTxOutArrayForChangeOfTokens
            fi 

            if [[ $opcionMenuPrincipal = "12" ]]; then 
                source "./mint-token-free.sh" 
            fi

            if [[ $opcionMenuPrincipal = "13" ]]; then 
                source "./mint-token-nft.sh"
            fi

            if [[ $opcionMenuPrincipal = "14" ]]; then 
                source "./mint-token-plus.sh"
            fi

            if [[ $opcionMenuPrincipal = "15" ]]; then 
                source "./mint-token-signed.sh"  
            fi

        fi

    fi
    if [[ $opcionMenuPrincipal = "16" ]]; then 
        printf "\nIngrese mínimo ADA por transacción:\n"
        minimoADA=
        while [[ $minimoADA = "" ]]; do
            read minimoADA
        done
    fi


done