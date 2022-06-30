#!/bin/bash

opcionMenuPrincipal=""

while ! [[ $opcionMenuPrincipal = "0" ]]; do

    printf "\nOPERACIONES CON WALLET Y SCRIPT\n"
 
    echo "1: Elegir o Crear Wallet (${walletName})"  
    echo "2: Elegir o Crear Scipt (${scriptName})"  
    echo "3: Depositar Dinero"
    echo "4: Retirar Dinero"
    echo "5: Ver utxo de Script"
    echo "6: Calcular Hash de Datum"
    echo "7: Buscar Datum de Hash"

    echo "0: Salir"

    read -n 1 -s opcionMenuPrincipal

    if [[ $opcionMenuPrincipal = "1" ]]; then 
        
        walletName=""
        until [[ -f "files/wallets/${walletName}.addr" && -f "files/wallets/${walletName}.pkh" && -f "files/wallets/${walletName}.skey"  && -f "files/wallets/${walletName}.vkey"  ]]
        do
            printf "\nNombre Wallet (01): "
            read walletName

            if [[ $walletName = "" ]]; then 
                walletName="01"
            fi

            if ! [[ -f "files/wallets/${walletName}.addr" && -f "files/wallets/${walletName}.pkh" && -f "files/wallets/${walletName}.skey"  && -f "files/wallets/${walletName}.vkey"  ]]
            then
                printf "\nWallet file ${walletName} no existe, desea crearla (y/n)? \nImportante: Necesita tener NODO y WALLET SERVER configurados e iniciados \n"

                read -n 1 -s opcion
                if [[ $opcion = "y" ]]; then 

                    bash ${SCRIPTS}/create-wallet-con-cardano-wallet.sh ${walletName} ${walletName}

                    cp $CARDANO_WALLET/wallets/${walletName}.json files/wallets/${walletName}.json
                    cp $CARDANO_WALLET/wallets/${walletName}.addr files/wallets/${walletName}.addr
                    cp $CARDANO_WALLET/wallets/${walletName}.id files/wallets/${walletName}.id
                    cp $CARDANO_WALLET/wallets/${walletName}.pkh files/wallets/${walletName}.pkh
                    cp $CARDANO_WALLET/wallets/${walletName}.skey files/wallets/${walletName}.skey
                    cp $CARDANO_WALLET/wallets/${walletName}.vkey files/wallets/${walletName}.vkey
                fi
            fi
        done

        walletAddr=$(cat files/wallets/${walletName}.addr)

        echo "Wallet Address:" $walletAddr

        walletSig=$(cat files/wallets/${walletName}.pkh)

        echo "Payment Key HASH:" $walletSig
    fi

    if [[ $opcionMenuPrincipal = "2" ]]; then 

        scriptName=""
        until [[ -f "files/validators/${scriptName}.plutus" && -f "files/validators/${scriptName}.hash"   && -f "files/validators/${scriptName}.addr" ]]
        do

            printf "\nNombre del Script: "

            scriptName=
            while [[ $scriptName = "" ]]; do
                read scriptName
            done

            if ! [[ -f "files/validators/${scriptName}.plutus" && -f "files/validators/${scriptName}.hash" ]]
            then
                printf "\nValidator script file ${scriptName} no existe\n"
            else
                $CARDANO_NODE/cardano-cli address build  \
                --payment-script-file files/validators/${scriptName}.plutus --out-file files/validators/${scriptName}.addr --testnet-magic $TESTNET_MAGIC
            fi

            printf "\nDesea crear files .plutus, .hash del actual validator en haskell (y/n)\nImportante: Necesita tener NODO configurado e iniciado\n"
            read -n 1 -s opcion
            if [[ $opcion = "y" ]]; then 
                printf "%s\n%s\n%s\n" "1" "files/validators" "$scriptName" | cabal exec deploy-smart-contracts-auto-exe  
                printf "%s\n%s\n%s\n" "2" "files/validators" "$scriptName" | cabal exec deploy-smart-contracts-auto-exe  
                
            fi

        done

        scriptAddr=$(cat files/validators/${scriptName}.addr)

        echo "Script Address:" $scriptAddr
    fi

    if [[ $opcionMenuPrincipal = "3" ]]; then 

        if [[ $walletName = "" ||  $scriptName = "" ]]; then
             printf "\nDebe elegir wallet y script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else

            walletTxIn=$(cat files/wallets/${walletName}.utxo)

            walletNroUTXO=
            while [[ "$walletNroUTXO" = "" ]]; do
                printf "\nUtxo At Wallet:\n"

                result=$($CARDANO_NODE/cardano-cli query utxo\
                --address $walletAddr --testnet-magic $TESTNET_MAGIC)

                echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | nl

                printf "\nUltima dirección utilizada: %s" $walletTxIn
                
                printf "\nElija utxo para pagar script (presione ENTER para recargar utxo o 0 para no cambiar): "

                read walletNroUTXO
            done

            if [[ $walletNroUTXO != "0" ]]; then 
                
                #echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" 

                TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 1p)
                TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 2p)

                echo $TxHash#$TxIx

                echo $TxHash#$TxIx>files/wallets/${walletName}.utxo

            fi

            walletTxIn=$(cat files/wallets/${walletName}.utxo)

            printf "\nCantidad ADA (3000000): "
            read cantidad
            if [[ $cantidad = "" ]]; then 
                cantidad="3000000"
            fi
            
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

                    printf "%s\n%s\n%s\n" "4" "files/datums" "$datumFile" "$datumCreator" "$datumDeadline" "$datumName" "$datumQty" | cabal exec deploy-smart-contracts-auto-exe  
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

            printf "\nRealizando Transferencia...\n\n"

            $CARDANO_NODE/cardano-cli transaction build \
                --alonzo-era \
                --testnet-magic $TESTNET_MAGIC \
                --change-address $walletAddr \
                --tx-in $walletTxIn \
                --tx-out "$scriptAddr $cantidad lovelace" \
                --tx-out-datum-hash-file files/datums/$datumFile.json \
                --out-file files/transacciones/${scriptName}.body

            if [ "$?" == "0" ]; then

                $CARDANO_NODE/cardano-cli transaction sign \
                    --tx-body-file files/transacciones/${scriptName}.body \
                    --signing-key-file files/wallets/${walletName}.skey \
                    --testnet-magic $TESTNET_MAGIC \
                    --out-file files/transacciones/${scriptName}.signed

                if [ "$?" == "0" ]; then

                    $CARDANO_NODE/cardano-cli transaction submit \
                        --testnet-magic $TESTNET_MAGIC \
                        --tx-file files/transacciones/${scriptName}.signed

                    if [ "$?" == "0" ]; then
                        printf "\nTransferencia Realidada!\n"
                    else
                        printf "\nError en submit Transferencia\n"
                    fi

                else
                    printf "\nError en sign Transferencia\n"
                fi

            else

                printf "\nError en build Transferencia\n"
            fi

        fi
        
    fi


    if [[ $opcionMenuPrincipal = "4" ]]; then 

        if [[ $walletName = "" ||  $scriptName = "" ]]; then
             printf "\nDebe elegir wallet y script primero\n"
             echo; read -rsn1 -p "Press any key to continue . . ."; echo
        else

            walletTxIn=$(cat files/wallets/${walletName}.utxo)

            walletNroUTXO=
            while [[ "$walletNroUTXO" = "" ]]; do
                printf "\nUtxo At Wallet:\n"

                result=$($CARDANO_NODE/cardano-cli query utxo\
                --address $walletAddr --testnet-magic $TESTNET_MAGIC)

                echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | nl

                printf "\nUltima dirección utilizada: %s" $walletTxIn
                
                printf "\nElija utxo para pagar collateral (presione ENTER para recargar utxo o 0 para no cambiar): "

                read walletNroUTXO
            done

            if [[ $walletNroUTXO != "0" ]]; then 
                
                #echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" 

                TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 1p)
                TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 2p)

                echo $TxHash#$TxIx

                echo $TxHash#$TxIx>files/wallets/${walletName}.utxo

            fi

            walletTxIn=$(cat files/wallets/${walletName}.utxo)

            result=$($CARDANO_NODE/cardano-cli query utxo\
                --address $scriptAddr --testnet-magic $TESTNET_MAGIC)

            printf "\nUtxo at Scritp:\n"

            echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\"]+" | nl

            printf "\nElija utxo que desea redeem (1): "
            read scriptNroUTXO
            if [[ $scriptNroUTXO = "" ]]; then 
                scriptNroUTXO="1"
            fi
            #echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" 

            TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${scriptNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 1p)
            TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+]+" | sed -n ${scriptNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 2p)

            echo $TxHash#$TxIx

            echo $TxHash#$TxIx>files/validators/${scriptName}.utxo

            scriptTxIn=$(cat files/validators/${scriptName}.utxo)

            $CARDANO_NODE/cardano-cli query protocol-parameters \
                --out-file files/config/protocol.json --testnet-magic $TESTNET_MAGIC 

            $CARDANO_NODE/cardano-cli query tip --testnet-magic $TESTNET_MAGIC | jq -r .slot >files/config/tip.slot

            tipSlot=$(cat files/config/tip.slot)

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

                    printf "\nIngrese creator ($walletSig): "
                    read datumCreator
                    if [[ $datumCreator = "" ]]; then 
                        datumCreator="$walletSig"
                    fi
                    printf "\nIngrese deadline (1656301816000): " 
                    read datumDeadline
                    if [[ $datumDeadline = "" ]]; then 
                        datumDeadline="1656301816000cd"
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

                    printf "%s\n%s\n%s\n" "4" "files/datums" "$datumFile" "$datumCreator" "$datumDeadline" "$datumName" "$datumQty" | cabal exec deploy-smart-contracts-auto-exe  

                fi

            done

            printf "\nDatum JSON: "
            cat files/datums/$datumFile.json | jq '.'

            datumHash=$($CARDANO_NODE/cardano-cli transaction hash-script-data --script-data-file "files/datums/$datumFile.json")

            printf "\nDatum Hash: "
            echo $datumHash

            printf "\nRedeemer ---- \n"

            redeemerFile=""

            until [[ -f "files/redeemers/$redeemerFile.json"  ]]
            do
                printf "\nIngrese nombre para el archivo Redeemer (DEF): "
                read redeemerFile
                if [[ $redeemerFile = "" ]]; then 
                    redeemerFile="DEF"
                fi

                if ! [[ -f "files/redeemers/$redeemerFile.json"  ]]
                then
                    printf "\nRedeemer file $redeemerFile.json no existe\n"
                fi
                
                printf "\nDesea crear Redeemer file (y/n): "
                read -n 1 -s opcion
                if [[ $opcion = "y" ]]; then 

                    printf "\nIngrese redeemer (1 o 2) (1): "
                    read redeemerOpcion
                    if [[ $redeemerOpcion = "" ]]; then 
                        redeemerOpcion="1"
                    fi
                    printf "%s\n%s\n%s\n" "5" "files/redeemers" "$redeemerFile" "$redeemerOpcion" | cabal exec deploy-smart-contracts-auto-exe 


                fi
            
            done

            printf "\nRealizando transferencia...\n\n"

            $CARDANO_NODE/cardano-cli transaction build \
                --alonzo-era \
                --testnet-magic $TESTNET_MAGIC \
                --change-address $walletAddr \
                --tx-in $scriptTxIn \
                --tx-in-script-file files/validators/$scriptName.plutus \
                --tx-in-datum-file  files/datums/$datumFile.json \
                --tx-in-redeemer-file files/redeemers/$redeemerFile.json  \
                --tx-in-collateral $walletTxIn \
                --required-signer-hash $walletSig \
                --required-signer=files/wallets/${walletName}.skey \
                --protocol-params-file files/config/protocol.json \
                --invalid-before ${tipSlot} \
                --out-file files/transacciones/${scriptName}.body 


            if [ "$?" == "0" ]; then

                $CARDANO_NODE/cardano-cli transaction sign \
                    --tx-body-file files/transacciones/${scriptName}.body \
                    --signing-key-file files/wallets/${walletName}.skey \
                    --testnet-magic $TESTNET_MAGIC \
                    --out-file  files/transacciones/${scriptName}.signed

                if [ "$?" == "0" ]; then

                    $CARDANO_NODE/cardano-cli transaction submit \
                        --testnet-magic $TESTNET_MAGIC \
                        --tx-file files/transacciones/${scriptName}.signed

            
                    if [ "$?" == "0" ]; then
                        printf "\nTransferencia Realidada!\n"
                    else
                        printf "\nError en submit Transferencia\n"
                    fi
                else
                    printf "\nError en sign Transferencia\n"
                fi
            else
                printf "\nError en build Transferencia\n"
            fi

        fi 
    fi  

    if [[ $opcionMenuPrincipal = "5" ]]; then 
        if [[ $scriptName = "" ]]; then
             printf "\nDebe elegir wallet y script primero\n"
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

    if [[ $opcionMenuPrincipal = "6" ]]; then 
        
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

                printf "%s\n%s\n%s\n" "4" "files/datums" "$datumFile" "$datumCreator" "$datumDeadline" "$datumName" "$datumQty" | cabal exec deploy-smart-contracts-auto-exe  
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

    if [[ $opcionMenuPrincipal = "7" ]]; then 
        
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


done