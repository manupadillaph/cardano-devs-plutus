#!/bin/bash


echo " "
echo "Elija tx in: "

tokens=()
tokensTotal=()
lovelaceTotal=0

source "$SCRIPTS/main_elegir_utxo_wallet.sh"

if [[ $lovelaceTotal = 0 ]]; then

    echo "Error en utxos elegidas: No se encuentran fondos para usar"
    echo; read -rsn1 -p "Press any key to continue . . ."; echo
else

    echo "Txin Elegidas: "

    results=""
    while IFS= read -r txin
    do
        results="$results\n$($CARDANO_NODE/cardano-cli query utxo\
        --tx-in $txin --testnet-magic $TESTNET_MAGIC)"
    done < "$SCRIPTS_FILES/wallets/${walletName}.utxo"

    echo "$results" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl 
        
    echo " "
    echo "Elija cual desea usar para collateral: "
    read collateralIx

    walletTxIn=$(cat "$SCRIPTS_FILES/wallets/${walletName}.utxo" | sed -n ${collateralIx}p)

    echo "walletTxIn: "$walletTxIn
    echo "walletTxInArray: "$walletTxInArray

    # walletTxIn=$(cat $SCRIPTS_FILES/wallets/${walletName}.utxo)

    # walletNroUTXO=
    # while [[ "$walletNroUTXO" = "" ]]; do
    #     printf "\nUtxo At Wallet:\n"

    #     result=$($CARDANO_NODE/cardano-cli query utxo\
    #     --address $walletAddr --testnet-magic $TESTNET_MAGIC)

    #     echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl

    #     printf "\nUltima dirección utilizada: %s" $walletTxIn
        
    #     printf "\nElija utxo para pagar collateral (presione ENTER para recargar utxo o 0 para no cambiar): "

    #     read walletNroUTXO
    # done

    # if [[ $walletNroUTXO != "0" ]]; then 
        
    #     #echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" 

    #     TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 1p)
    #     TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 2p)

    #     echo $TxHash#$TxIx

    #     echo $TxHash#$TxIx>$SCRIPTS_FILES/wallets/${walletName}.utxo

    # fi

    # walletTxIn=$(cat $SCRIPTS_FILES/wallets/${walletName}.utxo)
    
    # tokens=()
    # tokensTotal=()
    # NO INICIO LOS TOTALES DE TOKENS POR QUE SE SUMAN CON LOS INPUTS DE LA BILETERA Y LOS NECESITO ABAJO PARA LA walletTxOutArrayForChangeOfTokens
    # EN CAMBIO SI INICIO lovelaceTotal POR QUE ME SIRVE PARA CONTROLAR SI ELIGIO ALGO CORRECTO Y TAMPOCO NECESITO EL TOTAL QUE VIENE DE LA BILLETERA.
    lovelaceTotal=0

    echo " "
    echo "Elija tx para redeem: "

    source "$SCRIPTS/main_elegir_utxo_script.sh"

    if [[ $lovelaceTotal = 0 ]]; then

        echo "Error en utxos: No se encuentran fondos para redeem"
        echo; read -rsn1 -p "Press any key to continue . . ."; echo
    else

        read -r scriptTxIn  < $SCRIPTS_FILES/validators/${scriptName}.utxo
        echo "scriptTxIn: "$scriptTxIn
        echo "scriptTxInArray: "$scriptTxInArray

        swChangeTokens=0
        walletTxOutArrayForChangeOfTokens="$walletAddr $minimoADA lovelace"

        for i in ${!tokens[@]}; do
            txout="${tokensTotal[$i]} ${tokens[$i]}"
            walletTxOutArrayForChangeOfTokens="$walletTxOutArrayForChangeOfTokens + $txout "
            swChangeTokens=1
        done

        if [[ $swChangeTokens = 1 ]]; then

            printf "\nwallet TxOutArray For Change Of Tokens:\n"
            echo $walletTxOutArrayForChangeOfTokens

        fi 

        # result=$($CARDANO_NODE/cardano-cli query utxo\
        #     --address $scriptAddr --testnet-magic $TESTNET_MAGIC)

        # printf "\nUtxo at Scritp:\n"

        # echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl

        # printf "\nElija utxo que desea redeem (1): "
        # read scriptNroUTXO
        # if [[ $scriptNroUTXO = "" ]]; then 
        #     scriptNroUTXO="1"
        # fi
        # #echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" 

        # TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${scriptNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 1p)
        # TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${scriptNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 2p)

        # echo $TxHash#$TxIx

        # echo $TxHash#$TxIx>$SCRIPTS_FILES/validators/${scriptName}.utxo

        # scriptTxIn=$(cat $SCRIPTS_FILES/validators/${scriptName}.utxo)



        $CARDANO_NODE/cardano-cli query protocol-parameters \
            --out-file $SCRIPTS_FILES/config/protocol.json --testnet-magic $TESTNET_MAGIC 

        $CARDANO_NODE/cardano-cli query tip --testnet-magic $TESTNET_MAGIC | jq -r .slot >$SCRIPTS_FILES/config/tip.slot

        tipSlot=$(cat $SCRIPTS_FILES/config/tip.slot)

        source "$SCRIPTS/main_datum_elegir_crear.sh"


        printf "\nRedeemer ---- \n"

        redeemerFile=""

        until [[ -f "$SCRIPTS_FILES/redeemers/$redeemerFile.json"  ]]
        do
            printf "\nIngrese nombre para el archivo Redeemer (DEF): "
            read redeemerFile
            if [[ $redeemerFile = "" ]]; then 
                redeemerFile="DEF"
            fi

            if ! [[ -f "$SCRIPTS_FILES/redeemers/$redeemerFile.json"  ]]
            then
                printf "\nRedeemer file $redeemerFile.json no existe\n"
            fi
            
            printf "\nDesea crear Redeemer file (y/n):\n"
            read -n 1 -s opcion
            if [[ $opcion = "y" ]]; then 

                printf "\nIngrese redeemer (55): "
                read redeemerOpcion
                if [[ $redeemerOpcion = "" ]]; then 
                    redeemerOpcion="55"
                fi

                #Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
                CWD=$(pwd)
                cd $HASKELL

                printf "%s\n%s\n%s\n" "2" "$SCRIPTS_FILES/redeemers" "$redeemerFile" "$redeemerOpcion" | cabal exec deploy-smart-contracts-auto
                
                cd $CWD

            fi

        done

        printf "\n\nRealizando transferencia...\n\n"


        if [[ $swChangeTokens = 1 ]]; then
            $CARDANO_NODE/cardano-cli transaction build \
                --babbage-era \
                --testnet-magic $TESTNET_MAGIC \
                --change-address $walletAddr \
                $walletTxInArray \
                --tx-in $scriptTxIn \
                --tx-in-script-file $SCRIPTS_FILES/validators/$scriptName.plutus \
                --tx-in-datum-file  $SCRIPTS_FILES/datums/$datumFile.json \
                --tx-in-redeemer-file $SCRIPTS_FILES/redeemers/$redeemerFile.json  \
                --tx-in-collateral $walletTxIn \
                --tx-out "$walletTxOutArrayForChangeOfTokens" \
                --required-signer-hash $walletSig \
                --required-signer=$SCRIPTS_FILES/wallets/${walletName}.skey \
                --protocol-params-file $SCRIPTS_FILES/config/protocol.json \
                --invalid-before ${tipSlot} \
                --out-file $SCRIPTS_FILES/transacciones/${scriptName}.body 
        else
            $CARDANO_NODE/cardano-cli transaction build \
                --babbage-era \
                --testnet-magic $TESTNET_MAGIC \
                --change-address $walletAddr \
                $walletTxInArray \
                --tx-in $scriptTxIn \
                --tx-in-script-file $SCRIPTS_FILES/validators/$scriptName.plutus \
                --tx-in-datum-file  $SCRIPTS_FILES/datums/$datumFile.json \
                --tx-in-redeemer-file $SCRIPTS_FILES/redeemers/$redeemerFile.json  \
                --tx-in-collateral $walletTxIn \
                --required-signer-hash $walletSig \
                --required-signer=$SCRIPTS_FILES/wallets/${walletName}.skey \
                --protocol-params-file $SCRIPTS_FILES/config/protocol.json \
                --invalid-before ${tipSlot} \
                --out-file $SCRIPTS_FILES/transacciones/${scriptName}.body 
        fi

        if [ "$?" == "0" ]; then

            $CARDANO_NODE/cardano-cli transaction sign \
                --tx-body-file $SCRIPTS_FILES/transacciones/${scriptName}.body \
                --signing-key-file $SCRIPTS_FILES/wallets/${walletName}.skey \
                --testnet-magic $TESTNET_MAGIC \
                --out-file  $SCRIPTS_FILES/transacciones/${scriptName}.signed

            if [ "$?" == "0" ]; then

                $CARDANO_NODE/cardano-cli transaction submit \
                    --testnet-magic $TESTNET_MAGIC \
                    --tx-file $SCRIPTS_FILES/transacciones/${scriptName}.signed


                if [ "$?" == "0" ]; then
                    printf "\nTransferencia Realidada!\n"
                    echo; read -rsn1 -p "Press any key to continue . . ."; echo
                else
                    printf "\nError en submit Transferencia\n"
                    echo; read -rsn1 -p "Press any key to continue . . ."; echo
                fi
            else
                printf "\nError en sign Transferencia\n"
                echo; read -rsn1 -p "Press any key to continue . . ."; echo
            fi
        else
            printf "\nError en build Transferencia\n"
            echo; read -rsn1 -p "Press any key to continue . . ."; echo
        fi
    fi
fi