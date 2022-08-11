#!/bin/bash


echo " "
echo "Elija tx in: "

tokens=()
tokensTotal=()
lovelaceTotal=0

source "$SCRIPTS/main/main_elegir_utxo_wallet.sh"

if [[ $lovelaceTotal = 0 ]]; then

    echo "Error en utxos elegidas: No se encuentran fondos para usar"
    echo; read -rsn1 -p "Press any key to continue . . ."; echo
else

    echo "Txin Elegidas: "

    results=""
    while IFS= read -r txin
    do
        results="$results\n$($CARDANO_NODE/cardano-cli query utxo\
        --tx-in $txin --$TESTNET_MAGIC)"
    done < "$HASKELL_FILES/wallets/${walletName}.utxo"

    echo "$results" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl 
        
    echo " "
    echo "Elija cual desea usar para collateral: "
    read collateralIx

    walletTxIn=$(cat "$HASKELL_FILES/wallets/${walletName}.utxo" | sed -n ${collateralIx}p)

    echo "walletTxIn: "$walletTxIn
    echo "walletTxInArray: "$walletTxInArray

    # tokens=()
    # tokensTotal=()
    # NO INICIO LOS TOTALES DE TOKENS POR QUE SE SUMAN CON LOS INPUTS DE LA BILETERA Y LOS NECESITO ABAJO PARA LA walletTxOutArrayForChangeOfTokens
    # EN CAMBIO SI INICIO lovelaceTotal POR QUE ME SIRVE PARA CONTROLAR SI ELIGIO ALGO CORRECTO Y TAMPOCO NECESITO EL TOTAL QUE VIENE DE LA BILLETERA.
    lovelaceTotal=0

    echo " "
    echo "Elija tx para redeem: "

    source "$SCRIPTS/main/main_elegir_utxo_script.sh"

    if [[ $lovelaceTotal = 0 ]]; then

        echo "Error en utxos: No se encuentran fondos para redeem"
        echo; read -rsn1 -p "Press any key to continue . . ."; echo
    else

        read -r scriptTxIn  < $HASKELL_FILES/validators/${scriptName}.utxo
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

        $CARDANO_NODE/cardano-cli query protocol-parameters \
            --out-file $HASKELL_FILES/config/tx/protocol.json --$TESTNET_MAGIC 

        $CARDANO_NODE/cardano-cli query tip --$TESTNET_MAGIC | jq -r .slot >$HASKELL_FILES/config/tx/tip.slot

        tipSlot=$(cat $HASKELL_FILES/config/tx/tip.slot)

        source "$SCRIPTS/main/main_datum_elegir_crear.sh"


        printf "\nRedeemer ---- \n"

        redeemerFile=""

        until [[ -f "$HASKELL_FILES/redeemers/$redeemerFile.json"  ]]
        do
            printf "\nIngrese nombre para el archivo Redeemer (DEF): "
            read redeemerFile
            if [[ $redeemerFile = "" ]]; then 
                redeemerFile="DEF"
            fi

            if ! [[ -f "$HASKELL_FILES/redeemers/$redeemerFile.json"  ]]
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

                printf "%s\n%s\n%s\n" "2" "$HASKELL_FILES/redeemers" "$redeemerFile" "$redeemerOpcion" | cabal exec deploy-smart-contracts-auto
                
                cd $CWD

            fi

        done

        printf "\n\nRealizando transferencia...\n\n"


        if [[ $swChangeTokens = 1 ]]; then

            echo --babbage-era \
                --$TESTNET_MAGIC \
                --change-address $walletAddr \
                $walletTxInArray \
                --tx-in $scriptTxIn \
                --tx-in-script-file $HASKELL_FILES/validators/$scriptName.plutus \
                --tx-in-datum-file  $HASKELL_FILES/datums/$datumFile.json \
                --tx-in-redeemer-file $HASKELL_FILES/redeemers/$redeemerFile.json  \
                --tx-in-collateral $walletTxIn \
                --tx-out "$walletTxOutArrayForChangeOfTokens" \
                --required-signer-hash $walletSig \
                --required-signer=$HASKELL_FILES/wallets/${walletName}.skey \
                --protocol-params-file $HASKELL_FILES/config/tx/protocol.json \
                --invalid-before ${tipSlot} \
                --out-file $HASKELL_FILES/transacciones/${scriptName}.body 
       
            echo ""

            $CARDANO_NODE/cardano-cli transaction build \
                --babbage-era \
                --$TESTNET_MAGIC \
                --change-address $walletAddr \
                $walletTxInArray \
                --tx-in $scriptTxIn \
                --tx-in-script-file $HASKELL_FILES/validators/$scriptName.plutus \
                --tx-in-datum-file  $HASKELL_FILES/datums/$datumFile.json \
                --tx-in-redeemer-file $HASKELL_FILES/redeemers/$redeemerFile.json  \
                --tx-in-collateral $walletTxIn \
                --tx-out "$walletTxOutArrayForChangeOfTokens" \
                --required-signer-hash $walletSig \
                --required-signer=$HASKELL_FILES/wallets/${walletName}.skey \
                --protocol-params-file $HASKELL_FILES/config/tx/protocol.json \
                --invalid-before ${tipSlot} \
                --out-file $HASKELL_FILES/transacciones/${scriptName}.body 
        else
            echo --babbage-era \
                --$TESTNET_MAGIC \
                --change-address $walletAddr \
                $walletTxInArray \
                --tx-in $scriptTxIn \
                --tx-in-script-file $HASKELL_FILES/validators/$scriptName.plutus \
                --tx-in-datum-file  $HASKELL_FILES/datums/$datumFile.json \
                --tx-in-redeemer-file $HASKELL_FILES/redeemers/$redeemerFile.json  \
                --tx-in-collateral $walletTxIn \
                --required-signer-hash $walletSig \
                --required-signer=$HASKELL_FILES/wallets/${walletName}.skey \
                --protocol-params-file $HASKELL_FILES/config/tx/protocol.json \
                --invalid-before ${tipSlot} \
                --out-file $HASKELL_FILES/transacciones/${scriptName}.body 

            echo ""
            
            $CARDANO_NODE/cardano-cli transaction build \
                --babbage-era \
                --$TESTNET_MAGIC \
                --change-address $walletAddr \
                $walletTxInArray \
                --tx-in $scriptTxIn \
                --tx-in-script-file $HASKELL_FILES/validators/$scriptName.plutus \
                --tx-in-datum-file  $HASKELL_FILES/datums/$datumFile.json \
                --tx-in-redeemer-file $HASKELL_FILES/redeemers/$redeemerFile.json  \
                --tx-in-collateral $walletTxIn \
                --required-signer-hash $walletSig \
                --required-signer=$HASKELL_FILES/wallets/${walletName}.skey \
                --protocol-params-file $HASKELL_FILES/config/tx/protocol.json \
                --invalid-before ${tipSlot} \
                --out-file $HASKELL_FILES/transacciones/${scriptName}.body 
        fi

        if [ "$?" == "0" ]; then

            $CARDANO_NODE/cardano-cli transaction sign \
                --tx-body-file $HASKELL_FILES/transacciones/${scriptName}.body \
                --signing-key-file $HASKELL_FILES/wallets/${walletName}.skey \
                --$TESTNET_MAGIC \
                --out-file  $HASKELL_FILES/transacciones/${scriptName}.signed

            if [ "$?" == "0" ]; then

                $CARDANO_NODE/cardano-cli transaction submit \
                    --$TESTNET_MAGIC \
                    --tx-file $HASKELL_FILES/transacciones/${scriptName}.signed


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