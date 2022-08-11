
#!/bin/bash


echo " "
echo "Elija tx in para enviar: "

tokens=()
tokensTotal=()
lovelaceTotal=0

source "$FALCON_DEVS_SCRIPTS/main/main_elegir_utxo_wallet.sh"

if [[ $lovelaceTotal = 0 ]]; then

    echo "Error en utxos elegidas: No se encuentran fondos para enviar"
    echo; read -rsn1 -p "Press any key to continue . . ."; echo
else

    echo "walletTxIn: "$walletTxIn
    echo "walletTxInArray: "$walletTxInArray

    printf "\nCantidad ADA (Max: $lovelaceTotal sin tener en cuenta Fee, ni $minimoADA ADA para txo de change si sobran Tokens): "
    read cantidad
    if [[ $cantidad = "" ]]; then 
        cantidad="$lovelaceTotal"
    fi

    scriptTxOutMultiAssets="$scriptAddr $cantidad lovelace"

    swChangeTokens=0
    walletTxOutArrayForChangeOfTokens="$walletAddr $minimoADA lovelace"

    for i in ${!tokens[@]}; do
        printf "\nCantidad Token ${tokens[$i]} (Max: ${tokensTotal[$i]}): "
        read cantidadToken
        if [[ $cantidadToken = "" ]]; then 
            cantidadToken="${tokensTotal[$i]}"
        fi
        
        tokensTotal[$i]=$((${tokensTotal[$i]}-$cantidadToken))

        txout="$cantidadToken ${tokens[$i]}"
        scriptTxOutMultiAssets="$scriptTxOutMultiAssets + $txout "

        if [[ ${tokensTotal[$i]} > 0 ]];then
            printf "\nSobran: ${tokensTotal[$i]}\n"

            txout="${tokensTotal[$i]} ${tokens[$i]}"
            walletTxOutArrayForChangeOfTokens="$walletTxOutArrayForChangeOfTokens + $txout "
            swChangeTokens=1
        fi

    done

    printf "\nscriptTxOutMultiAssets:\n"
    echo $scriptTxOutMultiAssets

    printf "\nwalletTxOutArrayForChangeOfTokens:\n"
    echo $walletTxOutArrayForChangeOfTokens

    source "$FALCON_DEVS_SCRIPTS/main/main_datum_elegir_crear.sh"

    
    printf "\n\nRealizando Transferencia...\n\n"

    if [[ $swChangeTokens = 0 ]]; then
        $CARDANO_NODE/cardano-cli transaction build \
            --babbage-era \
            --$TESTNET_MAGIC \
            --change-address $walletAddr \
            $walletTxInArray \
            --tx-out "$scriptTxOutMultiAssets" \
            --tx-out-datum-hash-file $FALCON_DEVS_HASKELL_FILES/datums/$datumFile.json \
            --out-file $FALCON_DEVS_HASKELL_FILES/transacciones/${scriptName}.body 
    else

        $CARDANO_NODE/cardano-cli transaction build \
            --babbage-era \
            --$TESTNET_MAGIC \
            --change-address $walletAddr \
            $walletTxInArray \
            --tx-out "$scriptTxOutMultiAssets" \
            --tx-out-datum-hash-file $FALCON_DEVS_HASKELL_FILES/datums/$datumFile.json \
            --tx-out "$walletTxOutArrayForChangeOfTokens" \
            --out-file $FALCON_DEVS_HASKELL_FILES/transacciones/${scriptName}.body 
    fi

    if [ "$?" == "0" ]; then

        $CARDANO_NODE/cardano-cli transaction sign \
            --tx-body-file $FALCON_DEVS_HASKELL_FILES/transacciones/${scriptName}.body \
            --signing-key-file $FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.skey \
            --$TESTNET_MAGIC \
            --out-file $FALCON_DEVS_HASKELL_FILES/transacciones/${scriptName}.signed

        if [ "$?" == "0" ]; then

            $CARDANO_NODE/cardano-cli transaction submit \
                --$TESTNET_MAGIC \
                --tx-file $FALCON_DEVS_HASKELL_FILES/transacciones/${scriptName}.signed

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