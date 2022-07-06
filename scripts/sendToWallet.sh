printf "\nWallet Address a donde enviar: "
sentToAddr=
while [[ $sentToAddr = "" ]]; do
    read sentToAddr
done

echo " "
echo "Elija tx in para enviar: "

tokens=()
tokensTotal=()
lovelaceTotal=0

source "./getutxo.sh"

echo "walletTxIn: "$walletTxIn
echo "walletTxInArray: "$walletTxInArray

printf "\nCantidad ADA (Max: $lovelaceTotal sin tener en cuenta Fee, ni $minimoADA ADA para txo de change si sobran Tokens): "
read cantidad
if [[ $cantidad = "" ]]; then 
    cantidad="$lovelaceTotal"
fi


walletTxOutMultiAssets="$sentToAddr $cantidad lovelace"

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
    walletTxOutMultiAssets="$walletTxOutMultiAssets + $txout "

    if [[ ${tokensTotal[$i]} > 0 ]];then
        printf "\nSobran: ${tokensTotal[$i]}\n"

        txout="${tokensTotal[$i]} ${tokens[$i]}"
        walletTxOutArrayForChangeOfTokens="$walletTxOutArrayForChangeOfTokens + $txout "
        swChangeTokens=1
    fi

done

printf "\nwalletTxOutMultiAssets:\n"
echo $walletTxOutMultiAssets

printf "\nwalletTxOutArrayForChangeOfTokens:\n"
echo $walletTxOutArrayForChangeOfTokens


printf "\nRealizando Transferencia...\n\n"

if [[ $swChangeTokens = 0 ]]; then
    $CARDANO_NODE/cardano-cli transaction build \
        --babbage-era \
        --testnet-magic $TESTNET_MAGIC \
        --change-address $walletAddr \
        $walletTxInArray \
        --tx-out "$walletTxOutMultiAssets" \
        --out-file files/transacciones/sendTo.body 
else

    $CARDANO_NODE/cardano-cli transaction build \
        --babbage-era \
        --testnet-magic $TESTNET_MAGIC \
        --change-address $walletAddr \
        $walletTxInArray \
        --tx-out "$walletTxOutMultiAssets" \
        --tx-out "$walletTxOutArrayForChangeOfTokens" \
        --out-file files/transacciones/sendTo.body 
fi

if [ "$?" == "0" ]; then

    $CARDANO_NODE/cardano-cli transaction sign \
        --tx-body-file files/transacciones/sendTo.body \
        --signing-key-file files/wallets/${walletName}.skey \
        --testnet-magic $TESTNET_MAGIC \
        --out-file files/transacciones/sendTo.signed

    if [ "$?" == "0" ]; then

        $CARDANO_NODE/cardano-cli transaction submit \
            --testnet-magic $TESTNET_MAGIC \
            --tx-file files/transacciones/sendTo.signed

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
