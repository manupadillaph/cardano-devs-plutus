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

printf "\nRealizando Transferencia...\n\n"

if [[ $swChangeTokens = 0 ]]; then
    $CARDANO_NODE/cardano-cli transaction build \
        --babbage-era \
        --testnet-magic $TESTNET_MAGIC \
        --change-address $walletAddr \
        $walletTxInArray \
        --tx-out "$scriptTxOutMultiAssets" \
        --tx-out-datum-hash-file files/datums/$datumFile.json \
        --out-file files/transacciones/${scriptName}.body 
else

    $CARDANO_NODE/cardano-cli transaction build \
        --babbage-era \
        --testnet-magic $TESTNET_MAGIC \
        --change-address $walletAddr \
        $walletTxInArray \
        --tx-out "$scriptTxOutMultiAssets" \
        --tx-out-datum-hash-file files/datums/$datumFile.json \
        --tx-out "$walletTxOutArrayForChangeOfTokens" \
        --out-file files/transacciones/${scriptName}.body 
fi

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
