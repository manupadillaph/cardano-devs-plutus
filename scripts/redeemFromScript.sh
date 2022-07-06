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
echo "Elija cual desea usar para collateral: "
read collateralIx

walletTxIn=$(cat "files/wallets/${walletName}.utxo" | sed -n ${collateralIx}p)

echo "walletTxIn: "$walletTxIn
echo "walletTxInArray: "$walletTxInArray

# walletTxIn=$(cat files/wallets/${walletName}.utxo)

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

#     echo $TxHash#$TxIx>files/wallets/${walletName}.utxo

# fi

# walletTxIn=$(cat files/wallets/${walletName}.utxo)

echo " "
echo "Elija tx para redeem: "

source "./getutxoScript.sh"


read -r scriptTxIn  < files/validators/${scriptName}.utxo
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

    printf "\nwalletTxOutArrayForChangeOfTokens:\n"
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

# echo $TxHash#$TxIx>files/validators/${scriptName}.utxo

# scriptTxIn=$(cat files/validators/${scriptName}.utxo)



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

        printf "%s\n%s\n%s\n" "1" "files/datums" "$datumFile" "$datumCreator" "$datumDeadline" "$datumName" "$datumQty" | cabal exec deploy-smart-contracts-auto-exe  

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
        printf "%s\n%s\n%s\n" "2" "files/redeemers" "$redeemerFile" "$redeemerOpcion" | cabal exec deploy-smart-contracts-auto-exe 


    fi

done

printf "\nRealizando transferencia...\n\n"


if [[ $swChangeTokens = 1 ]]; then
    $CARDANO_NODE/cardano-cli transaction build \
        --babbage-era \
        --testnet-magic $TESTNET_MAGIC \
        --change-address $walletAddr \
        $walletTxInArray \
        --tx-in $scriptTxIn \
        --tx-in-script-file files/validators/$scriptName.plutus \
        --tx-in-datum-file  files/datums/$datumFile.json \
        --tx-in-redeemer-file files/redeemers/$redeemerFile.json  \
        --tx-in-collateral $walletTxIn \
        --tx-out "$walletTxOutArrayForChangeOfTokens" \
        --required-signer-hash $walletSig \
        --required-signer=files/wallets/${walletName}.skey \
        --protocol-params-file files/config/protocol.json \
        --invalid-before ${tipSlot} \
        --out-file files/transacciones/${scriptName}.body 
else
    $CARDANO_NODE/cardano-cli transaction build \
        --babbage-era \
        --testnet-magic $TESTNET_MAGIC \
        --change-address $walletAddr \
        $walletTxInArray \
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
fi

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