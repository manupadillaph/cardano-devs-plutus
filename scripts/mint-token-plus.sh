#!/bin/bash

oref=$1
tn=$2
amt=$3
addrFile=$4
skeyFile=$5
pkh=$6

echo "oref: $oref"
echo "amt: $amt"
echo "tn: $tn"
echo "address file: $addrFile"
echo "signing key file: $skeyFile"
echo "payment key hash: $pkh"

scriptName=""
until [[ -f "files/mintingpolicies/Plus-${scriptName}.plutus"  ]]
do

    printf "\nNombre de archivo de Minting Policy Plus: "

    scriptName=
    while [[ $scriptName = "" ]]; do
        read scriptName
    done

    if ! [[ -f "files/mintingpolicies/Plus-${scriptName}.plutus" ]]
    then
        printf "\nMinting Policiy file Plus-${scriptName}.plutus no existe\n"
    fi

    printf "\nDesea crear files .plutus de la policy en haskell (y/n)\n"
    read -n 1 -s opcion
    if [[ $opcion = "y" ]]; then 
    
        printf "%s\n%s\n%s\n" "17" "files/mintingpolicies" "Plus-${scriptName}" "$oref" "$tn" "$amt" | cabal exec deploy-smart-contracts-auto-exe

    fi

done

policyFile="files/mintingpolicies/Plus-${scriptName}.plutus"


ppFile=files/config/protocol.json
$CARDANO_NODE/cardano-cli query protocol-parameters \
                --out-file $ppFile --testnet-magic $TESTNET_MAGIC 


unsignedFile=files/transacciones/Plus.unsigned
signedFile=files/transacciones/Plus.signed

pid=$(cardano-cli transaction policyid --script-file $policyFile)

tnHex=$(cabal exec token-name -- $tn)

addr=$(cat $addrFile)

v="$amt $pid.$tnHex"

echo "currency symbol: $pid"

echo "token name (hex): $tnHex"

echo "minted value: $v"

echo "address: $addr"

printf "\nRealizando transferencia...\n\n"

$CARDANO_NODE/cardano-cli transaction build \
    --testnet-magic $TESTNET_MAGIC \
    --tx-in $oref \
    --tx-in-collateral $oref \
    --tx-out "$addr + 1500000 lovelace + $v" \
    --mint "$v" \
    --mint-script-file $policyFile \
    --mint-redeemer-file files/redeemers/unit.json \
    --change-address $addr \
    --protocol-params-file $ppFile \
    --out-file $unsignedFile 

if [ "$?" == "0" ]; then   

    $CARDANO_NODE/cardano-cli transaction sign \
        --tx-body-file $unsignedFile \
        --signing-key-file $skeyFile \
        --testnet-magic $TESTNET_MAGIC \
        --out-file $signedFile

    if [ "$?" == "0" ]; then      

        $CARDANO_NODE/cardano-cli transaction submit \
            --testnet-magic $TESTNET_MAGIC \
            --tx-file $signedFile

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