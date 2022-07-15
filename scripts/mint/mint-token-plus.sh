#!/bin/bash


addrFile="$HASKELL_FILES/wallets/${walletName}.addr"
skeyFile="$HASKELL_FILES/wallets/${walletName}.skey"


# echo "walletTxIn: $walletTxIn"
# echo "amt: $amt"
# echo "tn: $tn"
# echo "address file: $addrFile"
# echo "signing key file: $skeyFile"
# echo "payment key hash: $walletSig"

token_name=""

scriptPolicyName=""
until [[ -f "$HASKELL_FILES/mintingpolicies/Plus-${scriptPolicyName}.plutus"  ]]
do

    printf "\nNombre de archivo de Minting Policy Plus: "

    scriptPolicyName=
    while [[ $scriptPolicyName = "" ]]; do
        read scriptPolicyName
    done

    if ! [[ -f "$HASKELL_FILES/mintingpolicies/Plus-${scriptPolicyName}.plutus" ]]
    then
        printf "\nMinting Policiy file Plus-${scriptPolicyName}.plutus no existe\n"
    fi

    printf "\nDesea crear files Plus.plutus de la policy en haskell (y/n)\n"
    read -n 1 -s opcion
    if [[ $opcion = "y" ]]; then 

        printf "\nNombre del Token: "
        token_name=
        while [[ $token_name = "" ]]; do
            read token_name
        done

        printf "\nCantidad máxima desea acuñar: "
        token_cantidad=
        while [[ $token_cantidad = "" ]]; do
            read token_cantidad
        done

        echo "Plus en base de: $walletTxIn con el Token Name: $token_name y cantidad máxima: $token_cantidad"
        
        #Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
        CWD=$(pwd)
        cd $HASKELL

        printf "%s\n%s\n%s\n" "17" "$HASKELL_FILES/mintingpolicies" "Plus-${scriptPolicyName}" "$walletTxIn" "$token_name" "$token_cantidad" | cabal exec deploy-smart-contracts-auto

        cd $CWD
        
    fi

done

policyFile="$HASKELL_FILES/mintingpolicies/Plus-${scriptPolicyName}.plutus"

printf "\nDesea Mint Plus token ahora (y/n)?\n"
read -n 1 -s opcion
if [[ $opcion = "y" ]]; then 

    if  [[ $token_name = "" ]];
    then
        printf "\nNombre del Token: "
        token_name=
        while [[ $token_name = "" ]]; do
            read token_name
        done
    fi
    
    printf "\nCantidad que desea acuñar: "
    token_cantidad=
    while [[ $token_cantidad = "" ]]; do
        read token_cantidad
    done

    ppFile=$HASKELL_FILES/config/tx/protocol.json
    $CARDANO_NODE/cardano-cli query protocol-parameters \
                    --out-file $ppFile --testnet-magic $TESTNET_MAGIC 


    unsignedFile=$HASKELL_FILES/transacciones/Plus.unsigned
    signedFile=$HASKELL_FILES/transacciones/Plus.signed

    pid=$(cardano-cli transaction policyid --script-file $policyFile)

    #Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
    CWD=$(pwd)
    cd $HASKELL

    tnHex=$(cabal exec utils-token-name -- $token_name)

    cd $CWD
    
    addr=$(cat $addrFile)

    v="$token_cantidad $pid.$tnHex"

    echo "currency symbol: $pid"

    echo "token name (hex): $tnHex"

    echo "minted value: $v"

    # echo "address: $addr"

    printf "\n\nRealizando transferencia...\n\n"

     if [[ $swChangeTokens = 1 ]]; then

        $CARDANO_NODE/cardano-cli transaction build \
            --babbage-era \
            --testnet-magic $TESTNET_MAGIC \
            $walletTxInArray \
            --tx-in-collateral $walletTxIn \
            --tx-out "$addr + $minimoADA lovelace + $v" \
            --tx-out "$walletTxOutArrayForChangeOfTokens" \
            --mint "$v" \
            --mint-script-file $policyFile \
            --mint-redeemer-file $HASKELL_FILES/redeemers/unit.json \
            --change-address $addr \
            --required-signer-hash $walletSig \
            --required-signer=$skeyFile  \
            --protocol-params-file $ppFile \
            --out-file $unsignedFile 

    else
        $CARDANO_NODE/cardano-cli transaction build \
            --babbage-era \
            --testnet-magic $TESTNET_MAGIC \
            $walletTxInArray \
            --tx-in-collateral $walletTxIn \
            --tx-out "$addr + $minimoADA lovelace + $v" \
            --mint "$v" \
            --mint-script-file $policyFile \
            --mint-redeemer-file $HASKELL_FILES/redeemers/unit.json \
            --change-address $addr \
            --required-signer-hash $walletSig \
            --required-signer=$skeyFile  \
            --protocol-params-file $ppFile \
            --out-file $unsignedFile 
    fi


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