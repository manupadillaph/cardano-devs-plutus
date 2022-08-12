#!/bin/bash


addrFile="$FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.addr"
skeyFile="$FALCON_DEVS_HASKELL_FILES/wallets/${walletName}.skey"


# echo "walletTxIn: $walletTxIn"
# echo "amt: $amt"
# echo "tn: $tn"
# echo "address file: $addrFile"
# echo "signing key file: $skeyFile"
# echo "payment key hash: $walletSig"

token_name=""

scriptPolicyName=""
until [[ -f "$FALCON_DEVS_HASKELL_FILES/mintingpolicies/NFT-${scriptPolicyName}.plutus"  ]]
do

    printf "\nNombre de archivo de Minting Policy NFT: "

    scriptPolicyName=
    while [[ $scriptPolicyName = "" ]]; do
        read scriptPolicyName
    done

    if ! [[ -f "$FALCON_DEVS_HASKELL_FILES/mintingpolicies/NFT-${scriptPolicyName}.plutus" ]]
    then
        printf "\nMinting Policiy file NFT-${scriptPolicyName}.plutus no existe\n"
    fi

    printf "\nDesea crear files NFT.plutus de la policy en haskell (y/n)\n"
    read -n 1 -s opcion
    if [[ $opcion = "y" ]]; then 

        printf "\nNombre del Token: "
        token_name=
        while [[ $token_name = "" ]]; do
            read token_name
        done

        #Para poder ejecutar el cabal exec necesito estar en la carpeta $FALCON_DEVS_HASKELL donde hice el cabal build
        CWD=$(pwd)
        cd $FALCON_DEVS_HASKELL

        echo "NFT en base de: $walletTxIn con el Token Name: $token_name"
        printf "%s\n%s\n%s\n" "16" "$FALCON_DEVS_HASKELL_FILES/mintingpolicies" "NFT-${scriptPolicyName}" "$walletTxIn" "$token_name" | cabal exec deploy-smart-contracts-auto

        cd $CWD
    fi

done

policyFile="$FALCON_DEVS_HASKELL_FILES/mintingpolicies/NFT-${scriptPolicyName}.plutus"

printf "\nDesea Mint NFT token ahora (y/n)?\n"
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

    ppFile=$FALCON_DEVS_HASKELL_FILES/config/tx/protocol.json
    $CARDANO_NODE/cardano-cli query protocol-parameters \
                    --out-file $ppFile --$TESTNET_MAGIC 


    unsignedFile=$FALCON_DEVS_HASKELL_FILES/transacciones/NFT.unsigned
    signedFile=$FALCON_DEVS_HASKELL_FILES/transacciones/NFT.signed

    pid=$($CARDANO_NODE/cardano-cli transaction policyid --script-file $policyFile)

    #Para poder ejecutar el cabal exec necesito estar en la carpeta $FALCON_DEVS_HASKELL donde hice el cabal build
    CWD=$(pwd)
    cd $FALCON_DEVS_HASKELL

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
            --$TESTNET_MAGIC \
            $walletTxInArray \
            --tx-in-collateral $walletTxIn \
            --tx-out "$addr + $minimoADA lovelace + $v" \
            --tx-out "$walletTxOutArrayForChangeOfTokens" \
            --mint "$v" \
            --mint-script-file $policyFile \
            --mint-redeemer-file $FALCON_DEVS_HASKELL_FILES/redeemers/unit.json \
            --change-address $addr \
            --required-signer-hash $walletSig \
            --required-signer=$skeyFile  \
            --protocol-params-file $ppFile \
            --out-file $unsignedFile 

    else
        $CARDANO_NODE/cardano-cli transaction build \
            --babbage-era \
            --$TESTNET_MAGIC \
            $walletTxInArray \
            --tx-in-collateral $walletTxIn \
            --tx-out "$addr + $minimoADA lovelace + $v" \
            --mint "$v" \
            --mint-script-file $policyFile \
            --mint-redeemer-file $FALCON_DEVS_HASKELL_FILES/redeemers/unit.json \
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
            --$TESTNET_MAGIC \
            --out-file $signedFile

        if [ "$?" == "0" ]; then      

            $CARDANO_NODE/cardano-cli transaction submit \
                --$TESTNET_MAGIC \
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