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
until [[ -f "$FALCON_DEVS_HASKELL_FILES/mintingpolicies/Plus-${scriptPolicyName}.plutus"  ]]
do

    printf "\nNombre de archivo de Minting Policy Plus: "

    scriptPolicyName=
    while [[ $scriptPolicyName = "" ]]; do
        read scriptPolicyName
    done

    if ! [[ -f "$FALCON_DEVS_HASKELL_FILES/mintingpolicies/Plus-${scriptPolicyName}.plutus" ]]
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
        
        #Para poder ejecutar el cabal exec necesito estar en la carpeta $FALCON_DEVS_HASKELL donde hice el cabal build
        CWD=$(pwd)
        cd $FALCON_DEVS_HASKELL

        printf "%s\n%s\n%s\n" "17" "$FALCON_DEVS_HASKELL_FILES/mintingpolicies" "Plus-${scriptPolicyName}" "$walletTxIn" "$token_name" "$token_cantidad" | cabal exec deploy-smart-contracts-auto

        cd $CWD
        
    fi

done

policyFile="$FALCON_DEVS_HASKELL_FILES/mintingpolicies/Plus-${scriptPolicyName}.plutus"

printf "\nDesea Crear Metadatos ahora (y/n)?\n"
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
    

    tnHex=$(echo -n $token_name | basenc --base16 | awk '{print tolower($0)}')
    
    echo "Asset name '$token_name' encoded as base16: '$tnHex'"
    

    printf "\nDescripcion: "
    token_desc=
    while [[ $token_desc = "" ]]; do
        read token_desc
    done
    

    printf "\nTiker: "
    token_tiker=
    while [[ $token_tiker = "" ]]; do
        read token_tiker
    done

    printf "\nURL: "
    token_url=
    while [[ $token_url = "" ]]; do
        read token_url
    done

    printf "\nLogo: "
    token_logo=
    while [[ $token_logo = "" ]]; do
        read token_logo
    done

    printf "\nDecimals: "
    token_decimals=
    while [[ $token_decimals = "" ]]; do
        read token_decimals
    done


    # Esto no hace falta, con la otra forma arriba calcula mas rapido...
    # #Para poder ejecutar el cabal exec necesito estar en la carpeta $FALCON_DEVS_HASKELL donde hice el cabal build
    # CWD=$(pwd)
    # cd $FALCON_DEVS_HASKELL

    # tnHex=$(cabal exec utils-token-name -- $token_name)

    # cd $CWD

    pid=$($CARDANO_NODE/cardano-cli transaction policyid --script-file $policyFile)

    SUBJECT="$pid$tnHex"

    echo "Subject is '$SUBJECT'"
    
    CWD=$(pwd)
    cd $FALCON_DEVS_HASKELL_FILES/token-metadata

    $CARDANO_TOOLS_TOKEN_METADATA_CREATOR entry --init $SUBJECT

    $CARDANO_TOOLS_TOKEN_METADATA_CREATOR entry $SUBJECT \
        --name "$token_name" \
        --description "$token_desc" \

    $CARDANO_TOOLS_TOKEN_METADATA_CREATOR entry $SUBJECT \
        --ticker "$token_tiker" \
        --url "$token_url" \
        --logo "$token_logo" \
        --decimals $token_decimals

    $CARDANO_TOOLS_TOKEN_METADATA_CREATOR entry $SUBJECT -a $skeyFile

    $CARDANO_TOOLS_TOKEN_METADATA_CREATOR entry $SUBJECT --finalize

    cd $CWD
    
    exit 


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