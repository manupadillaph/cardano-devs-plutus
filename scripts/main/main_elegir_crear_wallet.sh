#!/bin/bash

walletLoadedInServerWallet=0

walletName=""
until [[ -f "$HASKELL_FILES/wallets/${walletName}.pkh" && -f "$HASKELL_FILES/wallets/${walletName}.skey"  && -f "$HASKELL_FILES/wallets/${walletName}.vkey"  ]]
do
    printf "\nNombre Wallet (01): "
    read walletName

    if [[ $walletName = "" ]]; then 
        walletName="01"
    fi

    if ! [[ -f "$HASKELL_FILES/wallets/${walletName}.pkh" && -f "$HASKELL_FILES/wallets/${walletName}.skey"  && -f "$HASKELL_FILES/wallets/${walletName}.vkey"  ]]
    then
        printf "\nWallet pkh, skey, vkey files no existen\n"

        if [[ -f "$HASKELL_FILES/wallets/${walletName}.json"   ]]
        then
            printf "\nSe encontró ${walletName} JSON file, desea crearla desde allí (y/n)? \nImportante: Necesita tener NODO y WALLET SERVER configurados e iniciados \n"
            read -n 1 -s opcion
            if [[ $opcion = "y" ]]; then 

                bash $SCRIPTS/tools/wallet-create-con-cardano-wallet-desde-json.sh ${walletName}

                walletLoadedInServerWallet=1 

            fi
        fi

        if [[ $walletLoadedInServerWallet = 0 ]]; then 

            printf "\nDesea crear una nueva Wallet (y/n)? \nImportante: Necesita tener NODO y WALLET SERVER configurados e iniciados \n"
            read -n 1 -s opcion
            if [[ $opcion = "y" ]]; then 

                bash $SCRIPTS/tools/wallet-create-con-cardano-wallet.sh ${walletName} ${walletName}

                walletLoadedInServerWallet=1

            fi
        fi
    fi
done

if [[ $walletLoadedInServerWallet = 0 ]]; then

    if ! [[ -f "$HASKELL_FILES/wallets/${walletName}.json" && -f "$HASKELL_FILES/wallets/${walletName}.id"  ]]
    then
        printf "\nWallet ${walletName} JSON o id files no existen. No se puede cargar en la Wallet en el Wallet Server, algunas funciones estarán limitadas (como los servicios de PAB)\n"
    fi

    if [[ -f "$HASKELL_FILES/wallets/${walletName}.json"  ]]
    then
        bash $SCRIPTS/tools/wallet-load-con-cardano-wallet-desde-json.sh ${walletName}

        if ! [[ -f "$HASKELL_FILES/wallets/${walletName}.addrs" ]];
        then

            if  [[ -f "$HASKELL_FILES/wallets/${walletName}.id"  ]];
            then

                WALLET_ID=$(cat $HASKELL_FILES/wallets/$walletName.id)

                DIRECCIONES=$(curl -H "content-type: application/json" \
                    -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq -r '.[]' )

                echo $DIRECCIONES | jq -r '.id' >$HASKELL_FILES/wallets/${walletName}.addrs

            fi
            
        fi
    fi

    if ! [[ -f "$HASKELL_FILES/wallets/${walletName}.addr" ]]
    then
        $CARDANO_NODE/cardano-cli address build \
	        --payment-verification-key-file $HASKELL_FILES/wallets/${walletName}.vkey --out-file $HASKELL_FILES/wallets/${walletName}.addr --testnet-magic $TESTNET_MAGIC 
    fi
fi


walletAddr=$(cat $HASKELL_FILES/wallets/${walletName}.addr)

echo "Wallet Address:" $walletAddr

walletSig=$(cat $HASKELL_FILES/wallets/${walletName}.pkh)

echo "Payment Key HASH:" $walletSig

if [[ -f "$HASKELL_FILES/wallets/${walletName}.json"  ]]
then
    walletPassphrase=$(cat $HASKELL_FILES/wallets/${walletName}.json | jq -r '.passphrase')
    echo "Passphrase:" $walletPassphrase
fi

