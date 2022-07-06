walletName=""
until [[ -f "files/wallets/${walletName}.addr" && -f "files/wallets/${walletName}.pkh" && -f "files/wallets/${walletName}.skey"  && -f "files/wallets/${walletName}.vkey"  ]]
do
    printf "\nNombre Wallet (01): "
    read walletName

    if [[ $walletName = "" ]]; then 
        walletName="01"
    fi

    if ! [[ -f "files/wallets/${walletName}.addr" && -f "files/wallets/${walletName}.pkh" && -f "files/wallets/${walletName}.skey"  && -f "files/wallets/${walletName}.vkey"  ]]
    then
        printf "\nWallet file ${walletName} no existe, desea crearla (y/n)? \nImportante: Necesita tener NODO y WALLET SERVER configurados e iniciados \n"

        read -n 1 -s opcion
        if [[ $opcion = "y" ]]; then 

            bash ${SCRIPTS}/create-wallet-con-cardano-wallet.sh ${walletName} ${walletName}

            cp $CARDANO_WALLET/wallets/${walletName}.json files/wallets/${walletName}.json
            cp $CARDANO_WALLET/wallets/${walletName}.addr files/wallets/${walletName}.addr
            cp $CARDANO_WALLET/wallets/${walletName}.id files/wallets/${walletName}.id
            cp $CARDANO_WALLET/wallets/${walletName}.pkh files/wallets/${walletName}.pkh
            cp $CARDANO_WALLET/wallets/${walletName}.skey files/wallets/${walletName}.skey
            cp $CARDANO_WALLET/wallets/${walletName}.vkey files/wallets/${walletName}.vkey
        fi
    fi
done

walletAddr=$(cat files/wallets/${walletName}.addr)

echo "Wallet Address:" $walletAddr

walletSig=$(cat files/wallets/${walletName}.pkh)

echo "Payment Key HASH:" $walletSig