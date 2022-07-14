#!/bin/bash

printf "\nDatum: \n"

datumFile=""

until [[ -f "$SCRIPTS_FILES/datums/$datumFile.json"  ]]
do

    printf "\nIngrese nombre del archivo Datum (DEF): "
    read datumFile
    if [[ $datumFile = "" ]]; then 
        datumFile="DEF"
    fi
    
    if ! [[ -f "$SCRIPTS_FILES/datums/$datumFile.json"  ]]
    then
        printf "\nDatum file ${datumFile}.json no existe\n"
    fi

    printf "\nDesea crear datum file (y/n):\n"
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

        #Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
        CWD=$(pwd)
        cd $HASKELL

        printf "%s\n%s\n%s\n" "1" "$SCRIPTS_FILES/datums" "$datumFile" "$datumCreator" "$datumDeadline" "$datumName" "$datumQty" | cabal exec deploy-smart-contracts-auto 
        
        cd $CWD

    fi

    
done

   
printf "\nDatum JSON: "
cat $SCRIPTS_FILES/datums/$datumFile.json | jq '.'

datumHash=$($CARDANO_NODE/cardano-cli transaction hash-script-data --script-data-file "$SCRIPTS_FILES/datums/$datumFile.json")

printf "\nDatum Hash: "
echo $datumHash

