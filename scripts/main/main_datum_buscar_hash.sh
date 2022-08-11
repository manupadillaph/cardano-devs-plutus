#!/bin/bash

printf "\nIngrese Hash a Buscar: "
datumHashBuscar=
while [[ $datumHashBuscar = "" ]]; do
    read datumHashBuscar
done

FILE_encontrada=""

for FILE in $FALCON_DEVS_HASKELL_FILES/datums/*; 
do 

    datumHash=$($CARDANO_NODE/cardano-cli transaction hash-script-data --script-data-file "$FILE")

    echo $FILE ": " $datumHash ; 

    if [[ "$datumHashBuscar" = "$datumHash" ]]; then 
            printf "Econtrado HASH\n"
        FILE_encontrada=$FILE
    fi

done 

echo ""

if ! [[ $FILE_encontrada = "" ]]; then 

    printf "\nEcontrado HASH en: %s \n" "$FILE_encontrada" ; 

    cat $FILE_encontrada | jq '.'

fi

echo; read -rsn1 -p "Press any key to continue . . ."; echo
   