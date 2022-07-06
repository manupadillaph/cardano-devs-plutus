scriptName=""
until [[ -f "files/validators/${scriptName}.plutus" && -f "files/validators/${scriptName}.hash"   && -f "files/validators/${scriptName}.addr" ]]
do

    printf "\nNombre del Script: "

    scriptName=
    while [[ $scriptName = "" ]]; do
        read scriptName
    done

    if ! [[ -f "files/validators/${scriptName}.plutus" && -f "files/validators/${scriptName}.hash" ]]
    then
        printf "\nValidator script file ${scriptName} no existe\n"
    else
        $CARDANO_NODE/cardano-cli address build  \
        --payment-script-file files/validators/${scriptName}.plutus --out-file files/validators/${scriptName}.addr --testnet-magic $TESTNET_MAGIC
    fi

    printf "\nDesea crear files .plutus, .hash del actual validator en haskell (y/n)\nImportante: Necesita tener NODO configurado e iniciado\n"
    read -n 1 -s opcion
    if [[ $opcion = "y" ]]; then 
        printf "%s\n%s\n%s\n" "3" "files/validators" "$scriptName" | cabal exec deploy-smart-contracts-auto-exe  
        printf "%s\n%s\n%s\n" "4" "files/validators" "$scriptName" | cabal exec deploy-smart-contracts-auto-exe  
        
        $CARDANO_NODE/cardano-cli address build  \
        --payment-script-file files/validators/${scriptName}.plutus --out-file files/validators/${scriptName}.addr --testnet-magic $TESTNET_MAGIC

    fi

done

scriptAddr=$(cat files/validators/${scriptName}.addr)

echo "Script Address:" $scriptAddr