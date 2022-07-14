#!/bin/bash

printf "\nValidador: "

printf "\n1: Locker"
printf "\n2: AlwaysTrue"
printf "\n3: AlwaysFalse"
printf "\n4: Beneficiary"
printf "\n5: Deadline"
printf "\n6: Redeemer"
# printf "\n7: Stake Simple"
printf "\n"

scriptNumero=
while [[ $scriptNumero = "" || $scriptNumero <1 || $scriptNumero > 6 ]]; do
    read scriptNumero
done

if [[ $scriptNumero = "1" ]]; then
    scriptName="Locker"
    scriptNumeroOpcionExportCBOR=3
    scriptNumeroOpcionExportHash=4
fi
if [[ $scriptNumero = "2" ]]; then
    scriptName="AlwaysTrue"
    scriptNumeroOpcionExportCBOR=5
    scriptNumeroOpcionExportHash=6
fi
if [[ $scriptNumero = "3" ]]; then
    scriptName="AlwaysFalse"
    scriptNumeroOpcionExportCBOR=7
    scriptNumeroOpcionExportHash=8
fi
if [[ $scriptNumero = "4" ]]; then
    scriptName="Beneficiary"
    scriptNumeroOpcionExportCBOR=9
    scriptNumeroOpcionExportHash=10
fi
if [[ $scriptNumero = "5" ]]; then
    scriptName="Deadline"
    scriptNumeroOpcionExportCBOR=11
    scriptNumeroOpcionExportHash=12
fi
if [[ $scriptNumero = "6" ]]; then
    scriptName="Redeemer"
    scriptNumeroOpcionExportCBOR=13
    scriptNumeroOpcionExportHash=14
fi
if [[ $scriptNumero = "7" ]]; then
    scriptName="Stake Simple"
    # scriptNumeroOpcionExportCBOR=15
    # scriptNumeroOpcionExportHash=16
fi


until [[ -f "$SCRIPTS_FILES/validators/${scriptName}.plutus" && -f "$SCRIPTS_FILES/validators/${scriptName}.hash"   && -f "$SCRIPTS_FILES/validators/${scriptName}.addr" ]]
do

    if ! [[ -f "$SCRIPTS_FILES/validators/${scriptName}.plutus" && -f "$SCRIPTS_FILES/validators/${scriptName}.hash" ]]
    then
        printf "\nValidator script file ${scriptName} no existe\n"
    else
        $CARDANO_NODE/cardano-cli address build  \
        --payment-script-file $SCRIPTS_FILES/validators/${scriptName}.plutus --out-file $SCRIPTS_FILES/validators/${scriptName}.addr --testnet-magic $TESTNET_MAGIC
    fi

    printf "\nDesea crear files .plutus, .hash del validator en haskell (y/n)\nImportante: Necesita tener NODO configurado e iniciado\n"
    read -n 1 -s opcion
    if [[ $opcion = "y" ]]; then
     
        #Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
        CWD=$(pwd)
        cd $HASKELL

        printf "%s\n%s\n%s\n" "$scriptNumeroOpcionExportCBOR" "$SCRIPTS_FILES/validators" "$scriptName" | cabal exec deploy-smart-contracts-auto 
        printf "%s\n%s\n%s\n" "$scriptNumeroOpcionExportHash" "$SCRIPTS_FILES/validators" "$scriptName" | cabal exec deploy-smart-contracts-auto 
        
        cd $CWD

        $CARDANO_NODE/cardano-cli address build  \
        --payment-script-file $SCRIPTS_FILES/validators/${scriptName}.plutus --out-file $SCRIPTS_FILES/validators/${scriptName}.addr --testnet-magic $TESTNET_MAGIC

    fi

done

scriptAddr=$(cat $SCRIPTS_FILES/validators/${scriptName}.addr)

echo "Script Address:" $scriptAddr