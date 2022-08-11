#!/bin/bash




#scriptTxIn=$(cat $HASKELL_FILES/validators/${scriptName}.utxo)

#printf "\nUltima dirección utilizada: %s" $scriptTxIn

printf "\nUltimas direcciones utilizadas:\n"

if [[ -f "$HASKELL_FILES/wallets/${walletName}.utxo"  ]]
then

    if [[ -f "$HASKELL_FILES/validators/${scriptName}.utxo"  ]]
    then
        while IFS= read -r line
        do
            echo "$line"
        done < "$HASKELL_FILES/validators/${scriptName}.utxo"
    fi

fi

printf "\nDesea elegir otras (y/n)?\n"
read -n 1 -s opcion
if [[ $opcion = "y" ]]; then 

    swPrimero=1
    utxoElegidas=()

    scriptNroUTXO=
    until [[ $scriptNroUTXO > 0 ]]; do
        printf "\nUtxo At Script:\n"

        result=$($CARDANO_NODE/cardano-cli query utxo\
        --address $scriptAddr --$TESTNET_MAGIC)

        echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl

        printf "\nIngrese Número de Utxo o 0 para terminar): "

        read scriptNroUTXO

        if ! [[ $scriptNroUTXO = "" ||  $scriptNroUTXO = "0" ]]; then 
        
            #echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${scriptNroUTXO}p | grep -Po "[a-zA-Z0-9]+" 

            TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${scriptNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 1p)
            TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${scriptNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 2p)

            if [ "$?" == "0" ]; then

                if ! [[ $TxHash = "" ]]; then 

                    swEncontrado=0
                    for i in ${!utxoElegidas[@]}; do
                        #echo "OLD: " ${utxoElegidas[$i]}
                        if [[ ${utxoElegidas[$i]} = "$TxHash#$TxIx" ]]; then 
                            swEncontrado=1
                        fi
                    done

                    if [[ $swEncontrado = 0 ]]; then 
                        utxoElegidas+=("$TxHash#$TxIx")

                        echo "Agregando:" $TxHash#$TxIx

                        if [ $swPrimero = 1 ]; then
                            echo $TxHash#$TxIx>$HASKELL_FILES/validators/${scriptName}.utxo
                            swPrimero=0
                        else
                            echo $TxHash#$TxIx>>$HASKELL_FILES/validators/${scriptName}.utxo
                        fi
                    fi

                fi
            fi

        fi

    done

    #scriptTxIn=$(cat $HASKELL_FILES/validators/${scriptName}.utxo)

fi

echo ""

#read -r scriptTxIn  < $HASKELL_FILES/validators/${scriptName}.utxo

scriptTxInArray=""

echo "Tx Elegidas:" 

if [[ -f "$HASKELL_FILES/wallets/${walletName}.utxo"  ]]
then

    while IFS= read -r txin
    do
        echo "" 

        if ! [[ $txin = "#" ]]; then 

            echo "Txid#txindex: "$txin

            scriptTxInArray="$scriptTxInArray --tx-in $sep $txin "
            sep=" "

            result=$($CARDANO_NODE/cardano-cli query utxo\
                --tx-in $txin --$TESTNET_MAGIC)

            lovelace=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | grep -Po "[0-9]+ lovelace" | grep -Po "[0-9]+")

            lovelaceTotal=$((lovelace + lovelaceTotal))


            echo "lovelace: " $lovelace

            countToken=1

            tokenCantidad=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p | grep -Po "[0-9]+" | sed -n 1p)
            tokenPolicy=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 2p)
            tokenName=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 3p)
            
            until [[ $tokenName = ""  ]];
            do

                tokenID="$tokenPolicy.$tokenName"

                swEncontrado=0
                for i in ${!tokens[@]}; do
                    #echo $tId
                    if [[ ${tokens[$i]} = $tokenID ]]; then 
                        swEncontrado=1
                        tokensTotal[$i]=$((${tokensTotal[$i]}+$tokenCantidad))
                    fi
                done

                if [[ $swEncontrado = 0 ]]; then 
                    tokens+=($tokenID)
                    tokensTotal+=($tokenCantidad)
                fi

                echo "tokenPolicy: " $tokenPolicy
                echo "tokenName: " $tokenName
                echo "tokenCantidad: " $tokenCantidad

                countToken=$(($countToken+1))

                tokenCantidad=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p | grep -Po "[0-9]+" | sed -n 1p)
                tokenPolicy=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 2p)
                tokenName=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 3p)
                


            done


            # if ! [[ $tokenName = "" ]]; then 

            #     tokenID="$tokenPolicy.$tokenName"

            #     swEncontrado=0
            #     for i in ${!tokens[@]}; do
            #         #echo $tId
            #         if [[ ${tokens[$i]} = $tokenID ]]; then 
            #             swEncontrado=1
            #             tokensTotal[$i]=$((${tokensTotal[$i]}+$tokenCantidad))
            #         fi
            #     done

            #     if [[ $swEncontrado = 0 ]]; then 
            #         tokens+=($tokenID)
            #         tokensTotal+=($tokenCantidad)
            #     fi

            #     echo "tokenPolicy: " $tokenPolicy
            #     echo "tokenName: " $tokenName
            #     echo "tokenCantidad: " $tokenCantidad

            # fi
        fi

    done < "$HASKELL_FILES/validators/${scriptName}.utxo"

fi

scriptTxInArray="$scriptTxInArray"

echo "" 
echo "Totales:" 

echo "lovelaceTotal: " $lovelaceTotal


for i in ${!tokens[@]}; do
    echo "tokenid: " ${tokens[$i]}
    echo "token total: " ${tokensTotal[$i]}
done

echo "" 
