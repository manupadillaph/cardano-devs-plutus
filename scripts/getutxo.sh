#!/bin/bash


#walletTxIn=$(cat files/wallets/${walletName}.utxo)

#printf "\nUltima dirección utilizada: %s" $walletTxIn

printf "\nUltimas direcciones utilizadas:\n"

while IFS= read -r line
do
    echo "$line"
done < "files/wallets/${walletName}.utxo"

printf "\nDesea elegir otras (y/n)?\n"
read -n 1 -s opcion
if [[ $opcion = "y" ]]; then 

    swPrimero=1

    walletNroUTXO=
    until [[ "$walletNroUTXO" = "0" ]]; do
        printf "\nUtxo At Wallet:\n"

        result=$($CARDANO_NODE/cardano-cli query utxo\
        --address $walletAddr --testnet-magic $TESTNET_MAGIC)

        echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | nl

        printf "\nElija utxo para pagar script (presione ENTER para recargar utxo, numero para agregar y 0 para terminar): "

        read walletNroUTXO

        if ! [[ $walletNroUTXO = "" ||  $walletNroUTXO = "0" ]]; then 
        
            #echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" 

            TxHash=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 1p)
            TxIx=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${walletNroUTXO}p | grep -Po "[a-zA-Z0-9]+" | sed -n 2p)

            if [ "$?" == "0" ]; then

                if ! [[ $TxHash = "" ]]; then 

                    echo "Agregando:" $TxHash#$TxIx

                    if [ $swPrimero = 1 ]; then
                        echo $TxHash#$TxIx>files/wallets/${walletName}.utxo
                        swPrimero=0
                    else
                        echo $TxHash#$TxIx>>files/wallets/${walletName}.utxo
                    fi

                fi
            fi

        fi


    done

    #walletTxIn=$(cat files/wallets/${walletName}.utxo)

fi

echo ""

#read -r walletTxIn  < files/wallets/${walletName}.utxo

walletTxInArray=""





echo "Tx Elegidas:" 

while IFS= read -r txin
do
    echo "" 

    if ! [[ $txin = "#" ]]; then 

        echo "Txid#txindex: "$txin

        walletTxInArray="$walletTxInArray --tx-in $sep $txin "
        sep=" "

        result=$($CARDANO_NODE/cardano-cli query utxo\
            --tx-in $txin --testnet-magic $TESTNET_MAGIC)

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

done < "files/wallets/${walletName}.utxo"

walletTxInArray="$walletTxInArray"

echo "" 
echo "Totales:" 

echo "lovelaceTotal: " $lovelaceTotal


for i in ${!tokens[@]}; do
    echo "tokenid: " ${tokens[$i]}
    echo "token total: " ${tokensTotal[$i]}
done

echo "" 
