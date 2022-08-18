#!/bin/bash


tokens=()
tokensTotal=()
lovelaceTotal=0

countTxin=1

result=$($CARDANO_NODE/cardano-cli query utxo\
        --address $scriptAddr --$TESTNET_MAGIC)

# echo $result

echo ""

txtin=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${countTxin}p)

# echo "txtin " $txtin

until [[ $txtin = ""  ]];
do

    lovelace=$(echo "$txtin" | grep -Po "[0-9]+ lovelace" | grep -Po "[0-9]+")

    lovelaceTotal=$((lovelace + lovelaceTotal))

    # echo "lovelace: " $lovelace

    countToken=1

    tokenCantidad=$(echo "$txtin" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p | grep -Po "[0-9]+" | sed -n 1p)
    tokenPolicy=$(echo "$txtin" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 2p)
    tokenName=$(echo "$txtin" |  grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 3p)
    
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

        # echo "tokenPolicy: " $tokenPolicy
        # echo "tokenName: " $tokenName
        # echo "tokenCantidad: " $tokenCantidad

        countToken=$(($countToken+1))

        tokenCantidad=$(echo "$txtin" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p | grep -Po "[0-9]+" | sed -n 1p)
        tokenPolicy=$(echo "$txtin" | grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 2p)
        tokenName=$(echo "$txtin" |  grep -Po "[0-9]+ +[a-zA-Z0-9]+\.[a-zA-Z0-9]+"  | sed -n ${countToken}p |  grep -Po "[0-9a-zA-Z]+" | sed -n 3p)
    

    done

    countTxin=$(($countTxin+1))
    txtin=$(echo "$result" | grep -Po "[a-zA-Z0-9]+ +[0-9]+ +[a-zA-Z0-9 \+\.\"]+" | sed -n ${countTxin}p)

    # echo "txtin " $txtin
    # echo " "
done

echo "" 
echo "Totales:" 

echo "lovelaceTotal: " $lovelaceTotal


for i in ${!tokens[@]}; do
    echo "tokenid: " ${tokens[$i]}
    echo "token total: " ${tokensTotal[$i]}
done

echo "" 

echo; read -rsn1 -p "Press any key to continue . . ."; echo