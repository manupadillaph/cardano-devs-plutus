#!/bin/bash


if [ -z "$1" ]; then
    echo "File name:"
    read file
else
  file=$1
fi

if [ -z "$2" ]; then
    echo "Name:"
    read name
else
  name=$2
fi

if [ -z "$3" ]; then
    echo "Passphrase:"
    read passphrase
else
  passphrase=$3
fi



# echo "file:"
# read file

echo "creating wallet with name $name passphrase $passphrase in $FALCON_DEVS_HASKELL_FILES/wallets/$file.json "

phrase=$($CARDANO_WALLET/cardano-wallet recovery-phrase generate)

x=''
sep=''
for word in $phrase
do
    x=$x$sep'"'$word'"'
    sep=', '
done

cat > $FALCON_DEVS_HASKELL_FILES/wallets/$file.json <<- EOM
{ "name": "$name"
, "mnemonic_sentence": [$x]
, "passphrase": "$passphrase"
}
EOM
echo "saved restoration file to $file.json"


bash $FALCON_DEVS_SCRIPTS/tools/wallet-create-con-cardano-wallet-desde-json.sh $file
