#!/bin/bash


if [ -z "$1" ]; then
  echo "Ingrese wallet name:"
  read file
else
  file=$1
fi

FRASE=$(cat $CARDANO_WALLET/wallets/$file.json | jq -r '(.mnemonic_sentence | join(" "))' )

# Byron | Icarus | Shelley | Shared
#echo "PALABRAS:" $FRASE 


echo $FRASE | $CARDANO_WALLET/cardano-wallet key from-recovery-phrase  Shelley > $CARDANO_WALLET/wallets/$file.root.prv

cat $CARDANO_WALLET/wallets/$file.root.prv | $CARDANO_WALLET/cardano-wallet key walletid  > $CARDANO_WALLET/wallets/$file.id

WALLET_ID=$(cat $CARDANO_WALLET/wallets/$file.id)
echo "WALLET ID:" $WALLET_ID

echo "" 
cat $CARDANO_WALLET/wallets/$file.root.prv \
  | $CARDANO_WALLET/cardano-wallet key child 1852H/1815H/0H/0/0 >  $CARDANO_WALLET/wallets/$file.prv

cat $CARDANO_WALLET/wallets/$file.prv | $CARDANO_WALLET/cardano-wallet key public --with-chain-code > $CARDANO_WALLET/wallets/$file.pub

echo "Inspect $file.root.prv:"
cat $CARDANO_WALLET/wallets/$file.root.prv
echo "" 
$CARDANO_WALLET/cardano-wallet key inspect <<< $(cat $CARDANO_WALLET/wallets/$file.root.prv)
echo "" 
echo "Inspect $file.prv:"
cat $CARDANO_WALLET/wallets/$file.prv
echo "" 
$CARDANO_WALLET/cardano-wallet key inspect <<< $(cat $CARDANO_WALLET/wallets/$file.prv)
echo "" 
echo "Inspect $file.pub:"
cat $CARDANO_WALLET/wallets/$file.pub
echo "" 
$CARDANO_WALLET/cardano-wallet key inspect <<< $(cat $CARDANO_WALLET/wallets/$file.pub)


echo ""
echo "WALLET PRIVATE KEY - SIGNING KEY - $file.skey: "

$CARDANO_NODE/cardano-cli key convert-cardano-address-key --signing-key-file $CARDANO_WALLET/wallets/$file.root.prv --shelley-payment-key --out-file $CARDANO_WALLET/wallets/$file.root.skey

cat $CARDANO_WALLET/wallets/$file.root.skey

$CARDANO_NODE/cardano-cli key convert-cardano-address-key --signing-key-file $CARDANO_WALLET/wallets/$file.prv --shelley-payment-key --out-file $CARDANO_WALLET/wallets/$file.skey

cat $CARDANO_WALLET/wallets/$file.skey

echo ""
echo "WALLET PUBLIC KEY - PAYMENT VERIFICATION KEY - $file.vkey: "

$CARDANO_NODE/cardano-cli key verification-key --signing-key-file $CARDANO_WALLET/wallets/$file.skey \
                                 --verification-key-file $CARDANO_WALLET/wallets/$file.vkey

cat $CARDANO_WALLET/wallets/$file.vkey

echo ""
echo "WALLET PUBLIC KEY HASH - PAYMENT VERIFICATION KEY HASH - $file.pkh: "

#cat $CARDANO_WALLET/wallets/$file.pub | $CARDANO_WALLET/cardano-wallet key hash > $CARDANO_WALLET/wallets/$file.bech32.pkh
cat $CARDANO_WALLET/wallets/$file.pub | $CARDANO_WALLET/cardano-wallet key hash --hex > $CARDANO_WALLET/wallets/$file.pkh

#WALLET_PUB_HASH_32=$(cat $CARDANO_WALLET/wallets/$file.bech32.pkh)
WALLET_PUB_HAS_HEX=$(cat  $CARDANO_WALLET/wallets/$file.pkh)

#echo "BENCH 32 con cardano wallet: " $WALLET_PUB_HASH_32
echo "Con cardano wallet: " 
echo $WALLET_PUB_HAS_HEX

echo ""
echo "Con cardano cli desde $file.pub: "

VERIFICATION_KEY=$(cat $CARDANO_WALLET/wallets/$file.pub)
$CARDANO_NODE/cardano-cli address key-hash --payment-verification-key $VERIFICATION_KEY

echo ""
echo "Con cardano cli desde $file.vkey: "

$CARDANO_NODE/cardano-cli address key-hash --payment-verification-key-file $CARDANO_WALLET/wallets/$file.vkey


echo ""

curl -H "content-type: application/json" -XPOST \
  -d @$CARDANO_WALLET/wallets/$file.json \
  localhost:8090/v2/wallets

echo ""
echo ""

DIRECCIONES=$(curl -H "content-type: application/json" \
      -XGET localhost:8090/v2/wallets/$WALLET_ID/addresses | jq -r '.[]' )

echo "Quieres ver todas las direcciones?"
read opcion
if [[ $opcion = "y" ]]; then 

  echo "WALLET ADDRESSES:"

  echo $DIRECCIONES | jq -r '.id' | nl 

fi

DIRECCION=$(echo $DIRECCIONES | jq -r '.id'| sed -n 1p)

echo ""
echo "DIRECCION GUARDADA:" $DIRECCION

echo $DIRECCION>$CARDANO_WALLET/wallets/$file.addr


walletAddr=$(cat $CARDANO_WALLET/wallets/$file.addr)

echo ""
echo "Quieres ver todas las utxu en esa direccion?"
read opcion
if [[ $opcion = "y" ]]; then 
  echo "utxu:"

  $CARDANO_NODE/cardano-cli query utxo\
    --address $walletAddr --testnet-magic $TESTNET_MAGIC 
fi