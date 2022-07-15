#!/bin/sh

echo "Nombre Wallet:"
read walletName

walletAddr=$(cat $HASKELL_FILES/${walletName}.addr)

walletTxIn=$(cat $HASKELL_FILES/${walletName}.utxo)

#walletSig=$(cat $HASKELL_FILES/01.skey | jq -r .cborHex)

walletSig=$(cat $HASKELL_FILES/01.pkh)


#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

scriptAddr=$(cat $HASKELL_FILES/${scriptName}.addr)

scriptTxIn=$(cat $HASKELL_FILES/${scriptName}.utxo)

$CARDANO_NODE/cardano-cli query protocol-parameters \
 	--out-file $HASKELL_FILES/protocol.json --testnet-magic 1097911063 


$CARDANO_NODE/cardano-cli query tip --testnet-magic 1097911063 | jq -r .slot >$HASKELL_FILES/tip.slot

tipSlot=$(cat $HASKELL_FILES/tip.slot)

$CARDANO_NODE/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $walletAddr \
    --tx-in $scriptTxIn \
    --tx-in-script-file $HASKELL_FILES/$scriptName.plutus \
    --tx-in-datum-file  $HASKELL_FILES/datum-DEF2.json \
    --tx-in-redeemer-file $HASKELL_FILES/redeemer-DEF.json  \
    --tx-in-collateral $walletTxIn \
    --required-signer-hash $walletSig \
    --required-signer=$HASKELL_FILES/${walletName}.skey \
    --protocol-params-file $HASKELL_FILES/protocol.json \
    --invalid-before ${tipSlot} \
    --out-file $HASKELL_FILES/${scriptName}.body 



# --tx-in-redeemer-value 444 

$CARDANO_NODE/cardano-cli transaction sign \
    --tx-body-file $HASKELL_FILES/${scriptName}.body \
    --signing-key-file $HASKELL_FILES/${walletName}.skey \
    --testnet-magic 1097911063 \
    --out-file  $HASKELL_FILES/${scriptName}.signed

$CARDANO_NODE/cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file $HASKELL_FILES/${scriptName}.signed



    