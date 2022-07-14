#!/bin/sh

echo "Nombre Wallet:"
read walletName

walletAddr=$(cat $SCRIPTS_FILES/${walletName}.addr)

walletTxIn=$(cat $SCRIPTS_FILES/${walletName}.utxo)

#walletSig=$(cat $SCRIPTS_FILES/01.skey | jq -r .cborHex)

walletSig=$(cat $SCRIPTS_FILES/01.pkh)


#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

scriptAddr=$(cat $SCRIPTS_FILES/${scriptName}.addr)

scriptTxIn=$(cat $SCRIPTS_FILES/${scriptName}.utxo)

$CARDANO_NODE/cardano-cli query protocol-parameters \
 	--out-file $SCRIPTS_FILES/protocol.json --testnet-magic 1097911063 


$CARDANO_NODE/cardano-cli query tip --testnet-magic 1097911063 | jq -r .slot >$SCRIPTS_FILES/tip.slot

tipSlot=$(cat $SCRIPTS_FILES/tip.slot)

$CARDANO_NODE/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $walletAddr \
    --tx-in $scriptTxIn \
    --tx-in-script-file $SCRIPTS_FILES/$scriptName.plutus \
    --tx-in-datum-file  $SCRIPTS_FILES/datum-DEF2.json \
    --tx-in-redeemer-file $SCRIPTS_FILES/redeemer-DEF.json  \
    --tx-in-collateral $walletTxIn \
    --required-signer-hash $walletSig \
    --required-signer=$SCRIPTS_FILES/${walletName}.skey \
    --protocol-params-file $SCRIPTS_FILES/protocol.json \
    --invalid-before ${tipSlot} \
    --out-file $SCRIPTS_FILES/${scriptName}.body 



# --tx-in-redeemer-value 444 

$CARDANO_NODE/cardano-cli transaction sign \
    --tx-body-file $SCRIPTS_FILES/${scriptName}.body \
    --signing-key-file $SCRIPTS_FILES/${walletName}.skey \
    --testnet-magic 1097911063 \
    --out-file  $SCRIPTS_FILES/${scriptName}.signed

$CARDANO_NODE/cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file $SCRIPTS_FILES/${scriptName}.signed



    