#!/bin/sh

echo "Nombre Wallet:"
read walletName

walletAddr=$(cat $FALCON_DEVS_HASKELL_FILES/${walletName}.addr)

walletTxIn=$(cat $FALCON_DEVS_HASKELL_FILES/${walletName}.utxo)

#walletSig=$(cat $FALCON_DEVS_HASKELL_FILES/01.skey | jq -r .cborHex)

walletSig=$(cat $FALCON_DEVS_HASKELL_FILES/01.pkh)


#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

scriptAddr=$(cat $FALCON_DEVS_HASKELL_FILES/${scriptName}.addr)

scriptTxIn=$(cat $FALCON_DEVS_HASKELL_FILES/${scriptName}.utxo)

$CARDANO_NODE/cardano-cli query protocol-parameters \
 	--out-file $FALCON_DEVS_HASKELL_FILES/protocol.json --testnet-magic 1097911063 


$CARDANO_NODE/cardano-cli query tip --testnet-magic 1097911063 | jq -r .slot >$FALCON_DEVS_HASKELL_FILES/tip.slot

tipSlot=$(cat $FALCON_DEVS_HASKELL_FILES/tip.slot)

$CARDANO_NODE/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $walletAddr \
    --tx-in $scriptTxIn \
    --tx-in-script-file $FALCON_DEVS_HASKELL_FILES/$scriptName.plutus \
    --tx-in-datum-file  $FALCON_DEVS_HASKELL_FILES/datum-DEF2.json \
    --tx-in-redeemer-file $FALCON_DEVS_HASKELL_FILES/redeemer-DEF.json  \
    --tx-in-collateral $walletTxIn \
    --required-signer-hash $walletSig \
    --required-signer=$FALCON_DEVS_HASKELL_FILES/${walletName}.skey \
    --protocol-params-file $FALCON_DEVS_HASKELL_FILES/protocol.json \
    --invalid-before ${tipSlot} \
    --out-file $FALCON_DEVS_HASKELL_FILES/${scriptName}.body 



# --tx-in-redeemer-value 444 

$CARDANO_NODE/cardano-cli transaction sign \
    --tx-body-file $FALCON_DEVS_HASKELL_FILES/${scriptName}.body \
    --signing-key-file $FALCON_DEVS_HASKELL_FILES/${walletName}.skey \
    --testnet-magic 1097911063 \
    --out-file  $FALCON_DEVS_HASKELL_FILES/${scriptName}.signed

$CARDANO_NODE/cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file $FALCON_DEVS_HASKELL_FILES/${scriptName}.signed



    