#!/bin/sh

echo "Nombre Wallet:"
read walletName

walletAddr=$(cat files/${walletName}.addr)

walletTxIn=$(cat files/${walletName}.utxo)

#walletSig=$(cat files/01.skey | jq -r .cborHex)

walletSig=$(cat files/01.pkh)


#scriptName="plazo-fijo"
echo "Nombre Script File:"
read scriptName

scriptAddr=$(cat files/${scriptName}.addr)

scriptTxIn=$(cat files/${scriptName}.utxo)

$CARDANO_NODE/cardano-cli query protocol-parameters \
 	--out-file files/protocol.json --testnet-magic 1097911063 


$CARDANO_NODE/cardano-cli query tip --testnet-magic 1097911063 | jq -r .slot >files/tip.slot

tipSlot=$(cat files/tip.slot)

$CARDANO_NODE/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $walletAddr \
    --tx-in $scriptTxIn \
    --tx-in-script-file files/$scriptName.plutus \
    --tx-in-datum-file  files/datum-DEF2.json \
    --tx-in-redeemer-file files/redeemer-DEF.json  \
    --tx-in-collateral $walletTxIn \
    --required-signer-hash $walletSig \
    --required-signer=files/${walletName}.skey \
    --protocol-params-file files/protocol.json \
    --invalid-before ${tipSlot} \
    --out-file files/${scriptName}.body 



# --tx-in-redeemer-value 444 

$CARDANO_NODE/cardano-cli transaction sign \
    --tx-body-file files/${scriptName}.body \
    --signing-key-file files/${walletName}.skey \
    --testnet-magic 1097911063 \
    --out-file  files/${scriptName}.signed

$CARDANO_NODE/cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file files/${scriptName}.signed



    