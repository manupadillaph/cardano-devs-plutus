#!/bin/sh

echo "Nombre Wallet:"
read walletName

$CARDANO_NODE/cardano-cli address key-gen \
	--verification-key-file $CARDANO_WALLET/wallets/${walletName}.vkey --signing-key-file $CARDANO_WALLET/wallets/${walletName}.skey 

$CARDANO_NODE/cardano-cli address key-hash \
	--verification-key-file $CARDANO_WALLET/wallets/${walletName}.vkey --out-file $CARDANO_WALLET/wallets/${walletName}.pkh 

$CARDANO_NODE/cardano-cli address build \
	 --payment-verification-key-file $CARDANO_WALLET/wallets/${walletName}.vkey --out-file $CARDANO_WALLET/wallets/${walletName}.addr --testnet-magic $TESTNET_MAGIC 
