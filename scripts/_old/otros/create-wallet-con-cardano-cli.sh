#!/bin/sh

echo "Nombre Wallet:"
read walletName

$CARDANO_NODE/cardano-cli address key-gen \
	--verification-key-file $HASKELL_FILES/wallets/${walletName}.vkey --signing-key-file $HASKELL_FILES/wallets/${walletName}.skey 

$CARDANO_NODE/cardano-cli address key-hash \
	--verification-key-file $HASKELL_FILES/wallets/${walletName}.vkey --out-file $HASKELL_FILES/wallets/${walletName}.pkh 

$CARDANO_NODE/cardano-cli address build \
	 --payment-verification-key-file $HASKELL_FILES/wallets/${walletName}.vkey --out-file $HASKELL_FILES/wallets/${walletName}.addr --testnet-magic $TESTNET_MAGIC 
