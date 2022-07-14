#!/bin/bash


#vim ~/.bashrc  

echo "Actualizando ENV en esta sesion ..."

CARDANO_NODE=~/source/tools/cardano-node-1.35.0-linux2
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE/db/node.socket
CARDANO_TESNET_SHELLEY=$CARDANO_NODE/configuration/testnet-shelley-genesis.json
CARDANO_TESNET_BYRON=$CARDANO_NODE/configuration/testnet-byron-genesis.json

export CARDANO_NODE
export CARDANO_NODE_SOCKET_PATH
export CARDANO_TESNET_SHELLEY
export CARDANO_TESNET_BYRON

TESTNET_MAGIC=1097911063
export TESTNET_MAGIC

PLUTUS_APPS=~/source/tools/plutus-apps
export PLUTUS_APPS

CARDANO_WALLET=~/source/tools/cardano-wallet-v2022-07-01-linux64
export CARDANO_WALLET

CARDANO_CHAIN_INDEX_FILES=~/source/tools/cardano-chain-index
export CARDANO_CHAIN_INDEX_FILES

FALCON_DEVS=~/source/cardano-falcon-stakepool-devs
HASKELL=$FALCON_DEVS/cardano-falcon-stakepol-devs-haskell
FRONTEND=$FALCON_DEVS/cardano-falcon-stakepol-devs-reactjs-server-frontend
BACKEND=$FALCON_DEVS/cardano-falcon-stakepol-devs-nodejs-server-backend

export FALCON_DEVS
export HASKELL
export FRONTEND
export BACKEND

SCRIPTS=$HASKELL/scripts
export SCRIPTS

SCRIPTS_FILES=$SCRIPTS/files
export SCRIPTS_FILES

SCRIPTS_MAIN=$HASKELL/scripts/main.sh
export SCRIPTS_MAIN

NIX_SHELL=$SCRIPTS/tools/init-nix-shell.sh
export NIX_SHELL

INIT_NODE=$SCRIPTS/tools/init-cardano-node.sh
export INIT_NODE

CHECK_NODE=$SCRIPTS/tools/cardano-node-check.sh
export CHECK_NODE

INIT_WALLET=$SCRIPTS/tools/init-cardano-wallet-server.sh
export INIT_NODE

INIT_CHAIN=$SCRIPT/tools/init-chain-index-server.sh
export INIT_NODE

INIT_PLAY_SERVER=$SCRIPTS/tools/init-playground-server.sh
export INIT_PLAY_SERVER

INIT_PLAY_CLIENT=$SCRIPTS/tools/init-playground-client.sh
export INIT_PLAY_CLIENT

echo "Hecho!"