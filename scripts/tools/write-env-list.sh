#INIT PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE 


PLUTUS_APPS=~/source/tools/plutus-apps
export PLUTUS_APPS

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

HASKELL_FILES=$HASKELL/files
export HASKELL_FILES

HASKELL_FILES_CONFIG=$HASKELL/files/config
export HASKELL_FILES_CONFIG

MAIN=$HASKELL/scripts/main.sh
export MAIN

NIX_SHELL=$SCRIPTS/tools/init-nix-shell.sh
export NIX_SHELL

INIT_NODE=$SCRIPTS/tools/init-cardano-node.sh
export INIT_NODE

CHECK_NODE=$SCRIPTS/tools/cardano-node-check.sh
export CHECK_NODE

INIT_WALLET=$SCRIPTS/tools/init-cardano-wallet-server.sh
export INIT_WALLET

INIT_CHAIN=$SCRIPTS/tools/init-chain-index-server.sh
export INIT_CHAIN

INIT_PLAY_SERVER=$SCRIPTS/tools/init-playground-server.sh
export INIT_PLAY_SERVER

INIT_PLAY_CLIENT=$SCRIPTS/tools/init-playground-client.sh
export INIT_PLAY_CLIENT

INIT_DOCS=$SCRIPTS/tools/init-plutus-docs.sh
export INIT_DOCS

WRITE_ENV=$SCRIPTS/tools/write-env.sh
export WRITE_ENV

CARDANO_NODE=~/source/tools/cardano-node-1.35.0-linux2
export CARDANO_NODE

CARDANO_NODE_PORT=3001
export CARDANO_NODE_PORT

CARDANO_NODE_DB_PATH=$CARDANO_NODE/db
export CARDANO_NODE_DB_PATH 

CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE/db/node.socket
export CARDANO_NODE_SOCKET_PATH

CARDANO_TESNET_CONFIG=$HASKELL_FILES_CONFIG/cardano-node/testnet-config.json
export CARDANO_TESNET_CONFIG

CARDANO_TESNET_TOPOLOGY=$HASKELL_FILES_CONFIG/cardano-node/testnet-topology.json
export CARDANO_TESNET_TOPOLOGY

CARDANO_TESNET_SHELLEY=$HASKELL_FILES_CONFIG/cardano-node/testnet-shelley-genesis.json
export CARDANO_TESNET_SHELLEY

CARDANO_TESNET_BYRON=$HASKELL_FILES_CONFIG/cardano-node/testnet-byron-genesis.json
export CARDANO_TESNET_BYRON

CARDANO_WALLET=~/source/tools/cardano-wallet-v2022-07-01-linux64
export CARDANO_WALLET

CARDANO_CHAIN_INDEX_CONFIG=$HASKELL_FILES_CONFIG/cardano-chain-index/chain-index-config.json
export CARDANO_CHAIN_INDEX_CONFIG

TESTNET_MAGIC=1097911063
export TESTNET_MAGIC

#END PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE
