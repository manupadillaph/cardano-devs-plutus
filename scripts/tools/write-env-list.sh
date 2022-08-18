#INIT PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE 

SOURCE=/home/manuelpadilla/source
export SOURCE

PLUTUS_APPS=$SOURCE/tools/plutus-apps
export PLUTUS_APPS



FALCON_DEVS_FRONTEND=$FALCON_DEVS/cardano-falcon-stakepol-devs-reactjs-server-frontend
export FALCON_DEVS_FRONTEND

FALCON_DEVS_BACKEND=$FALCON_DEVS/cardano-falcon-stakepol-devs-nodejs-server-backend
export FALCON_DEVS_BACKEND


FALCON_DEVS_HASKELL_FILES=$FALCON_DEVS_HASKELL/files
export FALCON_DEVS_HASKELL_FILES

FALCON_DEVS_HASKELL_FILES_CONFIG=$FALCON_DEVS_HASKELL/files/config
export FALCON_DEVS_HASKELL_FILES_CONFIG



MAIN1=$FALCON_DEVS_SCRIPTS/main.sh
export MAIN1



NIX_SHELL=$FALCON_DEVS_SCRIPTS/tools/init-nix-shell.sh
export NIX_SHELL

FIX_PERMISOS=$FALCON_DEVS_SCRIPTS/tools/fix-permisos.sh
export FIX_PERMISOS



INIT_NODE=$FALCON_DEVS_SCRIPTS/tools/init-cardano-node.sh
export INIT_NODE

CHECK_NODE=$FALCON_DEVS_SCRIPTS/tools/cardano-node-check.sh
export CHECK_NODE

INIT_WALLET=$FALCON_DEVS_SCRIPTS/tools/init-cardano-wallet-server.sh
export INIT_WALLET

INIT_CHAIN=$FALCON_DEVS_SCRIPTS/tools/init-chain-index-server.sh
export INIT_CHAIN

INIT_PLAY_SERVER=$FALCON_DEVS_SCRIPTS/tools/init-playground-server.sh
export INIT_PLAY_SERVER

INIT_PLAY_CLIENT=$FALCON_DEVS_SCRIPTS/tools/init-playground-client.sh
export INIT_PLAY_CLIENT

INIT_DOCS=$FALCON_DEVS_SCRIPTS/tools/init-plutus-docs.sh
export INIT_DOCS

WRITE_ENV_TESTNET=$FALCON_DEVS_SCRIPTS/tools/write-env-testnet.sh
export WRITE_ENV_TESTNET

WRITE_ENV_MAINNET=$FALCON_DEVS_SCRIPTS/tools/write-env-mainnet.sh
export WRITE_ENV_MAINNET

BACK2CWD=$FALCON_DEVS_SCRIPTS/tools/cd-back-to-cwd.sh
export BACK2CWD

#CARDANO_NODE=$SOURCE/tools/cardano-node-1.35.0-linux2
#CARDANO_NODE=$SOURCE/tools/cardano-node-1.35.3-testnetonly
#CARDANO_NODE=$SOURCE/tools/cardano-node-1.35.3-testnetonly
#CARDANO_NODE=$SOURCE/tools/cardano-node-1.35.2


if [[ $CARDANO_NODE_NETWORK = "TESTNET" ]]; then

    CARDANO_NODE=$SOURCE/tools/cardano-node-1.35.2
    export CARDANO_NODE

    CARDANO_NODE_PORT=3001
    export CARDANO_NODE_PORT

    CARDANO_NODE_DB_PATH=$CARDANO_NODE/db-testnet
    export CARDANO_NODE_DB_PATH 

    CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE/db-testnet/node.socket
    export CARDANO_NODE_SOCKET_PATH

    CARDANO_CONFIG=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-testnet/testnet-config.json
    export CARDANO_CONFIG

    CARDANO_TOPOLOGY=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-testnet/testnet-topology.json
    export CARDANO_TOPOLOGY

    CARDANO_SHELLEY=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-testnet/testnet-shelley-genesis.json
    export CARDANO_SHELLEY

    CARDANO_BYRON=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-testnet/testnet-byron-genesis.json
    export CARDANO_BYRON

    #TESTNET_MAGIC="testnet-magic 1097911063"
    TESTNET_MAGIC="testnet-magic 1097911063"  
    export TESTNET_MAGIC

    CARDANO_CHAIN_INDEX_TEMPLATE_CONFIG=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-chain-index/chain-index-config-testnet.TEMPLATE.json
    export CARDANO_CHAIN_INDEX_TEMPLATE_CONFIG

    CARDANO_CHAIN_INDEX_CONFIG=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-chain-index/chain-index-config-testnet.json
    export CARDANO_CHAIN_INDEX_CONFIG

    CARDANO_CHAIN_INDEX_DB=$SOURCE/tools/cardano-chain-index/chain-index-testnet.db
    export CARDANO_CHAIN_INDEX_DB

else 

    CARDANO_NODE=$SOURCE/tools/cardano-node-1.35.3-linux
    export CARDANO_NODE

    CARDANO_NODE_PORT=3001
    export CARDANO_NODE_PORT

    CARDANO_NODE_DB_PATH=$CARDANO_NODE/db-mainnet
    export CARDANO_NODE_DB_PATH 

    CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE/db-mainnet/node.socket
    export CARDANO_NODE_SOCKET_PATH

    CARDANO_CONFIG=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-mainnet/config.json
    export CARDANO_CONFIG

    CARDANO_TOPOLOGY=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-mainnet/topology.json
    export CARDANO_TOPOLOGY

    CARDANO_SHELLEY=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-mainnet/shelley-genesis.json
    export CARDANO_SHELLEY

    CARDANO_BYRON=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-mainnet/byron-genesis.json
    export CARDANO_BYRON

    CARDANO_MAINNET_ALONZO=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-mainnet/alonzo-genesis.json
    export CARDANO_MAINNET_ALONZO

    # CARDANO_MAINNET_RESTCONFIG=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-mainnet/rest-config.json
    # export CARDANO_MAINNET_RESTCONFIG

    # CARDANO_MAINNET_DBSYNC=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-node-mainnet/mainnet-db-sync-config.json
    # export CARDANO_MAINNET_DBSYNC

    TESTNET_MAGIC="mainnet"
    export TESTNET_MAGIC

    CARDANO_CHAIN_INDEX_TEMPLATE_CONFIG=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-chain-index/chain-index-config-mainnet.TEMPLATE.json
    export CARDANO_CHAIN_INDEX_TEMPLATE_CONFIG

    CARDANO_CHAIN_INDEX_CONFIG=$FALCON_DEVS_HASKELL_FILES_CONFIG/cardano-chain-index/chain-index-config-mainnet.json
    export CARDANO_CHAIN_INDEX_CONFIG

    CARDANO_CHAIN_INDEX_DB=$SOURCE/tools/cardano-chain-index/chain-index-mainnet.db
    export CARDANO_CHAIN_INDEX_DB

fi


CARDANO_WALLET=$SOURCE/tools/cardano-wallet-v2022-07-01-linux64
export CARDANO_WALLET



CARDANO_PAB_SERVER_TEMPLATE_CONFIG=$FALCON_DEVS_HASKELL_FILES/config/pab/pab-config.TEMPLATE.yml
export CARDANO_PAB_SERVER_TEMPLATE_CONFIG

CARDANO_PAB_SERVER_CONFIG=$FALCON_DEVS_HASKELL_FILES/config/pab/pab-config.VALIDATOR_SCRIPT_NAME.yml
export CARDANO_PAB_SERVER_CONFIG

CARDANO_PAB_DATABASE=$FALCON_DEVS_HASKELL_FILES/pab/plutus-pab.VALIDATOR_SCRIPT_NAME.db
export CARDANO_PAB_DATABASE

USUARIO=manuelpadilla
export USUARIO

CARDANO_TOOLS_TOKEN_METADATA_CREATOR=$SOURCE/tools/token-metadata-creator/token-metadata-creator
export CARDANO_TOOLS_TOKEN_METADATA_CREATOR

#END PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE
