#INIT PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE 

CARDANO_NODE_NETWORK="MAINNET"
export CARDANO_NODE_NETWORK


FALCON_DEVS=$SOURCE/copyRepos/Falcon-Devs
export FALCON_DEVS

FALCON_DEVS_HASKELL=$FALCON_DEVS/cardano-falcon-stakepol-devs-haskell
export FALCON_DEVS_HASKELL

FALCON_DEVS_SCRIPTS=$FALCON_DEVS_HASKELL/scripts
export FALCON_DEVS_SCRIPTS


source "$FALCON_DEVS_SCRIPTS/tools/write-env-list.sh"


#END PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE