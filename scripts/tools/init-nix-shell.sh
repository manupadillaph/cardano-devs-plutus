#!/bin/bash

#source  /home/manuelpadilla/.nix-profile/etc/profile.d/nix.sh

#sudo -su manuelpadilla
source  ~/.nix-profile/etc/profile.d/nix.sh

CWD=$(pwd)
export CWD

cd $PLUTUS_APPS
nix-shell

# --extra-experimental-features flakes # ya no necesito pasarlo por parametro 
# por que lo puse directamente en el archivo /etc/nix/nix.conf:
# experimental-features = nix-command flakes
# allow-import-from-derivation = true
# substituters = https://cache.nixos.org https://hydra.iohk.io
# trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=