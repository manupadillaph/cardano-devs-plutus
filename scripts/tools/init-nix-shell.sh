#!/bin/bash

#source  /home/manuelpadilla/.nix-profile/etc/profile.d/nix.sh
source  ~/.nix-profile/etc/profile.d/nix.sh

cd $PLUTUS_APPS
nix-shell --extra-experimental-features flakes
