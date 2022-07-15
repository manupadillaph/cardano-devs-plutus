#!/bin/bash

echo "Actualizando ENV en esta sesion ..."

source "$SCRIPTS/tools/write-env-list.sh"

swDentro=0

echo "">~/.bashrc2

while IFS= read -r line
do
    if [[ $line = "#INIT PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE" ]]; 
    then
        swDentro=1
    fi
    if [[ $swDentro = 0 ]]; 
    then
        echo $line>>~/.bashrc2
    fi
    if [[ $line = "#END PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE" ]]; 
    then
        swDentro=0
    fi

done < ~/.bashrc

while IFS= read -r line
do
    echo $line>>~/.bashrc2
done < "$SCRIPTS/tools/write-env-list.sh"

# cat ~/.bashrc2

sudo cp ~/.bashrc2 ~/.bashrc


echo "ENV var exported and writed down in ~/.bashrc"





 

# #INIT PLUTUS ENVS ~/.bashrc


# sed -e '' ~/.bashrc > ~/.bashrc2
# echo "" >>  ~/.bashrc2
# echo $PLUTUS_ENV >>  ~/.bashrc2


# echo "Hecho!"