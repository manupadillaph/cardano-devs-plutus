#!/bin/bash

echo "Actualizando ENV en esta sesion ..."

source "$FALCON_DEVS_SCRIPTS/tools/write-env-list.sh"

#antes copiaba todo el contenido de esa lista dentro de .bashrc, pero ahora solo pongo la linea de source

CWD=$(pwd)
cd ~

swPrimero=1

#aqui me encargo de buscar si existe la linea ya y comentarla
while IFS= read -r line
do

    if [[ "$line" == *"# falcon-devs-env"* ]]; then
        if [ $swPrimero = 1 ]; then
            echo "# $line">.bashrc.temp
            swPrimero=0
        else
            echo "# $line">>.bashrc.temp
        fi
    else
         if [ $swPrimero = 1 ]; then
            echo $line>.bashrc.temp
            swPrimero=0
        else
            echo $line>>.bashrc.temp
        fi
    fi

done < .bashrc

#esta es la linea que voy a agregar
lineNew="[ -f \"$FALCON_DEVS_SCRIPTS/tools/write-env-list.sh\" ] && source \"$FALCON_DEVS_SCRIPTS/tools/write-env-list.sh\" # falcon-devs-env"

echo $lineNew>>.bashrc.temp

#aqui me encargo de buscar un nombre para guardar la copia de bk
extension=1
base=".bashrc.old"
file="${base}${extension}"

echo "FILE1: $file"

until ! [[ -f "$file"  ]]
do
    extension=$(($extension+1))
    file=${base}${extension}

    echo "FILE2: $file"

done

#aqui reemplazo el original por el temporal creado arriba

cp .bashrc "$file"

cp .bashrc.temp .bashrc

cd $CWD


echo "Para set env en esta sesion ejecute en promt:"
echo "source \"$FALCON_DEVS_SCRIPTS/tools/write-env-list.sh\""

#de antes:
# CWD=$(pwd)
# cd ~

# swDentro=0
# swPrimero=1

# while IFS= read -r line
# do
#     if [[ $line = "#INIT PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE" ]]; 
#     then
#         swDentro=1
#     fi
#     if [[ $swDentro = 0 ]]; 
#     then
        
#         if [ $swPrimero = 1 ]; then
#             echo $line>.bashrc.temp
#             swPrimero=0
#         else
#             echo $line>>.bashrc.temp
#         fi
#     fi
#     if [[ $line = "#END PLUTUS ENVS ~/.bashrc - DONT DELETE THIS LINE" ]]; 
#     then
#         swDentro=0
#     fi
# done < .bashrc



# while IFS= read -r line
# do
#     echo $line>>.bashrc.temp
# done < "$FALCON_DEVS_SCRIPTS/tools/write-env-list.sh"

# cat ~/.bashrc.temp

# extension=1
# base=".bashrc.old"
# file="${base}${extension}"

# echo "FILE1: $file"

# until ! [[ -f "$file"  ]]
# do
#     extension=$(($extension+1))
#     file=${base}${extension}

#     echo "FILE2: $file"

# done

# cp .bashrc "$file"

# cp .bashrc.temp .bashrc

# cd $CWD

# echo "ENVS Exported and Writed down in ~/.bashrc"
# echo "Backup in $file"




 

# #INIT PLUTUS ENVS ~/.bashrc


# sed -e '' ~/.bashrc > ~/.bashrc2
# echo "" >>  ~/.bashrc2
# echo $PLUTUS_ENV >>  ~/.bashrc2


# echo "Hecho!"