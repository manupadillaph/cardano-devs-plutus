#!/bin/bash

echo "Arreglando permisos:"

echo " "
echo "Usuario: "$USUARIO

sudo chown -hR $USUARIO .
sudo chmod -R 777 ./
sudo chmod -R u+w  .


echo; read -rsn1 -p "Press any key to continue . . ."; echo