#!/bin/bash

echo "Arreglando permisos:"

echo " "
echo "Usuario: "$USUARIO

echo " "
CWD=$(pwd)
echo "Carpeta: "$CWD
echo " "

sudo chown $USUARIO -hR .
sudo find . -type d -exec chmod 755 {} \;
sudo find . -type f -exec chmod u+w  {} \;
sudo find . -name "*.sh" -type f -exec chmod +x {} \;

# sudo chown -hR $USUARIO .
# sudo chmod -R 777 ./
# sudo chmod -R u+w  .

echo; read -rsn1 -p "Press any key to continue . . ."; echo