
*******************************************  

## **Haskell Validator and Policie Scripts**


### **Cardano Developments Tools** 

*******************************************  

Compilar con:  
> cabal build  

Tambien se pueden instalar para acceder a los ejecutables sin usar cabal.   
> cabal install  
  
Y se crean exe en:  
>  ~/.cabal/bin/deploy-smart-contracts-manual-exe  
>  ~/.cabal/bin/deploy-smart-contracts-auto-exe  
 
*******************************************  

### Utilización

*******************************************  

Si se hizo cabal install, se tendran los ejecutables que sirven para exportar el codigo del validador en haskell a formato plutus core.  
Ambos ejecutables hacen lo mismo, pero se diferencian en lo que imprimen por pantalla.    
El ejecutable `deploy-smart-contracts-manual-exe` ofrece menues para elegir.  
El ejecutable `deploy-smart-contracts-auto-exe` no imprime menues.  
El segundo es usado por el script `main.sh` para enviarle los parametros y obtener el resultado sin intervencion del usuario.   

Si no se hizo cabal install se pueden executar de todas formas con cabal exec:    
> cabal exec deploy-smart-contracts-manual-exe  
> cabal exec deploy-smart-contracts-auto-exe  

<br>
Es importante desde que carpeta se ejecutan, pues van a querer guardar archivos y estos se guardan en un directorio relativo al path desde donde se estan ejecutando.  

La carpeta donde voy a querer guardar los archivos es: `scripts/files/` 

  
Hay una carpeta para cada tipo de arhivo exportado:    

- `scripts/files/config`: archivos de configuracion del protocolo necesarios para crear transacciones.  
- `scripts/files/datums`: archivos json de los datums.    
- `scripts/files/redeemers`: archivos json de los redeemers. 
- `scripts/files/transacciones`: archivos creados al hacer transacciones. Se crea el archivo .body primero y luego el .signed.
- `scripts/files/validators`: archivos .plutus, .hash y .addr de los validatores.    
- `scripts/files/wallets`: archivos .vkey, .skey, .pkh, .json, de cada wallet.

Si ejecuto `cabal exec deploy-smart-contracts-manual-exe` desde la raiz del repositorio, elijo la opción: `1: write script plutus cbor`, debo poner cuando me pide que ingrese path el siguiente: `scripts/files/validators`.  
  
Pero si ejecuto `cabal exec deploy-smart-contracts-auto-exe` desde la carpeta `scripts`, el path correcto será el siguiente: `files/validators`. Así es como se utiliza dentro del script `main.sh`  

Dentro de la carpeta `scripts` se encuentra el archivo `main.sh`. 
Es una aplicación de linea de comandos para interactuar con las wallets y los contratos.
  
Ejecutar con:
> cd scripts
> bash ./main.sh

La aplicación guardará todos los archivos creados en la carpeta `scripts/file`

Si se desea poder crear nuevas wallets por medio de esta aplicación se deben tener NODO y WALLET SERVER corriendo en la pc y configurado las siguientes variables de entorno:    

```
CARDANO_WALLET=ruta al server de WALLLET    
CARDANO_NODE=ruta al NODO   
TESTNET_MAGIC  
```

Y crear la carpeta `$CARDANO_WALLET/wallets  `

Para ver como correr NODO y WALLET y configurarlos para que funcionen con esta apliación referirse README del repositorio: `cardano-falcon-stakepol-devs-scripts`    


*******************************************  
### Solución de problemas

*******************************************  

- Problemas de permisos con git, ejecutar: `git config --global --add safe.directory /home/manuelpadilla/source/cardano-falcon-stakepool-devs/cardano-falcon-stakepol-devs-haskell`



*******************************************  

## **Work and Development Environment**


### **Cardano Falcon StakePool Developments Tools** 

*******************************************  
### **Windows Command Line**    
*******************************************  

Usar aplicacion: **Terminal**    

*******************************************  

### **WSL - Virtual Machine**    

*******************************************  

https://www.c-sharpcorner.com/article/how-to-install-windows-subsystem-for-linux-wsl2-on-windows-11/  
https://ubuntu.com/tutorials/install-ubuntu-on-wsl2-on-windows-11-with-gui-support#2-install-wsl    

-------------------------------------------      

Instalé **UBUNTU 20.04.4**    
Luego instalé nix y exporté imagen como ubuntu-nix-basic-image.tar    
Cargué la imagen en una nueva distro de wsl llamada ubuntu-nix    
Desinstalé UBUNTU 20.04.4 para quedarme solo con la versión que creé    
Instalé nodo y todas las configuraciones básicas que estan explicadas en este archivo y exporté como ubuntu-nix-cardano-image.tar  

-------------------------------------------     
**Abrir:**    
> wls -u manuelpadilla  

**Reboot:**  
> wsl --shutdown    
-------------------------------------------    

**Modificar** el archivo /etc/wsl.conf **para permitir chmod en wsl**:  
Create the file /etc/wsl.conf if it doesn’t exist  
Open the file (e.g.: nano /etc/wsl.conf) and add the following lines:  
> [automount]  
options = "metadata"  

Save the file and shutdown WSL launching   
> wsl --shutdown  

-------------------------------------------     
Ver **lista de maquinas virtuales** de wsl:  
> wsl --list   
Ubuntu-nix (Default)  
docker-desktop (\<-- es creada automaticamente por docker)  
docker-desktop-data (\<-- es creada automaticamente por docker) 

-------------------------------------------   
Set default distro:  
> wsl --setdefault DISTRO-NAME  
> wsl --setdefault Ubuntu-nix   
-------------------------------------------   
Convertir en version 2 para poder usarla en docker:  
> wsl --set-version Ubuntu-nix 2  
> wsl --set-default-version 2  
-------------------------------------------   
Ejecutar una especifica:  
> wsl -u \<Username> -d \<DistributionName>  
> wsl -u manuelpadilla -d Ubuntu-nix   
-------------------------------------------   
Exportar imagen de WSL:  
> wsl --export \<WSL Image Name> \<Export file>  
> wsl --export Ubuntu-nix C:\Users\manue\source\linux-install\images\ubuntu-nix-image.tar  

Importar imagen de WSL:  
> wsl --import \<Image Name you choose> \<Directory to store and run the image> \<Directory location of the exported .tar file>  
> wsl --import Ubuntu-nix "C:\Users\manue\source\linux-install\Ubuntu-nix" "C:\Users\manue\source\linux-install\images\ubuntu-nix-image.tar"  
-------------------------------------------   
Cargar imagen exportada de WSL en docker:  
> docker load -i \<image.tar>  
> docker load -i C:\Users\manue\source\linux-install\images\ubuntu-nix-image.tar  

Si no funciona, probar con:  
> docker import C:\Users\manue\source\linux-install\images\ubuntu-nix-image.tar ubuntu-nix  
-------------------------------------------   
Guardar el container de docker como nueva imagen para poder abrirlo de nuevo con discos montados, si es que ya la habia abierto la imagen sin discos  
> docker ps  -a  
> docker commit \<NUMERO> newimagename  
> docker commit 2f2846aa19f4 newimagename  

Ahora puedo abrir esa imagen en un nuevo container con mounts  

> docker run -ti -v \<RUTA EN WINDOWS>:\<RUTA EN LINUX> newimagename /bin/bash  
> docker run -ti -v C:\Users\manue\source\github:~/source/ newimagename /bin/bash  

Ahora puedo abrir este contenedor:  

> docker ps -a  
> docker container start -i \<NUMERO>  
> docker container start -i 20659b8c1542  
-------------------------------------------   

**Desinstalar Distro de WSL**  
Si es una oficial:  
> winget uninstall  
> winget uninstall --id "DISTRO-ID-NAME"  
> winget uninstall --id Canonical.Ubuntu  
> winget uninstall --id CanonicalGroupLimited.Ubuntu20.04LTS_79rhkp1fndgsc  

Si es una que yo creé:  
> wsl --list  
> wsl --unregister DISTRO-NAME  
> wsl --unregister Ubuntu  

*******************************************     
### **NIX**  
*******************************************     

**Instalar:**  

Actualizar entorno:  
> apt update  
> apt upgrade  
> apt-get install sudo  
> sudo adduser root sudo  
> sudo apt install curl  
> sudo apt-get install xz-utils  
> sudo apt-get install systemd  
> sudo apt install vim  
> sudo apt install less  
> sudo apt install git  

Preparar usuario:  
> sudo useradd manuelpadilla  
> sudo passwd manuelpadilla  
> sudo adduser manuelpadilla sudo  

Si no existe la carpeta del usuario en home, crearla:  
> cd home  
> mkdir manuelpadilla  
> sudo chown manuelpadilla: manuelpadilla  
> cd ..  

Loguearme con el usuario:  
> sudo -su manuelpadilla  

Version sin daemon, solo funciona en usuario que lo instala:  
> sh \<(curl -L https://nixos.org/nix/install) --no-daemon  

Version daemon para multiusuario (se necesita systemctl que no anda en las VM - NO LA USÉ)  
>sh \<(curl -L https://nixos.org/nix/install) --daemon  

Agregar el comando nix a mi linea de comandos:  
> source  /home/manuelpadilla/.nix-profile/etc/profile.d/nix.sh  

Preparar NIX   
> nix-shell -p nix-info --run "nix-info -m"  

-------------------------------------------  

**Configurar nix** para cardano como dicen en el tutorial: 

Hacer desde root:  
> sudo vim /etc/nix/nix.conf  

> Apretar I para insert.  
Pegar:  
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/  
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=  
Apretar ESC para salir del insert  
Apretar : y escribir wq para write and quit  

------------------------------------------- 

**Probar nix:**  
> nix-env --version  

*******************************************     
### **CONFIGURAR ENTORNO**  
*******************************************     

Crear symbolic link de la carpeta de github a una nueva llamada source en home directory:  
> mkdir ~/source/  
> ln -s /mnt/c/Users/manue/source/repos ~/source/  
> ln -s /mnt/c/Users/manue/source/tools ~/source/  
> ln -s /mnt/c/Users/manue/source/examples ~/source/  

-------------------------------------------   

Setear Variables de Entorno:  
> vim ~/.bashrc  

> CARDANO_NODE=\~/source/tools/cardano-node-1.34.1-linux  
CARDANO_NODE_SOCKET_PATH=\$CARDANO_NODE/db/node.socket  
CARDANO_TESNET_SHELLEY=\$CARDANO_NODE/configuration/testnet-shelley-genesis.json  
CARDANO_TESNET_BYRON=\$CARDANO_NODE/configuration/testnet-byron-genesis.json  
export CARDANO_NODE  
export CARDANO_NODE_SOCKET_PATH  
export CARDANO_TESNET_SHELLEY  
export CARDANO_TESNET_BYRON  
TESTNET_MAGIC=1097911063  
export TESTNET_MAGIC  
PLUTUS_APPS=\~/source/tools/plutus-apps  
export PLUTUS_APPS  
CARDANO_WALLET=\~/source/tools/cardano-wallet-v2022-05-27-linux64  
export CARDANO_WALLET  
SCRIPTS=\~/source/cardano-falcon-stakepool-devs/cardano-falcon-stakepol-devs-scripts/scripts
export SCRIPTS
NIX_SHELL=\$FALCON_DEVS_SCRIPTS/init-nix-shell.sh  
export NIX_SHELL  

- TESTNET_MAGIC viene del archivo testnet-shelley-genesis.json (leer CARDANO NODO)  
- cardano-node-1.34.1-linux (leer CARDANO NODO)  
- cardano-wallet-v2022-05-27-linux64 (leer CARDANO WALLET)  

-------------------------------------------   
**ABRIR NIX SHELL**  

> bash $NIX_SHELL  

init-nix-shell.sh:  
> #! /bin/sh  
source  /home/manuelpadilla/.nix-profile/etc/profile.d/nix.sh  
cd ~/source/tools/plutus-apps  
nix-shell   

-------------------------------------------   
Arreglar problemas de permisos:  

> cd directorio_con_problemas  
> bash $FALCON_DEVS_SCRIPTS/fix-permisos.sh  
> bash $FALCON_DEVS_SCRIPTS/fix-permisos-cabal.sh

-------------------------------------------  

Para compilar cabal:  
> [NIX-SHELL]> cabal build  

Para ejecutar cabal:  
> [NIX-SHELL]> cabal exec \<nombre del ejecutable\>  
> 
Para usar interprete haskell:  
> [NIX-SHELL]> cabal repl  

Tambien:  
> [NIX-SHELL]> ghci  

Para salir  
> GHCI> :quit  

*******************************************     
### **CARDANO NODO**  
*******************************************   

https://github.com/input-output-hk/cardano-node   
https://github.com/input-output-hk/cardano-node/releases  

-------------------------------------------   
Descargar Hydra Binaries:  
https://hydra.iohk.io/build/13065769  
cardano-node-1.34.1-linux.tar.gz  
  
Descomprimir en carpeta:  
\$CARDANO_NODE/  
  
Descargar archivos de configuracion (son 7) y pegar en la carpeta:  
\$CARDANO_NODE/configuration  
  
Del archivo testnet-shelley-genesis.json tomo el valor de networkMagic:
>   "networkMagic": 1097911063,

Y lo seteo en la variable de entorno \$TESTNET_MAGIC

-------------------------------------------   
**Iniciar nodo**:  

> bash $FALCON_DEVS_SCRIPTS/init-cardano-node.sh  

init-cardano-node.sh:  
> $CARDANO_NODE/cardano-node run \  
 --topology $CARDANO_NODE/configuration/testnet-topology.json \  
 --database-path $CARDANO_NODE/db \  
 --socket-path $CARDANO_NODE/db/node.socket \  
 --host-addr 127.0.0.1 \  
 --port 3001 \  
 --config $CARDANO_NODE/configuration/testnet-config.json  
  
El nodo corre en el puerto: `3001`    
  
-------------------------------------------   
**Chekear nodo**:  

> bash $FALCON_DEVS_SCRIPTS/check-cardano-node.sh  

check-cardano-node.sh:  
>	$CARDANO_NODE/cardano-cli \  
		query tip --testnet-magic $TESTNET_MAGIC  

*******************************************     
### **WALLET SERVER**  
******************************************* 

https://github.com/input-output-hk/cardano-wallet  
https://github.com/input-output-hk/cardano-wallet/releases/tag/v2022-05-27  

-------------------------------------------   

Utilizacion:  
https://developers.cardano.org/docs/integrate-cardano/creating-wallet-faucet  

------------------------------------------- 

Descargar Binaries:  
cardano-wallet-v2022-05-27-linux64.tar.gz  
  
Descomprimir en carpeta:  
$CARDANO_WALLET/  

------------------------------------------- 

**Iniciar**:  

Debe estar corriendo el nodo primero.  

> bash $FALCON_DEVS_SCRIPTS/init-cardano-wallet-server.sh  

init-cardano-wallet-server.sh:  
> $CARDANO_WALLET/cardano-wallet serve \  
    --testnet $CARDANO_TESNET_BYRON\  
    --node-socket $CARDANO_NODE_SOCKET_PATH  

El server de wallet corre en el puerto: `8090`   

Crear carpeta para almacenar wallets:
> cd $CARDANO_WALLET  
> mkdir wallets  

-------------------------------------------   

**Crear wallet**  
> bash $FALCON_DEVS_SCRIPTS/create-wallet.sh  

create-wallet.sh:  
> \$CARDANO_WALLET/cardano-wallet recovery-phrase generate  
> \$CARDANO_WALLET/cardano-wallet recovery-phrase generate>$CARDANO_WALLET/wallets/restore-wallet.json  

Creará un archivo json
> vim $CARDANO_WALLET/wallets/restore-wallet.json  

> {  
"name": "PAB testing wallet",   
"mnemonic_sentence": ["dad","cupboard","hotel","cause","mansion","feature","oppose","prevent","install","venue","finish","galaxy","tone","green","volcano","neglect","coil","toast","future","exchange","prize","social","next","tape"],   
"passphrase": "pab123456789"  
}  

------------------------------------------- 

**Load the wallet and Inspect**:  
> bash $FALCON_DEVS_SCRIPTS/load-and-check-wallet.sh  

A partir del archivo json se crea el id de la wallet, los archivos .prv y .pub que luego son transformados en .skey (PRIVATE KEY - SIGNING KEY) y .vkey (PUBLIC KEY - PAYMENT VERIFICATION KEY), del cual obtengo el PAYMENT VERIFICATION KEY HASH .pkh  
También genera el archivo .addr con la primera dirección de la WALLET  

Para eso usa:

> cardano-wallet key from-recovery-phrase  
> cardano-wallet key child 1852H/1815H/0H/0/0  
> cardano-wallet key public --with-chain-code  
> cardano-cli key convert-cardano-address-key  
> cardano-cli key verification-key  
> cardano-wallet key hash  
> cardano-cli address key-hash --payment-verification-key   
  
-------------------------------------------  

Agregar Test ADA:  
https://testnets.cardano.org/en/testnets/cardano/tools/faucet/  

-------------------------------------------   

Cardano Address Inspector:  
https://input-output-hk.github.io/cardano-addresses/demo/  
  
-------------------------------------------    
**Crear wallet usando CARDANO CLI**  

Crear Keys  
> [NIX-SHELL]>  $CARDANO_NODE/cardano-cli address key-gen \
	--verification-key-file 01.vkey --signing-key-file 01.skey   
> [NIX-SHELL]>  $CARDANO_NODE/cardano-cli address build \
	 --payment-verification-key-file 01.vkey --out-file 01.addr --testnet-magic $TESTNET_MAGIC 

> Addresses creadas:   
01  
addr_test1vz7dwumhyu3qncply2j2pev44sg0xevxq248wa66sa40e9qts3qrp  
ale  
addr_test1vrqps0njv48hqvp6t4ey7ltp3d53whhx2x90c2vngs4v82smhsc30  
nami  
addr_test1qz4ll7yrah8h5t3cv2qptn4mw22judsm9j9zychhmtuuzmszd3hm6w02uxx6h0s3qgd4hxgpvd0qzklnmahcx7v0mcysptyj8l  



*******************************************     
### **PLUTUS APP**  
*******************************************   

https://github.com/input-output-hk/plutus-apps  

-------------------------------------------   

Clonar en $PLUTUS_APPS  
  
  
******************************************* 

### **CHAIN INDEX Y PAB**  

******************************************* 

https://github.com/input-output-hk/plutus-apps/tree/c848b36a7f1fba7ce63814250c29ca0ea2d531cb/plutus-pab/test-node  

-------------------------------------------   

Crear chain index server y pab:  
> [NIX-SHELL]> cd $PLUTUS_APPS  
[NIX-SHELL]> cabal build plutus-pab-examples plutus-chain-index  

*******************************************   

### **CHAIN INDEX**  

*******************************************   

Edit chain-index-config.json con la direccion del socket del nodo:  
Archivo en: $PLUTUS_APPS/plutus-pab/test-node/testnet/chain-index-config.json  
  
Para obtener la dirección correcta:  
> echo $CARDANO_NODE_SOCKET_PATH  
Ej: /home/manuelpadilla/source/tools/cardano-node-1.34.1-linux/db/node.socket  

> vim $PLUTUS_APPS/plutus-pab/test-node/testnet/chain-index-config.json  
"cicSocketPath": "/home/manuelpadilla/source/tools/cardano-node-1.34.1-linux/db/node.socket",  

-------------------------------------------  

Start server:  
> [NIX-SHELL]> bash $FALCON_DEVS_SCRIPTS/init-chain-index-server.sh  

init-chain-index-server.sh:  
> #en [NIX-SHELL], para abrir ejecutar antes: bash $NIX_SHELL   
cd $PLUTUS_APPS/plutus-pab/test-node/  
cabal exec -- plutus-chain-index --config testnet/chain-index-config.json start-index 

Abrir en:  
http://localhost:9083/swagger/swagger-ui/  

El server de wallet corre en el puerto: `9083`   

*******************************************    

### **PAB**   

*******************************************   
 
Crea un server de API para conectar con el contrato que tenga creado.  
Es uno por projecto , no tengo creado ninguno, solo el de ejemplo.  
Configurar pab-config.yml:  
Archivo en: $PLUTUS_APPS/plutus-pab/test-node/testnet/pab-config.yml  
  
Para obtener la dirección correcta:  
> echo $CARDANO_NODE_SOCKET_PATH  
Ej: /home/manuelpadilla/source/tools/cardano-node-1.34.1-linux/db/node.socket  

> vim $PLUTUS_APPS/plutus-pab/test-node/testnet/pab-config.yml  
	pscSocketPath: /home/manuelpadilla/source/tools/cardano-node-1.34.1-linux/db/node.socket  

-------------------------------------------   

If it's the first time your running, you'll need to ask the PAB to make the database:  
> [NIX-SHELL]>  cd $PLUTUS_APPS/plutus-pab/test-node/  
[NIX-SHELL]>  cabal exec -- plutus-pab-examples \  
  --config testnet/pab-config.yml migrate  

------------------------------------------- 

Then, run the PAB  
> [NIX-SHELL]>  cd $PLUTUS_APPS/plutus-pab/test-node/  
[NIX-SHELL]>  cabal exec -- plutus-pab-examples \  
  --config testnet/pab-config.yml webserver \  
  --passphrase pab123456789 

  
El PAB backend server corre en el puerto: `9080`   


******************************************* 

### **Plutus playground**  

*******************************************   

Iniciar server:  
> [NIX-SHELL]>  bash $FALCON_DEVS_SCRIPTS/init-playground-server.sh  

init-playground.sh:  
> [NIX-SHELL]> cd $PLUTUS_APPS/plutus-playground-client/  
> [NIX-SHELL]> plutus-playground-server -i 120s  

En otra consola, inicio cliente:  
> [NIX-SHELL]>  bash $FALCON_DEVS_SCRIPTS/init-playground-client.sh  

init-playground-client.sh:  
> [NIX-SHELL]> cd $PLUTUS_APPS/plutus-playground-client  
> [NIX-SHELL]> npm start  

Abrir en:  
https://localhost:8009/  
https://playground.plutus.iohkdev.io/  

******************************************* 

### **CARDANO-SERIALIZATION-LIB**  

*******************************************  

https://developers.cardano.org/docs/get-started/cardano-serialization-lib/overview/  

https://github.com/Emurgo/cardano-serialization-lib  

-------------------------------------------  

Ejemplo de utlizacion en NODE + REACT JS:  

https://developers.cardano.org/docs/get-started/cardano-serialization-lib/create-react-app  

https://github.com/dynamicstrategies/cardano-wallet-connector  

*******************************************    

### **Documentacion de plutus**  

*******************************************     

Iniciar:  
> [NIX-SHELL]>  bash $FALCON_DEVS_SCRIPTS/init-plutus-docs.sh  

init-plutus-docs.sh:  
> [NIX-SHELL]> cd $PLUTUS_APPS  
[NIX-SHELL]> build-and-serve-docs  

Luego puedo abrir:  
http://localhost:8002/  
http://127.0.0.1:8002/  

Versiones Online:  
https://playground.plutus.iohkdev.io/doc/index.html  
https://plutus-apps.readthedocs.io/en/latest/  

Documentacion de modulos:  
http://127.0.0.1:8002/haddock/ 

Version Online:  
https://playground.plutus.iohkdev.io/doc/haddock/  


-------------------------------------------   

**Developers**  
https://developers.cardano.org/  

-------------------------------------------   

**PLUTUS PIONEER PROGRAM**  

https://github.com/input-output-hk/plutus-pioneer-program  
Intro:  
https://developers.cardano.org/docs/smart-contracts/plutus  
Clases youtube (iteracion #3):  
https://www.youtube.com/channel/UCX9j__vYOJu00iqBrCzecVw/playlists  
Las clases en texto (iteracion #2):  
https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week1.html  
Clases haskell:  
http://learnyouahaskell.com/chapters  
https://www.youtube.com/playlist?list=PLJ3w5xyG4JWmBVIigNBytJhvSSfZZzfTm  

-------------------------------------------  

**Para compilar** los ejercisios de cada semana hay que actualizar el PLUTUS APP a la version adecuada.  

Abro la carpeta de la semana:  
> cd ~/source/examples/plutus-pioneer-program/code/week01  

Abro el arhivo:  
> less cabal.project  
  
Copio el tag de plutus app:  
> location: https://github.com/input-output-hk/plutus-apps.git  
tag: 41149926c108c71831cfe8d244c83b0ee4bf5c8a  
  
Abro la carpeta de plutus app:  

> cd $PLUTUS_APPS  
  
Y actualizo el git a esa version:  

En Week 01:  
> git checkout -f 41149926c108c71831cfe8d244c83b0ee4bf5c8a  

Week 03:  
> git checkout -f  4edc082309c882736e9dec0132a3c936fe63b4ea  

Week 04:  
> git checkout -f ea1bfc6a49ee731c67ada3bfb326ee798001701a  

Si hay error de permisos usar también:
> git config --global --add safe.directory $PLUTUS_APPS 

Y abro el shell de nix, que va a tardar la primera vez bastante:  
> bash $NIX_SHELL  

Vuelvo a la carpeta de la semana:  
> [NIX-SHELL]> cd ~/source/examples/plutus-pioneer-program/code/week01  
  
Y hago build: 

> [NIX-SHELL]> cabal update  
[NIX-SHELL]> cabal build  



*******************************************   

### **LINKS UTILES**  

*******************************************   

BLOCKFROST  
https://blockfrost.io/dashboard 

Documentacion  
https://docs.blockfrost.io/  
Api para consultas a la blockchain  

-------------------------------------------  

STACK EXCHANGE  
https://cardano.stackexchange.com/  

------------------------------------------- 

BLOCKCHAIN EXPLORER  
https://testnet.cardanoscan.io/  

-------------------------------------------  

EPOCH CONVERTER (POSIX TIME)  
https://www.epochconverter.com/  

******************************************* 

### **PERMISOS**  

******************************************* 

Error con permisos:  
> cd directorio_a_revisar  
> bash $FALCON_DEVS_SCRIPTS/fix-permisos.sh  

fix-permisos.sh:  
> sudo chown manuelpadilla -R .  
sudo find . -type d -exec chmod 755 {} \;  
sudo find . -type f -exec chmod u+w  {} \;  
sudo find . -type f -exec chmod +x {} \;  

-------------------------------------------  

Error en cabal build con permisos:  
> cd directorio_a_revisar  
> bash $FALCON_DEVS_SCRIPTS/fix-permisos-cabal.sh  

fix-permisos-cabal.sh:  
> sudo chown -hR manuelpadilla .
sudo chmod -R 777 ./
sudo chmod -R u+w  .
  
******************************************* 

### **VCODE**  

*******************************************

Abrir visual studio code desde alguna carpeta:  
> code .
------------------------------------------  
Configurar GIT para que funcione en VCODE
Hacerlo en Windows y en WSL
> git config --global user.name "yourusername"  
> git config --global user.email "email@youremail.com"  





