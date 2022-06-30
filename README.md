
*******************************************  

## **Haskell Validator and Policie Scripts**


### **Cardano Falcon Stake Pool Developments Tools** 

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

