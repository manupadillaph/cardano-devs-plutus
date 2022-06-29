
*******************************************  

## **Haskell Validator and Policie Scripts**


### **Cardano Falcon StakePool Developments Tools** 

*******************************************  

Compilar con:  
> cabal build  

Una vez compilado puedo ejecutar deploy para obtener el codigo plutus.  
Hay una version que imprime por pantalla y otra que no. 
La que no imprime es utilizada por el script con argumentos para que haga todo sin mostrar salidas.  

Ejecutar deploy con:  
> cabal exec deploy-smart-contracts-manual-exe   
> cabal exec deploy-smart-contracts-auto-exe  

O si lo prefiero, puedo instalar para acceder a los ejecutables sin usar cabal.  
Para ello:  
> cabal install  
  
Y se crean exe en:  
>  ~/.cabal/bin/deploy-smart-contracts-manual-exe  
>  ~/.cabal/bin/deploy-smart-contracts-auto-exe  
 
Utilizar opciones para hacer deploy de los distintos contratos, datums y redeemers.   

Ejecutar main.sh para acceder a la aplicación principal por linea de comandos que interactua con los contratos.  

> cd scripts  
> ./main.sh  

Todos los archivos de wallets creados, contratos exportados, datums y redeemers se guardan en la carpeta scripts/files  

*******************************************  
### Solución de problemas

*******************************************  

- Problemas con git: `git config --global --add safe.directory /home/manuelpadilla/source/cardano-falcon-stakepool-devs/cardano-falcon-stakepol-devs-haskell`

