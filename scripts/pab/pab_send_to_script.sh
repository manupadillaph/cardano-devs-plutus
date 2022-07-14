#!/bin/bash

WALLET_ID=$(cat $SCRIPTS_FILES/wallets/$walletName.id)
echo "WALLET ID:" $WALLET_ID

#Para poder ejecutar el cabal exec necesito estar en la carpeta $HASKELL donde hice el cabal build
CWD=$(pwd)
cd $HASKELL

pkh=$(cabal exec utils-payment-key-hash -- $walletAddr)
skh=$(cabal exec utils-stake-key-hash -- $walletAddr)

cd $CWD

echo "payment key hash: $pkh"
echo "stake key hash: $skh"



# data StartParams = StartParams
#     { 
#         spDeadline :: !POSIXTime
#         , spName :: !Integer
#         , spAdaQty   :: !Integer
#     } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema,  P.Show)

# data GetParams = GetParams
#     { 
#         gpName :: !Integer
#         , gpAdaQty   :: !Integer
#     } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON,ToSchema, Data.OpenApi.Schema.ToSchema, P.Show)


curl -X 'POST' \
  'http://localhost:9080/api/contract/activate' \
  -H 'accept: application/json;charset=utf-8' \
  -H 'Content-Type: application/json;charset=utf-8' \
  -d '{
    "caWallet": {"getWalletId": "'"$WALLETID"'"},
    "caID": {
        "contents": {
            "spDeadline": 1657143764000,
            "spName": 0,
            "spAdaQty": 0
        },
        "tag": "Start"
    }
}'


# "tpAddress": {
#                 "addressCredential": {
#                     "contents": {"getPubKeyHash": "'"$pkh"'"},
#                     "tag": "PubKeyCredential"
#                 },
#                 "addressStakingCredential": {
#                     "contents": {
#                         "contents": {"getPubKeyHash": "'"$skh"'"},
#                         "tag": "PubKeyCredential"
#                     },
#                     "tag": "StakingHash"
#                 }
#             },