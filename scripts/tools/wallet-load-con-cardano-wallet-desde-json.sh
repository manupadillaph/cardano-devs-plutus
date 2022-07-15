#!/bin/bash


if [ -z "$1" ]; then
  echo "Ingrese wallet name:"
  read file
else
  file=$1
fi

echo "JSON:"  $SCRIPTS_FILES/wallets/$file.json


echo ""

curl -H "content-type: application/json" -XPOST \
  -d @$SCRIPTS_FILES/wallets/$file.json \
  localhost:8090/v2/wallets

echo ""
echo ""