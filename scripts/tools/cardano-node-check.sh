#!/bin/bash

$CARDANO_NODE/cardano-cli \
	query tip --testnet-magic $TESTNET_MAGIC
