#!/usr/bin/env bash

set -euo pipefail

here="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"
cd "$here"

export CARDANO_NODE_SOCKET_PATH=$HOME/ipc/node.socket
export MAGIC="--testnet-magic 1"

sudo cardano-cli address build --payment-script-file plutus/treasury_script.plutus --stake-verification-key-file keys/stake.vkey $MAGIC --out-file plutus/treasury.addr
sudo cardano-cli address build --payment-script-file plutus/staking_script.plutus $MAGIC --out-file plutus/staking.addr
sudo cardano-cli transaction policyid --script-file plutus/factory_policy.plutus > plutus/factory
