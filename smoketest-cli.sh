#!/bin/bash

# download all files here https://cardano-course.gitbook.io/cardano-course/handbook/module-1-build-and-run-the-node/running-the-node-and-connecting-to-a-network
network_dir="/home/mgalazyn/workspace/iohk/network-preview"
export CARDANO_NODE_SOCKET_PATH="$network_dir/node.socket"
export CARDANO_NODE_NETWORK_ID=2

temp_dir=$(mktemp -d)  # Create a temporary directory
function cleanup {
  echo "Press enter to remove $temp_dir and finish..."
  read -r
  rm -rf "$temp_dir"
}
trap cleanup EXIT ERR


# cd into fusion-flamingo
pushd ..
cli=$(cabal list-bin cardano-cli)
addr="/home/mgalazyn/workspace/iohk/network-preview/cardano-address"

pushd "$temp_dir"

set -euxo pipefail

$cli address key-gen --verification-key-file payment.vkey --signing-key-file payment.skey
$cli key verification-key --verification-key-file /dev/stdout --signing-key-file payment.skey

$cli stake-address key-gen \
  --verification-key-file stake.vkey \
  --signing-key-file stake.skey
$cli address build \
  --payment-verification-key-file payment.vkey \
  --stake-verification-key-file stake.vkey \
  --out-file payment.addr

$cli node key-gen \
  --cold-verification-key-file cold.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter-file opcert.counter
$cli node key-gen-KES \
  --verification-key-file kes.vkey \
  --signing-key-file kes.skey
$cli node key-gen-VRF \
  --verification-key-file vrf.vkey \
  --signing-key-file vrf.skey

$cli stake-address registration-certificate \
  --stake-verification-key-file stake.vkey \
  --out-file registration.cert

$cli address key-hash --payment-verification-key-file payment.vkey 
$cli address info --address $(cat payment.addr)

$cli stake-address build --stake-verification-key-file stake.vkey
$cli stake-address deregistration-certificate --stake-verification-key-file stake.vkey --out-file deregistration.cert

echo -e "\n\nSend funds from preview testnet faucet https://docs.cardano.org/cardano-testnet/tools/faucet/ to the address:\n$(cat payment.addr)\nand press enter..."
read -r

$cli query utxo --address $(cat payment.addr)

$cli transaction build \
  --witness-override 2 \
  --tx-in $($cli query utxo --address $(cat payment.addr) --out-file  /dev/stdout | jq -r 'keys[0]') \
  --change-address $(cat payment.addr) \
  --certificate-file registration.cert \
  --out-file tx.raw

$cli transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file payment.skey \
  --signing-key-file stake.skey \
  --out-file tx.signed

$cli transaction submit \
  --tx-file tx.signed

$cli stake-pool registration-certificate \
  --cold-verification-key-file cold.vkey \
  --vrf-verification-key-file vrf.vkey \
  --pool-pledge 9000000000 \
  --pool-cost 340000000 \
  --pool-margin 0.05 \
  --pool-reward-account-verification-key-file stake.vkey \
  --pool-owner-stake-verification-key-file stake.vkey \
  --testnet-magic 2 \
  --pool-relay-ipv4 104.77.244.203 \
  --pool-relay-port 30303 \
  --metadata-url https://git.io/JJWdJ \
  --metadata-hash 3c914463aa1cddb425fba48b21c4db31958ea7a30e077f756a82903f30e04905 \
  --out-file pool-registration.cert

$cli stake-address delegation-certificate \
  --stake-verification-key-file stake.vkey \
  --cold-verification-key-file cold.vkey \
  --out-file delegation.cert

$cli transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file payment.skey \
  --signing-key-file cold.skey \
  --signing-key-file stake.skey \
  --out-file tx.signed

echo -e "Get delegation from the faucet for the pool:\n$($cli stake-pool id --cold-verification-key-file cold.vkey --output-format bech32)\n and press enter..."
read -r

$cli node issue-op-cert --kes-verification-key-file kes.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter-file opcert.counter \
  --kes-period $(($($cli query tip --testnet-magic 2 | jq .slot) / 129600)) \
  --out-file opcert.cert


# $cli query protocol-parameters
# $cli query tip
# $cli query stake-pools
# $cli query stake-distribution
# $cli query stake-address-info --address stake_test1upp6fr9evvqcprgr57vq0u9mdwxck2whdlyjhve750xl29gu5zexv
# $cli query utxo --address addr_test1qr3q67fq0vfjdsksapc3jmuy7h03udw54374afn6tx2zcuzr5jxtjccpszxs8fucqlctk6ud3v5awm7f9wenag7d752spy4eh4
# $cli query utxo --whole-utxo
# $cli query ledger-state
# $cli query protocol-state
# $cli query stake-snapshot
# $cli query pool-params
$cli query leadership-schedule \
  --cold-verification-key-file cold.vkey \
  --genesis shelley-genesis.json \
  --vrf-signing-key-file vrf.skey \
  --current
$cli query kes-period-info --op-cert-file opcert.cert
$cli node key-gen-KES \
  --verification-key-file kes.vkey \
  --signing-key-file kes.skey
$cli node issue-op-cert --kes-verification-key-file kes.vkey \
  --cold-signing-key-file cold.skey \
  --operational-certificate-issue-counter-file opcert.counter \
  --kes-period <current kes period> \
  --out-file opcert.cert

echo "SUCCESS"

