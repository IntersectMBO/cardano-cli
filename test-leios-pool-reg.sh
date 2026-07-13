#!/usr/bin/env bash
#
# Acceptance test suite of the leios-enabled pool registration.
# Builds, verifies, signs, and submits a pool-registration transaction
# containing a Leios BLS key against a running local devnet.
set -eo pipefail

# If cardano-cli in PATH lacks the 'dijkstra' subcommand, try to find the
# cabal-built binary and prepend it automatically.
if ! cardano-cli dijkstra --help &>/dev/null 2>&1; then
	CABAL_CLI=$(cabal list-bin cardano-cli 2>/dev/null || true)
	if [ -n "$CABAL_CLI" ] && "$CABAL_CLI" dijkstra --help &>/dev/null 2>&1; then
		export PATH
		PATH="$(dirname "$CABAL_CLI"):$PATH"
		echo "Note: using cabal-built cardano-cli: $CABAL_CLI"
	else
		echo "Error: cardano-cli does not support the 'dijkstra' subcommand."
		echo ""
		echo "Build the local binary and add it to PATH:"
		echo ""
		echo "  export PATH=\$(dirname \$(cabal list-bin cardano-cli)):\$PATH"
		exit 1
	fi
fi

# Check for other required commands
if ! command -v jq &>/dev/null; then
	echo "Error: 'jq' is not available."
	exit 1
fi

# ---------------------------------------------------------------------------
# Devnet configuration
# ---------------------------------------------------------------------------
# Point at a running local devnet.  Override any of these before invoking.
: "${DEVNET_DIR:=/media/nvme/cardano-node/tmp-devnet}"
: "${LEIOS_SOURCE_DIR:=$HOME/git/iog/ouroboros-leios/demo/proto-devnet/config}"
: "${SOCKET_PATH:=$DEVNET_DIR/node1/node.socket}"
: "${TESTNET_MAGIC:=164}"

# The funder pays fees and deposits (stakeAddressDeposit + stakePoolDeposit).
# Defaults to delegator1 from the proto-devnet config (has a stable 30T lovelace UTxO).
# NOTE: The genesis utxo1 UTxO is consumed by the tx-centrifuge on a running devnet.
#       Use one of the delegator keys instead, which retain their UTxOs.
#       Override to use a different funded UTxO explicitly:
#   FUNDER_TX_IN=<txhash>#<ix>  FUNDER_SKEY=path/to/key.skey  ./test-leios-pool-reg.sh
: "${FUNDER_SKEY:=$LEIOS_SOURCE_DIR/stake-delegators/delegator1/payment.skey}"
: "${FUNDER_VKEY:=${FUNDER_SKEY%.skey}.vkey}"
: "${FUNDER_STAKE_VKEY:=$LEIOS_SOURCE_DIR/stake-delegators/delegator1/staking.vkey}"
# Leave FUNDER_TX_IN unset to let the script auto-detect a UTxO.
: "${FUNDER_TX_IN:=}"

: "${WORKING_DIR:=$(pwd)/tmp-test-leios-pool-reg}"
if [ -d "$WORKING_DIR" ]; then
	echo "Working directory already exists: $WORKING_DIR"
	read -r -rp "Remove and re-initialize? (Y/n): " response
	if [[ "$response" =~ ^[Yy]$ || -z "$response" ]]; then
		chmod a+w -R "$WORKING_DIR"
		rm -rf "$WORKING_DIR"
	else
		echo "Aborting."
		exit 0
	fi
fi

mkdir -p "${WORKING_DIR}"
cd "${WORKING_DIR}" || exit 1

# ---------------------------------------------------------------------------
# Key generation
# ---------------------------------------------------------------------------
cardano-cli address key-gen \
	--verification-key-file payment.vkey \
	--signing-key-file payment.skey

cardano-cli dijkstra stake-address key-gen \
	--verification-key-file stake.vkey \
	--signing-key-file stake.skey

cardano-cli node key-gen \
	--cold-verification-key-file cold.vkey \
	--cold-signing-key-file cold.skey \
	--operational-certificate-issue-counter cold.counter

cardano-cli node key-gen-KES \
	--verification-key-file kes.vkey \
	--signing-key-file kes.skey

cardano-cli node key-gen-VRF \
	--verification-key-file vrf.vkey \
	--signing-key-file vrf.skey

chmod 400 vrf.skey

cardano-cli node issue-op-cert \
	--kes-verification-key-file kes.vkey \
	--cold-signing-key-file cold.skey \
	--operational-certificate-issue-counter cold.counter \
	--kes-period 0 \
	--out-file node.cert

cardano-cli dijkstra node key-gen-BLS \
	--verification-key-file bls.vkey \
	--signing-key-file bls.skey

chmod 400 bls.skey

cardano-cli dijkstra node issue-pop-BLS \
	--bls-signing-key-file bls.skey \
	--out-file bls.pop

# ---------------------------------------------------------------------------
# Certificates
# ---------------------------------------------------------------------------
cardano-cli dijkstra stake-pool registration-certificate \
	--cold-verification-key-file cold.vkey \
	--vrf-verification-key-file vrf.vkey \
	--bls-signing-key-file bls.skey \
	--pool-pledge 10000000000 \
	--pool-cost 340000000 \
	--pool-margin 0.01 \
	--pool-reward-account-verification-key-file stake.vkey \
	--pool-owner-stake-verification-key-file stake.vkey \
	--single-host-pool-relay relay1.yourpool.example.com \
	--pool-relay-port 3001 \
	--testnet-magic "$TESTNET_MAGIC" \
	--out-file pool.cert

# Query deposits from the live node
PROTO_PARAMS=$(cardano-cli dijkstra query protocol-parameters \
	--testnet-magic "$TESTNET_MAGIC" \
	--socket-path "$SOCKET_PATH")
STAKE_DEPOSIT=$(echo "$PROTO_PARAMS" | jq '.stakeAddressDeposit')
POOL_DEPOSIT=$(echo "$PROTO_PARAMS" | jq '.stakePoolDeposit')
TOTAL_DEPOSITS=$(( STAKE_DEPOSIT + POOL_DEPOSIT ))

# Combined stake-address registration + delegation in one certificate.
cardano-cli dijkstra stake-address registration-and-delegation-certificate \
	--stake-verification-key-file stake.vkey \
	--cold-verification-key-file cold.vkey \
	--key-reg-deposit-amt "$STAKE_DEPOSIT" \
	--out-file reg-and-deleg.cert

# ---------------------------------------------------------------------------
# Resolve funder UTxO
# ---------------------------------------------------------------------------
FUNDER_ADDR_ARGS=(--payment-verification-key-file "$FUNDER_VKEY" --testnet-magic "$TESTNET_MAGIC")
if [ -n "${FUNDER_STAKE_VKEY:-}" ] && [ -f "$FUNDER_STAKE_VKEY" ]; then
	FUNDER_ADDR_ARGS+=(--stake-verification-key-file "$FUNDER_STAKE_VKEY")
fi
FUNDER_ADDR=$(cardano-cli dijkstra address build "${FUNDER_ADDR_ARGS[@]}")

if [ -z "$FUNDER_TX_IN" ]; then
	echo "Querying UTxOs at funder address: $FUNDER_ADDR"
	FUNDER_UTXO_JSON=$(cardano-cli dijkstra query utxo \
		--address "$FUNDER_ADDR" \
		--testnet-magic "$TESTNET_MAGIC" \
		--socket-path "$SOCKET_PATH" \
		--output-json)
	FUNDER_TX_IN=$(echo "$FUNDER_UTXO_JSON" | jq -r 'to_entries | first | .key // empty')
	FUNDER_VALUE=$(echo "$FUNDER_UTXO_JSON" | jq -r 'to_entries | first | .value.value.lovelace // empty')
	if [ -z "$FUNDER_TX_IN" ]; then
		echo "Error: no UTxO found at funder address $FUNDER_ADDR" >&2
		echo "" >&2
		echo "On the proto-devnet the tx-centrifuge consumes the genesis UTxO." >&2
		echo "Provide a funded UTxO explicitly:" >&2
		echo "  FUNDER_TX_IN=<txhash>#<ix>  FUNDER_SKEY=<key.skey>  $0" >&2
		exit 1
	fi
else
	FUNDER_VALUE=$(cardano-cli dijkstra query utxo \
		--tx-in "$FUNDER_TX_IN" \
		--testnet-magic "$TESTNET_MAGIC" \
		--socket-path "$SOCKET_PATH" \
		--output-json | jq -r 'to_entries | first | .value.value.lovelace // empty')
	if [ -z "$FUNDER_VALUE" ]; then
		echo "Error: UTxO $FUNDER_TX_IN not found on chain" >&2
		exit 1
	fi
fi
echo "Using funder UTxO: $FUNDER_TX_IN ($FUNDER_VALUE lovelace)"

# ---------------------------------------------------------------------------
# Build transaction (two-pass: estimate fee, then build final)
# ---------------------------------------------------------------------------
# Pass 1: draft with fee=0, change = value - deposits (used only for fee estimation)
DRAFT_CHANGE=$(( FUNDER_VALUE - TOTAL_DEPOSITS ))
cardano-cli dijkstra transaction build-raw \
	--tx-in "$FUNDER_TX_IN" \
	--tx-out "${FUNDER_ADDR}+${DRAFT_CHANGE}" \
	--fee 0 \
	--certificate-file pool.cert \
	--certificate-file reg-and-deleg.cert \
	--out-file tx.draft

FEE=$(cardano-cli dijkstra transaction calculate-min-fee \
	--tx-body-file tx.draft \
	--protocol-params-file <(echo "$PROTO_PARAMS") \
	--witness-count 3 \
	--output-json | jq '.fee')

# Pass 2: final tx with correct fee and change
FINAL_CHANGE=$(( FUNDER_VALUE - TOTAL_DEPOSITS - FEE ))
cardano-cli dijkstra transaction build-raw \
	--tx-in "$FUNDER_TX_IN" \
	--tx-out "${FUNDER_ADDR}+${FINAL_CHANGE}" \
	--fee "$FEE" \
	--certificate-file pool.cert \
	--certificate-file reg-and-deleg.cert \
	--out-file tx.raw

# ---------------------------------------------------------------------------
# Verify BLS key is present in the pool params
# ---------------------------------------------------------------------------
pool_params=$(cardano-cli debug transaction view --tx-file tx.raw |
	jq '.certificates[0]."Pool registration"."pool params"')
# leiosKey.leiosPubKey is the raw-bytes hex of the BLS verification key.
# The bls.vkey cborHex is the CBOR-encoded form, which is a short CBOR prefix
# followed by the same raw bytes, so raw bytes are always a substring of cborHex.
actual_bls=$(jq -r '.leiosKey.leiosPubKey' <<<"$pool_params")
expected_bls=$(jq -r '.cborHex' bls.vkey)

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
RESET='\033[0m'

if [[ "$actual_bls" == "null" || -z "$actual_bls" ]]; then
	echo -e "${RED}${BOLD}✗ leiosKey not found in pool params${RESET}" >&2
	echo -e "${BOLD}Full pool params:${RESET}" >&2
	echo "$pool_params" >&2
	exit 1
elif [[ "$expected_bls" == *"$actual_bls"* ]]; then
	echo -e "${GREEN}${BOLD}✓ BLS key in pool registration matches bls.vkey${RESET}"
else
	echo -e "${RED}${BOLD}✗ BLS key mismatch:${RESET}" >&2
	echo -e "  ${YELLOW}expected${RESET} (from bls.vkey cborHex): $expected_bls" >&2
	echo -e "  ${YELLOW}actual${RESET}   (leiosKey.leiosPubKey):  $actual_bls" >&2
	echo -e "${BOLD}Full pool params:${RESET}" >&2
	echo "$pool_params" >&2
	exit 1
fi

# ---------------------------------------------------------------------------
# Sign and submit
# ---------------------------------------------------------------------------
cardano-cli dijkstra transaction sign \
	--tx-file tx.raw \
	--signing-key-file "$FUNDER_SKEY" \
	--signing-key-file stake.skey \
	--signing-key-file cold.skey \
	--testnet-magic "$TESTNET_MAGIC" \
	--out-file tx.signed

TXID=$(cardano-cli dijkstra transaction txid --tx-file tx.signed)

cardano-cli dijkstra transaction submit \
	--tx-file tx.signed \
	--testnet-magic "$TESTNET_MAGIC" \
	--socket-path "$SOCKET_PATH"

echo "Submitted pool registration tx: $TXID"

# ---------------------------------------------------------------------------
# Wait for the pool to appear in the stake-pool set
# ---------------------------------------------------------------------------
POOL_ID=$(cardano-cli dijkstra stake-pool id \
	--cold-verification-key-file cold.vkey \
	--output-bech32)

echo "Waiting for pool $POOL_ID to be registered on-chain ..."
for _ in $(seq 1 60); do
	if cardano-cli dijkstra query stake-pools \
		--testnet-magic "$TESTNET_MAGIC" \
		--socket-path "$SOCKET_PATH" |
		jq -e --arg p "$POOL_ID" 'map(. == $p) | any' >/dev/null 2>&1; then
		echo -e "${GREEN}${BOLD}✓ Pool $POOL_ID registered on-chain${RESET}"
		echo ""
		echo -e "${BOLD}Pool state:${RESET}"
		cardano-cli dijkstra query pool-state \
			--stake-pool-id "$POOL_ID" \
			--testnet-magic "$TESTNET_MAGIC" \
			--socket-path "$SOCKET_PATH" \
			--output-json | jq .
		exit 0
	fi
	sleep 2
done

echo -e "${RED}${BOLD}✗ Pool not seen on-chain after 120 s${RESET}" >&2
exit 1
