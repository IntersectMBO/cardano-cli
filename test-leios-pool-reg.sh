#!/usr/bin/env bash
#
# Acceptance test suite of the leios-enabled pool registration
set -eo pipefail

# Check for required commands
REQUIRED_COMMANDS=(
	"cardano-cli"
)

MISSING_COMMANDS=()
for cmd in "${REQUIRED_COMMANDS[@]}"; do
	if ! command -v "$cmd" &>/dev/null; then
		MISSING_COMMANDS+=("$cmd")
	fi
done

if [ ${#MISSING_COMMANDS[@]} -gt 0 ]; then
	echo "Error: The following required commands are not available:"
	for cmd in "${MISSING_COMMANDS[@]}"; do
		echo "  - $cmd"
	done
	echo ""
	echo "If you want to use the cabal built binary, use:"
	echo ""
	echo 'export PATH=$(dirname $(cabal list-bin cardano-cli)):$PATH'
	exit 1
fi

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

cat >poolMetaData.json <<EOF
{
  "name": "Your Pool Name",
  "description": "Your pool description",
  "ticker": "TICK",
  "homepage": "https://yourpool.example.com"
}
EOF
cardano-cli dijkstra stake-pool metadata-hash \
	--pool-metadata-file poolMetaData.json \
	--out-file poolMetaDataHash.txt

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
	--metadata-url https://YOUR_METADATA_URL \
	--metadata-hash "$(cat poolMetaDataHash.txt)" \
	--out-file pool.cert

cardano-cli dijkstra stake-address stake-delegation-certificate \
	--stake-verification-key-file stake.vkey \
	--cold-verification-key-file cold.vkey \
	--out-file deleg.cert

cardano-cli dijkstra transaction build-raw \
	--tx-in 0000000000000000000000000000000000000000000000000000000000000000#0 \
	--tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \
	--fee 0 \
	--certificate-file pool.cert \
	--certificate-file deleg.cert \
	--out-file tx.raw

pool_params=$(cardano-cli debug transaction view --tx-file tx.raw |
	jq '.certificates[0]."Pool registration"."pool params"')
actual_bls=$(jq -r '.bls' <<<"$pool_params")
expected_bls=$(jq -r '.cborHex' bls.vkey)

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
BOLD='\033[1m'
RESET='\033[0m'

if [[ "$expected_bls" == *"$actual_bls"* ]]; then
	echo -e "${GREEN}${BOLD}✓ BLS key in pool registration matches bls.vkey${RESET}"
else
	echo -e "${RED}${BOLD}✗ BLS key mismatch:${RESET}" >&2
	echo -e "  ${YELLOW}expected${RESET} (from bls.vkey cborHex): $expected_bls" >&2
	echo -e "  ${YELLOW}actual${RESET}   (in pool registration):  $actual_bls" >&2
	echo -e "${BOLD}Full pool params:${RESET}" >&2
	echo "$pool_params" >&2
	exit 1
fi
