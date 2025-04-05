#!/bin/bash

# These fail because its generating a drep id. You could also include this in your 
# reading function for dreps perhaps
drepbech32=$(cardano-cli conway governance drep id --drep-verification-key-file drep.vkey)
echo $drepbech32

drephex=$(cardano-cli conway governance drep id --drep-verification-key-file drep.vkey --output-hex)
echo $drephex

# Works: cabal run cardano-cli -- cip-format cip-129 drep --drep-file drep.vkey --output-text
committeecoldkeyhash=$(cardano-cli conway governance committee key-hash --verification-key-file cc.vkey)
# cardano-cli cip-format cip-129 drep --drep-hex $drephex --output-text

stakevkey=$(cat stake-addr.vkey)
echo "stakevkey: $stakevkey"
stakeaddr=$(cardano-cli conway stake-address build --mainnet --stake-verification-key $stakevkey)
echo "stakeaddr: $stakeaddr"

cardano-cli conway governance action create-info\
  --mainnet\
  --governance-action-deposit 100\
  --deposit-return-stake-address  $stakeaddr \
  --anchor-url "www.whatever.com"\
  --anchor-data-hash "2bcf2a93cb840d72e6fbbad4d52419fa69a3971dee2e32fab414e32a44ecbaf7"\
  --out-file example.action
