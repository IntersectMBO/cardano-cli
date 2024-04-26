#!/bin/bash


base_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# URL of the JSON data
url="https://js.cexplorer.io/api-static/basic/global.json"

# Fetch the JSON data from the URL and extract the "epoch_param" object from the "data" key
json=$(curl -s "$url" | jq '.data.epoch_param')

# Mapping and creating new JSON structure
new_json=$(echo "$json" | jq '{
    protocolVersion: {
        major: (.protocol_major | tonumber),
        minor: (.protocol_minor | tonumber)
    },
    decentralization: (.decentralisation | tonumber),
    extraPraosEntropy: .extra_entropy,
    maxBlockHeaderSize: (.max_bh_size | tonumber),
    maxBlockBodySize: (.max_block_size | tonumber),
    maxTxSize: (.max_tx_size | tonumber),
    txFeeFixed: (.min_fee_a | tonumber),
    txFeePerByte: (.min_fee_b | tonumber),
    minUTxOValue: (.min_utxo_value | tonumber),
    stakeAddressDeposit: (.key_deposit | tonumber),
    stakePoolDeposit: (.pool_deposit | tonumber),
    minPoolCost: (.min_pool_cost | tonumber),
    poolRetireMaxEpoch: (.max_epoch | tonumber),
    stakePoolTargetNum: (.optimal_pool_count | tonumber),
    poolPledgeInfluence: (.influence | tonumber),
    monetaryExpansion: (.monetary_expand_rate | tonumber),
    treasuryCut: (.treasury_growth_rate | tonumber),
    costModels: { unCostModels: .cost_model_id },  # Assuming simple mapping
    executionUnitPrices: {
        priceMemory: (.price_mem | tonumber),
        priceSteps: (.price_step | tonumber)
    },
    maxTxExecutionUnits:  {
        steps: (.max_tx_ex_steps | tonumber),
        memory: (.max_tx_ex_mem | tonumber)
    },
    maxBlockExecutionUnits: {
        steps: (.max_block_ex_mem | tonumber),
        memory: (.max_block_ex_steps | tonumber)
    },
    maxValueSize: (.max_val_size | tonumber),
    collateralPercentage: (.collateral_percent | tonumber),
    maxCollateralInputs: (.max_collateral_inputs | tonumber),
    utxoCostPerByte: (.coins_per_utxo_size | tonumber)
}')

pparamsfp=$base_dir/protocol-parameters.json
echo "$new_json" > "$pparamsfp"

# Uncomment to test that we can decode the pparams file
#dummytxin="423dfc4573c2fb6249bb9bd2e8ae09a624d458efa426806131bef85a389acf4c#0"
#txbodyfp=$base_dir/txbody
#dummychangeaddr="addr1v8yvhgl3pdxs9dkv3t3mfpu7052plpdwl4khatla8ttrkaq2gpmqn"
#
# cardano-cli babbage transaction build-estimate\
#   --shelley-key-witnesses 1\
#   --protocol-params-file "$pparamsfp"\
#   --total-utxo-value 100000000\
#   --tx-in "$dummytxin"\
#   --change-address "$dummychangeaddr"\
#   --tx-out "$dummychangeaddr+5000000"\
#   --out-file "$txbodyfp"

echo "$new_json"