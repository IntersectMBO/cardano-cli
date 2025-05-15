# Changelog for cardano-cli

## 10.9.0.0

- The `--output-cbor` flag has been split to `--output-cbor-bin` and `--output-cbor-hex`.
  The `query protocol-state` command now takes output format flags:
  * `--output-cbor-bin`
  * `--output-cbor-hex`
  * `--output-json`
  * `--output-yaml`
  (breaking, refactoring)
  [PR 1183](https://github.com/IntersectMBO/cardano-cli/pull/1183)

- Upgrade cardano-api-10.16 https://github.com/IntersectMBO/cardano-api/blob/master/cardano-api/CHANGELOG.md#101600
  (compatible)
  [PR 1159](https://github.com/IntersectMBO/cardano-cli/pull/1159)

- - Added build for windows as an artefact for releases
  (feature)
  [PR 1182](https://github.com/IntersectMBO/cardano-cli/pull/1182)

- The following commands have been updated to take output format:
  * `conway query kes-period-info`
  * `conway query ledger-peer-snapshot`
  * `conway query pool-params`
  * `conway query pool-state`
  * `conway query protocol-parameters`
  * `conway query spo-stake-distribution`
  * `conway query stake-address-info`
  * `conway query stake-pool-default-vote`
  * `conway query stake-snapshot`
  * `conway query tip`
  * `conway query tx-mempool`
  * `conway query tx-mempool info`
  * `conway query tx-mempool next-tx`
  * `conway query tx-mempool tx-exists`
  * `latest query kes-period-info`
  * `latest query ledger-peer-snapshot`
  * `latest query pool-params`
  * `latest query pool-state`
  * `latest query protocol-parameters`
  * `latest query spo-stake-distribution`
  * `latest query stake-address-info`
  * `latest query stake-pool-default-vote`
  * `latest query stake-snapshot`
  * `latest query tip`
  * `latest query tx-mempool`
  * `latest query tx-mempool info`
  * `latest query tx-mempool next-tx`
  * `latest query tx-mempool tx-exists`
  * `query kes-period-info`
  * `query ledger-peer-snapshot`
  * `query pool-params`
  * `query pool-state`
  * `query protocol-parameters`
  * `query stake-address-info`
  * `query stake-snapshot`
  * `query tip`
  * `query tx-mempool`
  * `query tx-mempool info`
  * `query tx-mempool next-tx`
  * `query tx-mempool tx-exists`
  (compatible, maintenance)
  [PR 1180](https://github.com/IntersectMBO/cardano-cli/pull/1180)

- Delete `governance` `poll` commands:
  * `compatible babbage governance answer-poll`
  * `compatible babbage governance create-poll`
  * `compatible babbage governance verify-poll`
  (breaking, maintenance)
  [PR 1178](https://github.com/IntersectMBO/cardano-cli/pull/1178)

- Rollout `--output-json` and `--output-yaml` to various commands:
  * `compatible conway governance action view`
  * `compatible conway governance vote view`
  * `conway governance action view`
  * `conway governance vote view`
  * `conway query leadership-schedule`
  * `debug transaction view`
  * `latest governance action view`
  * `latest governance vote view`
  * `latest query leadership-schedule`
  * `query leadership-schedule`
  Ensure we encode json the same way in each command.
  (breaking, maintenance)
  [PR 1175](https://github.com/IntersectMBO/cardano-cli/pull/1175)

- Standard output format for `query ledger-peer-snapshot` command
  Supports `--output-json`, `--output-json-pretty` and `--output-yaml`
  (breaking)
  [PR 1172](https://github.com/IntersectMBO/cardano-cli/pull/1172)

- Consistent output for `query ledger-state` cmd
  (breaking, maintenance)
  [PR 1168](https://github.com/IntersectMBO/cardano-cli/pull/1168)

- Fix incomplete pattern match error in transaction build using byron address
  (bugfix)
  [PR 1167](https://github.com/IntersectMBO/cardano-cli/pull/1167)

- Consistent output for `text-view decode-cbor ` command
  (breaking, refactoring)
  [PR 1164](https://github.com/IntersectMBO/cardano-cli/pull/1164)

- Use lazy ByteString for writing out result of utxo query
  (optimisation)
  [PR 1163](https://github.com/IntersectMBO/cardano-cli/pull/1163)

- Use `Vary` for key output format
  (breaking)
  [PR 1146](https://github.com/IntersectMBO/cardano-cli/pull/1146)

- Added support for stake pool extended keys
  (feature)
  [PR 1091](https://github.com/IntersectMBO/cardano-cli/pull/1091)

- Modified Plutus cost calculation command to allow the user to supply offline data instead of querying the node
  (feature)
  [PR 1079](https://github.com/IntersectMBO/cardano-cli/pull/1079)

- Wrap CLI help based on terminal width
  (feature)
  [PR 1152](https://github.com/IntersectMBO/cardano-cli/pull/1152)

## 10.8.0.0

- Implement changes needed for UTxO-HD Consensus feature.
  - Minor adjust on number of parameters of `AnyNewEpochState`.

  Update to latest `cardano-api 10.14.0.0`.
  (feature, compatible)
  [PR 1134](https://github.com/IntersectMBO/cardano-cli/pull/1134)

- Corrected the flipped text & JSON output format for `query stake-pools` command.
  (bugfix)
  [PR 1139](https://github.com/IntersectMBO/cardano-cli/pull/1139)

- Make the output format flag for the `query utxo` command only have one default: JSON (rather than a different default depending on whether the output file is specified).
  (breaking, refactoring)
  [PR 1133](https://github.com/IntersectMBO/cardano-cli/pull/1133)

- Add canonical CBOR output toggle for transaction building and signing commands.
  (feature, compatible)
  [PR 1092](https://github.com/IntersectMBO/cardano-cli/pull/1092)

## 10.7.0.0

- Bump cardano-api to include bug fixes for:
  -  Silently dropping simple scripts in transaction construction
  -  Not selecting the highest protocol version in a given era. This resulted in erroneous plutus script decoding failures.

  (compatible, bugfix)
  [PR 1127](https://github.com/IntersectMBO/cardano-cli/pull/1127)

- Bump to cardano-api-10.13.0.0 - Fix bug in the construction of the redeemer pointer map
  (bugfix)
  [PR 1124](https://github.com/IntersectMBO/cardano-cli/pull/1124)

- Delete top level `babbage` command group
  (breaking)
  [PR 1117](https://github.com/IntersectMBO/cardano-cli/pull/1117)

## 10.6.0.0

- Source import of the `vary` package
  (feature)
  [PR 1121](https://github.com/IntersectMBO/cardano-cli/pull/1121)

- Bumped ledger and dependencies for node 10.3 release.
    * Removed use of parameterised crypto (`EraCrypto c`, this enables many other data types to become mono-morphic over `StandardCrypto`)
    * Added `query stake-pool-default-vote`
    * Ensured security parameter is non-zero
  (breaking, feature)
  [PR 1075](https://github.com/IntersectMBO/cardano-cli/pull/1075)

- Add a new query for stake-pool default vote
  (feature, release)
  [PR 1081](https://github.com/IntersectMBO/cardano-cli/pull/1081)

- Delete top-level `shelley` command group and delete associated tests
  (breaking)
  [PR 1111](https://github.com/IntersectMBO/cardano-cli/pull/1111)

- Parallelised help golden test generation and validation
  (optimisation)
  [PR 1103](https://github.com/IntersectMBO/cardano-cli/pull/1103)

## 10.5.0.0

- Added support for mnemonic sentence generation and extended key derivation from mnemonic sentences.
  (feature)
  [PR 975](https://github.com/IntersectMBO/cardano-cli/pull/975)

- Remove minting capabilities in eras prior to Mary.
  Change minted assets representation to `L.MultiAsset` instead of `Value`.
  (compatible)
  [PR 1085](https://github.com/IntersectMBO/cardano-cli/pull/1085)

- Update [cardano-api-10.10.0.0](https://github.com/IntersectMBO/cardano-api/blob/master/cardano-api/CHANGELOG.md#101000)
  Fix signing of a transaction in `compatible shelley transaction signed-transaction` command. Previously two different transaction bodies were used for the resulting transaction and the signature - now it's used the same for both purposes.
  (bugfix)
  [PR 1057](https://github.com/IntersectMBO/cardano-cli/pull/1057)

- Add stake address registration and delegation certificate and stake pool delegation certificate to compatible commands
  (compatible)
  [PR 1070](https://github.com/IntersectMBO/cardano-cli/pull/1070)

- Default to hex for binary query utxo output
  (feature, breaking)
  [PR 1066](https://github.com/IntersectMBO/cardano-cli/pull/1066)

- Add command to calculate plutus script costs from an already constructed transaction.
  (feature)
  [PR 1031](https://github.com/IntersectMBO/cardano-cli/pull/1031)

- Add binary output option for query utxo command
  (feature)
  [PR 1000](https://github.com/IntersectMBO/cardano-cli/pull/1000)

- Add `cardano-cli conway query future-pparams`
  (feature, compatible)
  [PR 1038](https://github.com/IntersectMBO/cardano-cli/pull/1038)

- Add governance action deposits to the output of `query stake-address-info`.  This also renames the field stakeDeposit to stakeRegistrationDeposit in the JSON output.
  (feature, breaking)
  [PR 1032](https://github.com/IntersectMBO/cardano-cli/pull/1032)

- Add the ratify-state query
  (feature, compatible)
  [PR 1036](https://github.com/IntersectMBO/cardano-cli/pull/1036)

- Fix costs calculation for transaction with more than one certificates with the same stake credential and script witness.
  (bugfix)
  [PR 1028](https://github.com/IntersectMBO/cardano-cli/pull/1028)

- Add certificates to CLI interface in `compatible transaction-sign`
  (feature, compatible)
  [PR 972](https://github.com/IntersectMBO/cardano-cli/pull/972)

## 10.4.0.0

- Add `cardano-cli conway query future-pparams`
  (feature, compatible)
  [PR 1038](https://github.com/IntersectMBO/cardano-cli/pull/1038)

- Add governance action deposits to the output of `query stake-address-info`.  This also renames the field stakeDeposit to stakeRegistrationDeposit in the JSON output.
  (feature, breaking)
  [PR 1032](https://github.com/IntersectMBO/cardano-cli/pull/1032)

- Add the ratify-state query
  (feature, compatible)
  [PR 1036](https://github.com/IntersectMBO/cardano-cli/pull/1036)

- Fix costs calculation for transaction with more than one certificates with the same stake credential and script witness.
  (bugfix)
  [PR 1028](https://github.com/IntersectMBO/cardano-cli/pull/1028)

- Add certificates to CLI interface in `compatible transaction-sign`
  (feature, compatible)
  [PR 972](https://github.com/IntersectMBO/cardano-cli/pull/972)

## 10.3.0.0

- Add QueryLedgerPeerSnapshotCmd for a snapshot of big ledger peers used when syncing in Genesis
  (feature)
  [PR 727](https://github.com/IntersectMBO/cardano-cli/pull/727)

- Integrate `plutus`, `cardano-ledger`, `ouroboros-network`, `ouroboros-consensus`, `cardano-api-10.6.0.0`
  (breaking, feature)
  [PR 986](https://github.com/IntersectMBO/cardano-cli/pull/986)

- transaction id: add --output-[json,text] flag to control format of the output
  (feature, compatible)
  [PR 1005](https://github.com/IntersectMBO/cardano-cli/pull/1005)

- drep id: have --output-bech32 and --output-hex instead of --output-format STRING
  (breaking)
  [PR 1017](https://github.com/IntersectMBO/cardano-cli/pull/1017)

- drep id: support key hash as input
  (compatible)
  [PR 1009](https://github.com/IntersectMBO/cardano-cli/pull/1009)

- Added datums and scripts to `friendlyTxImpl`, which translates into them being showed by `transaction view`.
  (feature)
  [PR 977](https://github.com/IntersectMBO/cardano-cli/pull/977)

- Disambiguate DRep being a key or a script in certificate descriptions
  (breaking)
  [PR 1007](https://github.com/IntersectMBO/cardano-cli/pull/1007)

- Moved `genesis hash` command to `hash genesis-file`
  (breaking)
  [PR 982](https://github.com/IntersectMBO/cardano-cli/pull/982)

## 10.2.0.0

- Augment of query spo-stake-distribution to include the DRep delegation choices of the Pool's rewards accounts
  (breaking)
  [PR 990](https://github.com/IntersectMBO/cardano-cli/pull/990)

- Implement the `query proposals` command
  (feature, compatible)
  [PR 984](https://github.com/IntersectMBO/cardano-cli/pull/984)

- More fine grained controls of eras for create-testnet-data. Forbid creating DReps and CC in babbage.

  Remove the `create-testnet-data` altogether in eras earlier than Babbage
  (breaking)
  [PR 968](https://github.com/IntersectMBO/cardano-cli/pull/968)

- Update API to 10.4.0.0
  (breaking)
  [PR 988](https://github.com/IntersectMBO/cardano-cli/pull/988)

- Modified anchor-data checking to allow HTTP schema for testing purposes
  (feature)
  [PR 979](https://github.com/IntersectMBO/cardano-cli/pull/979)

- Minting script witness refactor
  The type `ScriptWitnessFiles` makes it difficult to accommodate for specific changes to plutus scripts.
  As a result we introduce `MintScriptWitnessWithPolicyId` as a first step towards deprecating `ScriptWitnessFiles`.
  This paves the way to more readable code and allows us to introduce specific changes to the different script types i.e simple vs plutus.
  (breaking, refactoring)
  [PR 971](https://github.com/IntersectMBO/cardano-cli/pull/971)

- create-testnet-data: create byron genesis
  (breaking, test)
  [PR 974](https://github.com/IntersectMBO/cardano-cli/pull/974)

- `transaction build` now checks and fails if stake addresses used for deposit return or treasury withdrawals in proposals are NOT registered on-chain.
  (feature)
  [PR 963](https://github.com/IntersectMBO/cardano-cli/pull/963)

- transaction submit: print transaction hash, like this:

  Transaction successfully submitted. Transaction hash is:
  {"txhash":"456c614d5d547b7fe197a4d18fbb86e086cb9080594dabf9059adf08b00cf2bd"}

  Previously it was:

  Transaction successfully submitted.
  (feature, breaking)
  [PR 925](https://github.com/IntersectMBO/cardano-cli/pull/925)

- Add option --committee-keys to `create-testnet-data` to create the cold and hot credential for constitutional committee members
  (compatible)
  [PR 961](https://github.com/IntersectMBO/cardano-cli/pull/961)

- Added anchor data hash checks to `transaction build`
  (feature)
  [PR 951](https://github.com/IntersectMBO/cardano-cli/pull/951)

- Update cardano-api to [10.2.0.0](https://github.com/IntersectMBO/cardano-api/releases/tag/cardano-api-10.2.0.0)
  (breaking)
  [PR 967](https://github.com/IntersectMBO/cardano-cli/pull/967)

- Add the `debug check-node-configuration --node-configuration-file node-config.json` command
  that reads `node-config.json` and checks that the hashes of genesis files are correct.
  (feature)
  [PR 923](https://github.com/IntersectMBO/cardano-cli/pull/923)

## 10.1.1.0

- Restore stable query cmds
  (bugfix)
  [PR 955](https://github.com/IntersectMBO/cardano-cli/pull/955)

## 10.1.0.0

- Upgrade CHaP (include `cardano-ledger-conway-1.17.1.0`)
  (compatible)
  [PR 949](https://github.com/IntersectMBO/cardano-cli/pull/949)

- Fixed number of `DRep` credentials generated by `create-testnet-data`.
  (bugfix)
  [PR 948](https://github.com/IntersectMBO/cardano-cli/pull/948)

- Introduce "compatible" command group
  (feature, compatible)
  [PR 917](https://github.com/IntersectMBO/cardano-cli/pull/917)

- Upgrade cardano-api-10.1.0.0 [CHANGELOG](https://github.com/IntersectMBO/cardano-api/blob/main/cardano-api/CHANGELOG.md#10100)
  (breaking)
  [PR 946](https://github.com/IntersectMBO/cardano-cli/pull/946)

- Added support for the new envelope types for transactions: "TxSignedShelley", "Tx AllegraEra", "Tx MaryEra", "Tx AlonzoEra", "Tx BabbageEra", "Tx ConwayEra".
  The following old envelope types are no longer supported: "Witnessed Tx ShelleyEra", "Witnessed Tx AllegraEra", "Witnessed Tx MaryEra", "Witnessed Tx AlonzoEra", "Witnessed Tx BabbageEra", "Witnessed Tx ConwayEra", "Unwitnessed Tx ByronEra", "Unwitnessed Tx ShelleyEra", "Unwitnessed Tx AllegraEra", "Unwitnessed Tx MaryEra", "Unwitnessed Tx AlonzoEra", "Unwitnessed Tx BabbageEra", "Unwitnessed Tx ConwayEra"
  (bugfix, breaking)
  [PR 892](https://github.com/IntersectMBO/cardano-cli/pull/892)

## 10.0.0.0

- Integrates updated ledger type and fixes test due to new plutus ops
  Update to cardano-api-10.0 [CHANGELOG](https://github.com/IntersectMBO/cardano-api/blob/main/cardano-api/CHANGELOG.md#10000)
  (feature, breaking, test, maintenance)
  [PR 940](https://github.com/IntersectMBO/cardano-cli/pull/940)

- Added hash checks for `governance committee create-cold-key-resignation-certificate` and `governance vote create`
  (feature, test)
  [PR 937](https://github.com/IntersectMBO/cardano-cli/pull/937)

- Update cardano-api to 9.4.0.0
  (breaking, release)
  [PR 936](https://github.com/IntersectMBO/cardano-cli/pull/936)

- Reverts https://github.com/IntersectMBO/cardano-cli/pull/908
  (breaking)
  [PR 930](https://github.com/IntersectMBO/cardano-cli/pull/930)

- Move address commands to top level
  (feature, compatible)
  [PR 934](https://github.com/IntersectMBO/cardano-cli/pull/934)

- Added metadata, metadata hash validation and URL support to `stake-pool registration-certificate`; and hash checking and URL support to `stake-pool metadata-hash`.
  (feature, test)
  [PR 932](https://github.com/IntersectMBO/cardano-cli/pull/932)

- Move key commands to top level as they are era agnostic
  (feature, compatible)
  [PR 931](https://github.com/IntersectMBO/cardano-cli/pull/931)

- Added URL support to `governance drep metadata-hash`
  (feature)
  [PR 927](https://github.com/IntersectMBO/cardano-cli/pull/927)

- The node commands are era agnostic and should be moved to the top level
  (feature, compatible)
  [PR 929](https://github.com/IntersectMBO/cardano-cli/pull/929)

- Added hash checks for `drep registration-certificate` and `drep update-certificate`
  (feature, breaking, test)
  [PR 916](https://github.com/IntersectMBO/cardano-cli/pull/916)

- Implement cddl's `stake_reg_deleg_cert` and `vote_reg_deleg_cert` and `stake_vote_reg_deleg_cert` as:
  - cardano-cli conway stake-address registration-and-delegation-certificate
  - cardano-cli conway stake-address registration-and-vote-delegation-certificate
  - cardano-cli conway stake-address registration-stake-and-vote-delegation-certificate
  (feature)
  [PR 919](https://github.com/IntersectMBO/cardano-cli/pull/919)

- `create-testnet-data` now takes a node configuration file as input. If a file is given, it is augmented with the hashes (and paths) of the genesis files, or if the hashes/paths are present; they are checked.
  (feature, breaking)
  [PR 908](https://github.com/IntersectMBO/cardano-cli/pull/908)

- Large deprecation of legacy commands and removal of related code.
  (breaking)
  [PR 905](https://github.com/IntersectMBO/cardano-cli/pull/905)

- Add anchor data hash checks to all `governance action` commands
  (feature, breaking, test)
  [PR 915](https://github.com/IntersectMBO/cardano-cli/pull/915)

- Adding support for script stake credentials and stake address in `create-treasury-withdrawal` and improving help text. New options are: `--funds-receiving-stake-script-file` and `--funds-receiving-stake-address`
  (feature, documentation)
  [PR 914](https://github.com/IntersectMBO/cardano-cli/pull/914)

- Add proposal hash check when creating `info` governance action
  (feature, breaking)
  [PR 910](https://github.com/IntersectMBO/cardano-cli/pull/910)

- Remove `era transaction view` and `transaction view` commands. Use `debug transaction view` instead.
  (breaking)
  [PR 868](https://github.com/IntersectMBO/cardano-cli/pull/868)

- Remove eras older than Babbage support in `transaction build` and `transaction build-estimate`
  (breaking, refactoring)
  [PR 878](https://github.com/IntersectMBO/cardano-cli/pull/878)

- Added hash validation and support for HTTP(S) and IPFS to command `hash anchor-data`
  (feature)
  [PR 895](https://github.com/IntersectMBO/cardano-cli/pull/895)

- Show user friendly warning when `cardano ping` command is misconfigured.
  (feature)
  [PR 893](https://github.com/IntersectMBO/cardano-cli/pull/893)

## 9.4.1.0

- Add support for fetching tip through cardano-ping-0.4.0.2
  Added NodeToClientVersionV17
  (feature, compatible)
  [PR 888](https://github.com/IntersectMBO/cardano-cli/pull/888)

## 9.4.0.0

- add support for script based drep on update certificate
  (feature)
  [PR 884](https://github.com/IntersectMBO/cardano-cli/pull/884)

- Bump CHaP
  Propagate experimental api to `transaction build` internals
  Deprecate pre-Conway eras for `transaction build`
  (breaking)
  [PR 853](https://github.com/IntersectMBO/cardano-cli/pull/853)

- Add for eras <= Babbage a deprecation notice.
  (feature, breaking)
  [PR 879](https://github.com/IntersectMBO/cardano-cli/pull/879)

- Make funds receiving address and amount mandatory.
  (bugfix)
  [PR 877](https://github.com/IntersectMBO/cardano-cli/pull/877)

- create-testnet-data's --total-supply option doesn't have a default anymore. The default value is to take the value from the shelley genesis file (if provided, otherwise this file is defaulted, so total supply comes from the default shelley genesis). create-testnet-data's --delegated-supply option doesn't have a default anymore. The default is to use half of the total supply.
  (breaking, bugfix)
  [PR 874](https://github.com/IntersectMBO/cardano-cli/pull/874)

- Forbid incorrect values in parsers of many Int-like options. Previously those values would overflow and be turned into a random valid value. If this breaks your use case, this means your use case wasn't doing what you expected.
  (bugfix)
  [PR 864](https://github.com/IntersectMBO/cardano-cli/pull/864)

- Fix estimated fee rendering: previously the output was of the form `Estimated transaction fee: Coin 357154`.
  Now it is: `Estimated transaction fee: 357154 Lovelace`
  (bugfix)
  [PR 873](https://github.com/IntersectMBO/cardano-cli/pull/873)

- Modify `transaction view` to show what inputs redeemers refer to.
  (feature)
  [PR 861](https://github.com/IntersectMBO/cardano-cli/pull/861)

## 9.3.0.0

- Add `query spo-stake-distribution` command to obtain stake distribution for SPOs
  (feature)
  [PR 854](https://github.com/IntersectMBO/cardano-cli/pull/854)

- Remove redundant voting and proposal procedures functions.
  Update cardano-api-9.2.0.0.
  (compatible, refactoring)
  [PR 856](https://github.com/IntersectMBO/cardano-cli/pull/856)

- Create toJSON instance for `query drep-state` output. Haskell users can know use this type to parse back `query drep-state`'s output to a Haskell value automatically.
  (feature, compatible)
  [PR 863](https://github.com/IntersectMBO/cardano-cli/pull/863)

- Make "[era] transaction view" command fail. Please use "debug transaction view" instead.
  (breaking)
  [PR 858](https://github.com/IntersectMBO/cardano-cli/pull/858)

- Fix invalid sync percentage display in `query tip`
  (compatible, bugfix)
  [PR 851](https://github.com/IntersectMBO/cardano-cli/pull/851)

## 9.2.1.0

- Add "query treasury" command
  (feature, compatible)
  [PR 845](https://github.com/IntersectMBO/cardano-cli/pull/845)

- Relax requirement of supplying datums to plutus spending scripts
  Add flags to enable use of reference scripts with voting and proposing scripts
    - `--vote-tx-in-reference`
    - `--proposal-tx-in-reference`
  (feature, compatible)
  [PR 822](https://github.com/IntersectMBO/cardano-cli/pull/822)

## 9.2.0.0

- Make `genesis`: `create`, `create-staked` and `create-cardano` commands accept optional era parameters.
  (feature, compatible)
  [PR 812](https://github.com/IntersectMBO/cardano-cli/pull/812)

- Remove check of Drep metadata size, always return the hash of the file passed by the user. This to be compatible with CIP119.
  Upgrade cardano-api-9.1.0.0
  (feature, breaking)
  [PR 818](https://github.com/IntersectMBO/cardano-cli/pull/818)

## 9.1.0.0

- Add era-independent "debug transaction view" command
  (breaking)
  [PR 840](https://github.com/IntersectMBO/cardano-cli/pull/840)

- Add output options flags to calculate-min-fee
  (feature, compatible)
  [PR 824](https://github.com/IntersectMBO/cardano-cli/pull/824)

- Fix commands' descriptions referencing old 'governance hash' command
  (bugfix)
  [PR 821](https://github.com/IntersectMBO/cardano-cli/pull/821)

## 9.0.0.1

- transaction-build and build-estimate: include current treasury value only if a donation is being done
  (breaking)
  [PR 826](https://github.com/IntersectMBO/cardano-cli/pull/826)

- Fix typo in committee help and error messages
  (compatible)
  [PR 820](https://github.com/IntersectMBO/cardano-cli/pull/820)

## 9.0.0.0

- Add --hot-script-hash option to committee create-hot-key-authorization-certificate subcommand
  (breaking)
  [PR 806](https://github.com/IntersectMBO/cardano-cli/pull/806)

- Move "conway governance hash" commands to "hash". Users should adapt their calls as follows:

  `cardano-cli conway governance hash anchor-data ...` becomes `cardano-cli hash anchor-data ...`
  `cardano-cli conway governance hash script ...` becomes `cardano-cli hash script ...`
  (breaking)
  [PR 787](https://github.com/IntersectMBO/cardano-cli/pull/787)

## 8.25.0.0

- Add --current-treasury-value and --treasury-donation to transaction build and friends
  (feature, breaking)
  [PR 778](https://github.com/IntersectMBO/cardano-cli/pull/778)

- Update cardano-api to 8.49.0.0
  (compatible)
  [PR 804](https://github.com/IntersectMBO/cardano-cli/pull/804)

- Make `--fee` mandatory in legacy transaction build-raw
  (breaking, bugfix)
  [PR 797](https://github.com/IntersectMBO/cardano-cli/pull/797)

## 8.24.0.0

- Pre-integration for cardano-node 8.12 release
  `cardano-cli transaction build ... --calculate-plutus-script-cost` now returns the execution logs of the scripts
  (breaking)
  [PR 789](https://github.com/IntersectMBO/cardano-cli/pull/789)

- New `debug log-epoch-state` command
  (feature, compatible, refactoring)
  [PR 775](https://github.com/IntersectMBO/cardano-cli/pull/775)

- transaction view: show proposals and votes

  This adds to new fields to the JSON output of `transaction view`: `governance actions` and `voters`.
  Please see https://github.com/IntersectMBO/cardano-cli/pull/774#issuecomment-2139062455 for example outputs.
  When those fields are irrelevant, they are present but there value is `null`.
  (compatible)
  [PR 774](https://github.com/IntersectMBO/cardano-cli/pull/774)

- query protocol-parameters: use ledger JSON encoding, not API one
  (breaking)
  [PR 758](https://github.com/IntersectMBO/cardano-cli/pull/758)

- Make `--fee` mandatory in `transaction build-raw`. Remove `TxCmdTxFeeValidationError` type.
  (breaking, bugfix)
  [PR 768](https://github.com/IntersectMBO/cardano-cli/pull/768)

- Remove UnwitnessedCliFormattedTxBody constructor
  (improvement)
  [PR 707](https://github.com/IntersectMBO/cardano-cli/pull/707)

## 8.23.1.0

- Make `--prev-governance-action-tx-id` and `--prev-governance-action-tx-id` optional for `create-no-confidence` command
  (feature, compatible)
  [PR 753](https://github.com/IntersectMBO/cardano-cli/pull/753)

## 8.23.0.0

- Introduces the `governance action create-hardfork` cmd.
  (feature, test)
  [PR 746](https://github.com/IntersectMBO/cardano-cli/pull/746)

- Export friendly function that returns ByteString + export buildShelleyAddress
  (compatible)
  [PR 748](https://github.com/IntersectMBO/cardano-cli/pull/748)

- Export toTxOutInAnyEra
  (compatible)
  [PR 745](https://github.com/IntersectMBO/cardano-cli/pull/745)

- Enable manual specification of execution units for plutus voting and proposing scripts
  (feature, breaking)
  [PR 744](https://github.com/IntersectMBO/cardano-cli/pull/744)

- query stake-distribution: add --output-{json,text} flags
  (feature, breaking)
  [PR 743](https://github.com/IntersectMBO/cardano-cli/pull/743)

- Introduce new `cardano-cli latest transaction build-estimate` command which will
  produce a balanced transaction body without requiring a connection to a live node
  (feature)
  [PR 728](https://github.com/IntersectMBO/cardano-cli/pull/728)

- Add `minFeeRefScriptCostPerByte`  to create-protocol-parameter-update governance action command.
  (feature)
  [PR 736](https://github.com/IntersectMBO/cardano-cli/pull/736)

- Fixed misunderstanding in generated README file by `genesis create-testnet-data`.
  (bugfix)
  [PR 726](https://github.com/IntersectMBO/cardano-cli/pull/726)

- conway governance committe key-hash: support extended CC keys
  (feature, compatible, bugfix)
  [PR 717](https://github.com/IntersectMBO/cardano-cli/pull/717)

- Defaulted `calculate-min-fee --reference-script-size`'s value to `0`
  (improvement)
  [PR 716](https://github.com/IntersectMBO/cardano-cli/pull/716)

- Enable both `--cold-script-file` and `--cold-script-hash` for `governance committee create-hot-key-authorization-certificate` and `committee create-cold-key-resignation-certificate`
  (feature, compatible)
  [PR 699](https://github.com/IntersectMBO/cardano-cli/pull/699)

- Fixed `--include-stake` behavior in `query drep-state`
  (breaking, bugfix)
  [PR 698](https://github.com/IntersectMBO/cardano-cli/pull/698)

## 8.22.0.0

- Add `ref-script-size` query command
  (feature, compatible)
  [PR 672](https://github.com/IntersectMBO/cardano-cli/pull/672)

- convert-cardano-address-key: support DRep and CC keys
  (feature, compatible)
  [PR 691](https://github.com/IntersectMBO/cardano-cli/pull/691)

- - New required flag `--reference-script-size` for `transaction calculate-min-fee`
  - Update cardano-api-8.44 https://github.com/IntersectMBO/cardano-api/blob/main/cardano-api/CHANGELOG.md#84400
  - Update ouroboros-consensus-0.17, ouroboros-consensus-cardano-0.15, ouroboros-consensus-protocol-0.8, cardano-ledger-shelley-1.10, cardano-ledger-byron-1.0.1
  (feature, breaking)
  [PR 680](https://github.com/IntersectMBO/cardano-cli/pull/680)

- Fix protocol params order for minFeeA and minFeeB
  (bugfix)
  [PR 693](https://github.com/IntersectMBO/cardano-cli/pull/693)

- Update CLI to use `threshold` instead of `quorum` for constitutional committee.
  (feature, breaking)
  [PR 689](https://github.com/IntersectMBO/cardano-cli/pull/689)

- Add info about redeemers to output of `transaction view` command.
  (feature)
  [PR 664](https://github.com/IntersectMBO/cardano-cli/pull/664)

- Add support for querying drep state and stake distribution by script hash
  (feature)
  [PR 666](https://github.com/IntersectMBO/cardano-cli/pull/666)

- transaction build{,-raw}: throw an error on identical action-ids in votes files
  (breaking, bugfix)
  [PR 681](https://github.com/IntersectMBO/cardano-cli/pull/681)

- Add `--drep-script-hash` parameter to `conway governance drep retirement-certificate`
  (feature)
  [PR 678](https://github.com/IntersectMBO/cardano-cli/pull/678)

## 8.21.0.0

- vote create: support DRep and CC script hash
  (feature, compatible)
  [PR 665](https://github.com/IntersectMBO/cardano-cli/pull/665)

- Add Plutus script hash support in `update-committee`, `overnance committee create-cold-key-resignation-certificate` and `query committee-state` commands.
  (feature, improvement)
  [PR 658](https://github.com/IntersectMBO/cardano-cli/pull/658)

- verification-key: support DRep keys as well as committee keys, extended or not
  (feature, compatible, bugfix)
  [PR 652](https://github.com/IntersectMBO/cardano-cli/pull/652)

- Fix calculate-min-fee by switching to use Cardano.Api.evaluateTransactionFee
  instead of the deprecated Cardano.Api.estimateTransactionFee.
  This also deprecates the --mainnet, --testnet-magic, --tx-in-count,
  and --tx-out-count command args, which are no longer necessary.
  They can still be provided, but have no effect.
  (feature, improvement, bugfix, test)
  [PR 642](https://github.com/IntersectMBO/cardano-cli/pull/642)

- Modified `create-testnet-data` option so that it registers DReps generated and delegates stake delegators to them. Also introduced transient drep delegation and refactored existing code so that more is reused.
  (feature, improvement)
  [PR 646](https://github.com/IntersectMBO/cardano-cli/pull/646)

- Extend committee `create-hot-key-authorization-certificate` to support scripts
  (feature, compatible, improvement)
  [PR 641](https://github.com/IntersectMBO/cardano-cli/pull/641)

- query leadership: add --output-[json,text] flag to control format of the output. Previous behavior is preserved (write text to stdout, write json to file)
  (feature, compatible)
  [PR 649](https://github.com/IntersectMBO/cardano-cli/pull/649)

- Update [cardano-api-8.40.0.0](https://github.com/IntersectMBO/cardano-api/blob/main/cardano-api/CHANGELOG.md#84000)
  (improvement)
  [PR 648](https://github.com/IntersectMBO/cardano-cli/pull/648)

- create-testnet-data: fix that treasury could be unexpectedly negative
  (breaking, improvement, bugfix)
  [PR 644](https://github.com/IntersectMBO/cardano-cli/pull/644)

- create-testnet-data: fixes that amount of delegated coins was incorrect
  (breaking, bugfix)
  [PR 638](https://github.com/IntersectMBO/cardano-cli/pull/638)

- Allow an output file to be specified for the various versions of query pool-state
  (compatible, improvement)
  [PR 635](https://github.com/IntersectMBO/cardano-cli/pull/635)

- create-testnet-data: allow to specify relays for SPOs (like create-staked)
  (feature, compatible)
  [PR 632](https://github.com/IntersectMBO/cardano-cli/pull/632)

- Bump cardano-ping version
  (compatible)
  [PR 633](https://github.com/IntersectMBO/cardano-cli/pull/633)

## 8.20.3.0

- Update cardano-api-8.39.2, ouroboros-consensus-0.16, ouroboros-consensus-cardano-0.14
  (compatible)
  [PR 615](https://github.com/IntersectMBO/cardano-cli/pull/615)

## 8.20.2.0

- Enable deposit return script addresses
  Enable constitutional scripts
  Bump to cardano-api-8.39.1.0
  (feature, compatible, bugfix)
  [PR 609](https://github.com/IntersectMBO/cardano-cli/pull/609)

- Add missing newlines to `cardano-cli query utxo` text output.
  (bugfix)
  [PR 620](https://github.com/IntersectMBO/cardano-cli/pull/620)

- Update [cardano-api-8.39.0.0](https://github.com/IntersectMBO/cardano-api/blob/main/cardano-api/CHANGELOG.md#83900).
  Removed direct imports from `Cardano.Ledger`.
  (improvement)
  [PR 558](https://github.com/IntersectMBO/cardano-cli/pull/558)

- query stake-pools, add --output-[json,text] flag to control format of the output
  (feature, compatible)
  [PR 617](https://github.com/IntersectMBO/cardano-cli/pull/617)

- Upgrade hedgehog-extras to 0.6.1.0
  (breaking, maintenance)
  [PR 613](https://github.com/IntersectMBO/cardano-cli/pull/613)

- query utxo: add --output-[json,text] flag to control format of the output. Previous behavior is preserved (write text to stdout, write json to file)
  (feature, compatible)
  [PR 611](https://github.com/IntersectMBO/cardano-cli/pull/611)

- Fixed git revision showed by --version flag when built using nix
  (bugfix)
  [PR 610](https://github.com/IntersectMBO/cardano-cli/pull/610)

## 8.20.1.0

- Fix `create-testnet-data` creating negative supply
  (bugfix)
  [PR 599](https://github.com/IntersectMBO/cardano-cli/pull/599)

- Add the `drep update-certificate` command
  (feature, compatible)
  [PR 561](https://github.com/IntersectMBO/cardano-cli/pull/561)

- Add `--include-stake` flag to obtain the stake in the `drep-state` query
  (compatible)
  [PR 557](https://github.com/IntersectMBO/cardano-cli/pull/557)

- Allow users to specify the target in conway queries (either immutable tip or volatile tip)
  (feature)
  [PR 603](https://github.com/IntersectMBO/cardano-cli/pull/603)

- Fix error messages as support for cardano-cli serialisation format was already removed.
  (improvement, test)
  [PR 605](https://github.com/IntersectMBO/cardano-cli/pull/605)

- This PR improves `create-testnet-data` cmd so that it takes testnet magic either from template file or from the  CLI flag `--testnet-magic`, when used.
  (improvement)
  [PR 595](https://github.com/IntersectMBO/cardano-cli/pull/595)

- Make committee keys able to sign transactions
  (feature, compatible)
  [PR 596](https://github.com/IntersectMBO/cardano-cli/pull/596)


## 8.20.0.0

- - Bump CHaP in preparation for cardano-node 8.8
  - `--pool-retirement-epoch-boundary` updated to `--pool-retirement-epoch-interval`
  - `--governance-action-lifetime` and  `--drep-activity` parse an `EpochInterval`
  - Treasury withdrawals and protocol parameter updates now optionally parse a guard rail script
  - `transaction build` and `transaction build-raw` commands now parse optional Plutus script witnesses for votes and proposals
  - The "tx body intermediate format" is now fully deprecated
  - Protocol parameter parsers are updated as follows:
    - `pMaxBodySize :: Parser Word32`
    - `pMaxBlockHeaderSize :: Parser Word32`
    - `pCommitteeTermLength :: Parser EpochInterval`
  - New `--pool-voting-threshold-pp-security-group` parser added for protocol parameter updates in the Conway era
  (breaking)
  [PR 529](https://github.com/IntersectMBO/cardano-cli/pull/529)

- create-testnet-data: better UX for supply arguments: have a flag for total supply and delegated supply is a fraction of that.
  (breaking)
  [PR 581](https://github.com/IntersectMBO/cardano-cli/pull/581)

- Provide more detailed error upon missing policy id
  (improvement)
  [PR 527](https://github.com/IntersectMBO/cardano-cli/pull/527)

- Bump hedgehog-extras to 0.6.0.1 to benefit of this fix: https://github.com/input-output-hk/hedgehog-extras/pull/58
  (compatible, test, release)
  [PR 589](https://github.com/IntersectMBO/cardano-cli/pull/589)

- Avoid an internal crash of `create-testnet-data`
  (compatible, bugfix)
  [PR 588](https://github.com/IntersectMBO/cardano-cli/pull/588)

- Update help for `drep-stake-distribution` and `drep-state` queries
  (improvement, test)
  [PR 585](https://github.com/IntersectMBO/cardano-cli/pull/585)

- Improvements to --create-testnet-data
  (compatible, test)
  [PR 575](https://github.com/IntersectMBO/cardano-cli/pull/575)

- Support signing with drep extended key
  (feature, compatible)
  [PR 556](https://github.com/IntersectMBO/cardano-cli/pull/556)

## 8.19.0.0

- Update cardano-api 8.37.1
  (compatible)
  [PR 576](https://github.com/IntersectMBO/cardano-cli/pull/576)

- Revert #569, where we tried to adapt the output of the `protocol-parameters` query to show all parameters in Conway, and which broke round-tripping and downstream requirements.
  (bugfix)
  [PR 572](https://github.com/IntersectMBO/cardano-cli/pull/572)

- Adapt the output of the `protocol-parameters` query to show all parameters in Conway. Includes a temporary hack around faulty ToJSON instance in `cardano-ledger`.
  (bugfix)
  [PR 569](https://github.com/IntersectMBO/cardano-cli/pull/569)

- - add support for registration of script DReps;
  - rework the `conway governance hash` command: separate sub-commands for anchor data and scripts
  (breaking, improvement, bugfix)
  [PR 563](https://github.com/IntersectMBO/cardano-cli/pull/563)

- Add --drep-keys flag to --create-testnet-data
  (feature, compatible)
  [PR 565](https://github.com/IntersectMBO/cardano-cli/pull/565)

- Make queries that optionally filter their result by DRep keys more explicit:
  - `cardano-cli conway query drep-state`
  - `cardano-cli conway query drep-stake-distribution`
  (breaking, improvement)
  [PR 555](https://github.com/IntersectMBO/cardano-cli/pull/555)

- Make queries that optionally filter their result by stake pool keys more explicit:
  - `cardano-cli * query-stake-snapshot`
  - `cardano-cli * query pool-params` (which is superseded by the next one, but still present)
  - `cardano-cli * query pool-state`
  - `cardano-cli conway query stake-snapshot`
  (breaking, improvement)
  [PR 541](https://github.com/IntersectMBO/cardano-cli/pull/541)

- Remove some dead code
  (compatible, improvement)
  [PR 560](https://github.com/IntersectMBO/cardano-cli/pull/560)

- Move input files out of `golden` directory
  (compatible, improvement)
  [PR 454](https://github.com/IntersectMBO/cardano-cli/pull/454)

- Suppress output of `voteDelegation` before Conway
  (bugfix)
  [PR 519](https://github.com/IntersectMBO/cardano-cli/pull/519)

## 8.18.0.0

- Upgrade hedgehog-extras to 0.5.0.0
  (compatible)
  [PR 536](https://github.com/IntersectMBO/cardano-cli/pull/536)

- Make it possible to merge again, by fixing dead links
  (compatible, improvement)
  [PR 528](https://github.com/IntersectMBO/cardano-cli/pull/528)

- use AnyShelleyBasedEra
  (improvement)
  [PR 535](https://github.com/IntersectMBO/cardano-cli/pull/535)

- In `transaction view` and `governance action view`, replace:

  `--output-format json` by `--output-json`
  `--output-format yaml` by `--output-yaml`
  (breaking)
  [PR 523](https://github.com/IntersectMBO/cardano-cli/pull/523)

- governance vote view: use `--output-format`, like other commands, instead of `--yaml`
  (breaking)
  [PR 521](https://github.com/IntersectMBO/cardano-cli/pull/521)

- split eras in transaction build
  (bugfix)
  [PR 520](https://github.com/IntersectMBO/cardano-cli/pull/520)

- create-testnet-data: rename --stake-delegators to --transient-stake-delegators

  Introduce --stake-delegators, that generates delegators, but write credentials to disk.
  (feature, breaking)
  [PR 512](https://github.com/IntersectMBO/cardano-cli/pull/512)

- Make `query pool-state` default to returning information on all pools
  (bugfix)
  [PR 514](https://github.com/IntersectMBO/cardano-cli/pull/514)

- `stake-address registration-certificate`: remove flag `--key-reg-deposit-amt` from all eras except conway
  (breaking)
  [PR 509](https://github.com/IntersectMBO/cardano-cli/pull/509)

- Remove the `constitution-hash` option from the non-conway versions of `query`
  (breaking)
  [PR 515](https://github.com/IntersectMBO/cardano-cli/pull/515)

- replace Aeson encode with encodePretty
  (improvement)
  [PR 513](https://github.com/IntersectMBO/cardano-cli/pull/513)

- [create-testnet-data: add succinct documentation in generated directory
  (breaking)
  [PR 508](https://github.com/IntersectMBO/cardano-cli/pull/508)

- Use the same parameter names for previous governance action txid and tx index in all governance actions.
  (breaking, improvement, test)
  [PR 511](https://github.com/IntersectMBO/cardano-cli/pull/511)

## 8.17.0.0

- Restore the inclusion of datum hashes in Alonzo era tx bodies
  (bugfix)
  [PR 507](https://github.com/input-output-hk/cardano-cli/pull/507)

- Remove ByronTx
  Bump to cardano-api-8.36.0.1
  (breaking)
  [PR 478](https://github.com/input-output-hk/cardano-cli/pull/478)

- Add `create-testnet-data` command, that generates files for starting a testnet, with a UI that is more conveninent than `create-staked`
  (feature, compatible)
  [PR 488](https://github.com/input-output-hk/cardano-cli/pull/488)

- Join some calls `writeLazyByteStringFile` in do blocks
  (improvement)
  [PR 497](https://github.com/input-output-hk/cardano-cli/pull/497)

- Update the help text for `--key-output-format` to say that the default is text-envelope
  (bugfix, documentation)
  [PR 486](https://github.com/input-output-hk/cardano-cli/pull/486)

## 8.16.0.1

- Fix era mismatch error in stake-address-info
  (compatible, bugfix)
  [PR 490](https://github.com/input-output-hk/cardano-cli/pull/490)

## 8.16.0.0

- Use node queries with tighter eons. Simplify prettyprinting.
  (improvement)
  [PR 481](https://github.com/input-output-hk/cardano-cli/pull/481)

- Rename stake-address-info field: stakeDelegation -> delegation for eras prior to Conway
  (compatible, bugfix)
  [PR 487](https://github.com/input-output-hk/cardano-cli/pull/487)

- Add missing help texts for `create-protocol-parameters-update`
  (bugfix, documentation)
  [PR 484](https://github.com/input-output-hk/cardano-cli/pull/484)

- Fix using queryStakeVoteDelegatees in eras before conway
  (compatible, bugfix)
  [PR 483](https://github.com/input-output-hk/cardano-cli/pull/483)

- Use `selectStakeCredentialWitness` instead of duplicating credential selection
  (compatible, bugfix)
  [PR 476](https://github.com/input-output-hk/cardano-cli/pull/476)

- Add support for Plutus V3 in command line interface
  (feature, compatible)
  [PR 479](https://github.com/input-output-hk/cardano-cli/pull/479)

- Add cost models file to the protocol parameter update commands
  (feature, breaking, compatible, test)
  [PR 405](https://github.com/input-output-hk/cardano-cli/pull/405)

- add the vote delegateee to the output of `stake-address-info`
  (compatible)
  [PR 452](https://github.com/input-output-hk/cardano-cli/pull/452)

- Revert #455 - use custom serialization for protocol parameters.
  (breaking)
  [PR 475](https://github.com/input-output-hk/cardano-cli/pull/475)

- governance actions: prefix `--stake-verification-key-*` and `--stake-key` arguments so that they are prefixed with `--deposit-return` now.
  (breaking)
  [PR 470](https://github.com/input-output-hk/cardano-cli/pull/470)

- Further separation of Byron from the Shelley based eras
  (breaking, improvement)
  [PR 467](https://github.com/input-output-hk/cardano-cli/pull/467)

- The era's being rendered in the NodeEraMismatchError error were mismatched
  (bugfix)
  [PR 469](https://github.com/input-output-hk/cardano-cli/pull/469)

## 8.15.0.0

- Collateral inputs and the minimum value calculation are not available in the Byron era. Update the code to reflect this.
  Upgrade to `cardano-api-8.33.0.0`
  (breaking, compatible, improvement)
  [PR 463](https://github.com/input-output-hk/cardano-cli/pull/463)

- Deleted `--constitution-anchor-metadata`
  Delete `--constitution-anchor-metadata-file`

  To workaround those deletions, call `cardano-cli conway governance hash ...` and pass the data/file you were previously passing
  to `--constitution-anchor-metadata`/`--constitution-anchor-metadata-file`, then read the result, and feed it
  to the call `cardano-cli conway governance action create-constitution`.

  Renamed `--constitution-anchor-url` to `--constitution-url`
  Renamed `--constitution-anchor-metadata-hash` to `--constitution-hash`
  (breaking)
  [PR 460](https://github.com/input-output-hk/cardano-cli/pull/460)

- Add `--out-file` flag to `conway governance hash` command, so that it can write its result to a file (instead of only to stdout)
  (feature)
  [PR 459](https://github.com/input-output-hk/cardano-cli/pull/459)

- Make --key-reg-deposit-amt mandatory in the parser of conway stake-address registration-certificate, because it is actually mandatory.
  (compatible, improvement, bugfix)
  [PR 456](https://github.com/input-output-hk/cardano-cli/pull/456)

- Update pCmds to take ShelleyBasedEra era instead of Cardano era
  (improvement)
  [PR 458](https://github.com/input-output-hk/cardano-cli/pull/458)

- Add command `governance hash (--file-binary|--file-text|--text)`.

  Remove flags: `--proposal-anchor-metadata-file`, `--proposal-anchor-metadata`,
  `--vote-anchor-metadata`, and `--vote-anchor-metadata-file`.

  To handle the removing of flags, call `cardano-cli conway governance hash` and pass the file or text to hash,
  and then pass the result of this command to the governance action you used to call directly
  (`create-constitution`, `update-committee`, `create-info`, `create-no-confidence`, `create-protocol-parameters-update`, `create-treasury-widthdrawal`, `vote create`).

  Rename `--proposal-anchor-url` to `--anchor-url`
  Rename `--proposal-anchor-metadata-hash` to `--anchor-data-hash`
  Rename `--vote-anchor-metadata-hash` to `--anchor-data-hash`
  (breaking)
  [PR 442](https://github.com/input-output-hk/cardano-cli/pull/442)

- Change output of `conway query protocol-parameters` so that it's consistent with the ledger's JSON output

  The output of `conway query protocol-parameters` becomes similar to the one of `cardano-cli conway query gov-state | jq .enactState.curPParams`
  (breaking)
  [PR 455](https://github.com/input-output-hk/cardano-cli/pull/455)

- Update to `cardano-8.31.0.0`
  (compatible)
  [PR 435](https://github.com/input-output-hk/cardano-cli/pull/435)

- Remove remaining uses of `IsCardanoEra`
  (improvement)
  [PR 434](https://github.com/input-output-hk/cardano-cli/pull/434)

- Puts the `nShelleyKeyWitnesses`, `nByronKeyWitnesses` arguments to `runCalculateMinFeeCmd` in the correct order.
  (bugfix)
  [PR 443](https://github.com/input-output-hk/cardano-cli/pull/443)


## 8.14.0.0

- Command types for `node` commands
  (improvement)
  [PR 428](https://github.com/input-output-hk/cardano-cli/pull/428)

- - Upgrade cardano-api to 8.30
    https://github.com/input-output-hk/cardano-api/blob/main/cardano-api/CHANGELOG.md#83000
  - Remove usage of stake pool keys as credentials
    https://github.com/input-output-hk/cardano-cli/pull/412
  (breaking, improvement)
  [PR 430](https://github.com/input-output-hk/cardano-cli/pull/430)

- Extended to non-extended key: write description field in all cases
  (improvement)
  [PR 416](https://github.com/input-output-hk/cardano-cli/pull/416)

- Split governance `MIRTransferConstructor`
  Add `babbage governance create-genesis-key-delegation-certificate`
  (feature, improvement)
  [PR 427](https://github.com/input-output-hk/cardano-cli/pull/427)

- Command argument types for `drep` commands
  (improvement)
  [PR 425](https://github.com/input-output-hk/cardano-cli/pull/425)

- Command arguments types for stake-pool commands
  (improvement)
  [PR 419](https://github.com/input-output-hk/cardano-cli/pull/419)

- Command argument types for poll commands
  (compatible, improvement)
  [PR 414](https://github.com/input-output-hk/cardano-cli/pull/414)

- Support converting a drep extended key to its non-extended version
  (feature, compatible)
  [PR 377](https://github.com/input-output-hk/cardano-cli/pull/377)

- Update cardano-ping to 0.2.0.7, so that `-j` is honored more often in ping command:
  the output of the info `network_rtt`, `handshake_rtt`, `negotiated_version`, and `queried_versions`
  is now written in JSON when `-j` is passed.
  (compatible)
  [PR 411](https://github.com/input-output-hk/cardano-cli/pull/411)

- Remove `ShelleyMode` and `ByronMode`
  (breaking)
  [PR 404](https://github.com/input-output-hk/cardano-cli/pull/404)

## 8.13.0.0

- Updated cardano-ledger, ouroboros-consensus and cardano-api packages
  Replaced  queryCommitteState with new queryCommitteeMembersState
  Added anchor to committee cold key resignation certificate command
  (breaking, maintenance)
  [PR 385](https://github.com/input-output-hk/cardano-cli/pull/385)

- Enable use of inline datums in Babbage era transactions
  (compatible, bugfix)
  [PR 407](https://github.com/input-output-hk/cardano-cli/pull/407)

- Remove `--update-proposal-file` from Conway onwards
  (compatible, improvement)
  [PR 396](https://github.com/input-output-hk/cardano-cli/pull/396)

- Delete `--utxo-cost-per-word`.  No longer supporting this flag in any era going forward
  This include deleting the CLI option from both legacy and era-base commands
  (breaking)
  [PR 400](https://github.com/input-output-hk/cardano-cli/pull/400)

- Fix delegating to always no confidence default DRep
  Add golden tests for stake address vote delegation certificates
  (bugfix, test)
  [PR 403](https://github.com/input-output-hk/cardano-cli/pull/403)

- Remove `--byron-era` from legacy `transaction build` command.
  (breaking)
  [PR 397](https://github.com/input-output-hk/cardano-cli/pull/397)

- `cardano-cli conway governance action create-protocol-parameters-update` was writing the update directly to disk instead of putting it in a `Proposal`
  (feature, compatible)
  [PR 369](https://github.com/input-output-hk/cardano-cli/pull/369)

- Record types for vote commands
  (improvement)
  [PR 395](https://github.com/input-output-hk/cardano-cli/pull/395)

- Refactor command argument types for the `genesis` commands.
  (improvement)
  [PR 398](https://github.com/input-output-hk/cardano-cli/pull/398)

- Simplify `toTxOutInAnyEra`
  (improvement)
  [PR 394](https://github.com/input-output-hk/cardano-cli/pull/394)

- `cardano-cli query stake-address-info` now returns the deposit for the currently delegated staking credential (in addition to what it returned before).
  (feature, compatible)
  [PR 375](https://github.com/input-output-hk/cardano-cli/pull/375)

- Upgrade to `cardano-api-8.27.0.0`
  (improvement)
  [PR 390](https://github.com/input-output-hk/cardano-cli/pull/390)

- Anchors: use cardano-api types, remove cardano-cli's ones
  (improvement)
  [PR 345](https://github.com/input-output-hk/cardano-cli/pull/345)

- Upgrade to `cardano-api-8.26.0.0`
  Support for DRep verification key and DRep extended keys
  (feature, improvement)
  [PR 389](https://github.com/input-output-hk/cardano-cli/pull/389)

- Make the stake identifier flags of treasury withdrawals less ambiguous: is it for returning deposits or for receiving funds?
  (breaking, bugfix)
  [PR 378](https://github.com/input-output-hk/cardano-cli/pull/378)

- Check that poll answer index is not negative. This replaces a Prelude.!! error by a regular `GovernanceCmdError` error
  (compatible, bugfix)
  [PR 380](https://github.com/input-output-hk/cardano-cli/pull/380)

- Shelley to Alonzo: add create-genesis-key-delegation-certificate to governance
  (compatible)
  [PR 368](https://github.com/input-output-hk/cardano-cli/pull/368)

- Add a command to view governance action files
  (feature, compatible, improvement, test)
  [PR 374](https://github.com/input-output-hk/cardano-cli/pull/374)

- Remove the `constitution-hash` command from eras. Users can use the `constitution` query instead.
  (breaking)
  [PR 370](https://github.com/input-output-hk/cardano-cli/pull/370)

- Clearer command line flags for vote, proposal, and constitution anchors
  (breaking, bugfix)
  [PR 372](https://github.com/input-output-hk/cardano-cli/pull/372)

- Command argument types for `transaction` commands
  (compatible, improvement)
  [PR 371](https://github.com/input-output-hk/cardano-cli/pull/371)

- Do not allow submitting transactions older than current node era
  (breaking)
  [PR 350](https://github.com/input-output-hk/cardano-cli/pull/350)

- Add a command to calculate DRep metadata hashes
  (feature, compatible)
  [PR 308](https://github.com/input-output-hk/cardano-cli/pull/308)

- Make it possible to add an anchor to votes
  (bugfix)
  [PR 362](https://github.com/input-output-hk/cardano-cli/pull/362)

- Enable reading of Conway era TxWitnesses from text envelope
  (bugfix)
  [PR 367](https://github.com/input-output-hk/cardano-cli/pull/367)

- transaction build-raw support for --vote-file and --proposal-file
  (feature, compatible)
  [PR 365](https://github.com/input-output-hk/cardano-cli/pull/365)

- Make `governance action create-protocol-parameters-update` Conway onwards only
  (breaking)
  [PR 366](https://github.com/input-output-hk/cardano-cli/pull/366)

- Command argument types for `governance key` commands
  (compatible, improvement)
  [PR 364](https://github.com/input-output-hk/cardano-cli/pull/364)

- Command argument types for `key` commands
  (improvement)
  [PR 360](https://github.com/input-output-hk/cardano-cli/pull/360)

- Bring back legacy *-poll commands
  (compatible)
  [PR 349](https://github.com/input-output-hk/cardano-cli/pull/349)

- The `cardano-cli conway governance query drep-state` and `... drep-stake-distribution` now accept zero or more DRep verification keys, making the help messages correct.
  (bugfix)
  [PR 348](https://github.com/input-output-hk/cardano-cli/pull/348)

- Remove doublon in PR template
  (improvement, maintenance)
  [PR 343](https://github.com/input-output-hk/cardano-cli/pull/343)

- Rename create-new-committee into update-committee
  (breaking, improvement)
  [PR 344](https://github.com/input-output-hk/cardano-cli/pull/344)

- Move `governance query` commands to `query` command group
  (breaking, improvement)
  [PR 347](https://github.com/input-output-hk/cardano-cli/pull/347)

- Added a command to create DRep retirement certificates
  (feature, compatible)
  [PR 316](https://github.com/input-output-hk/cardano-cli/pull/316)

- Added a command to inspect a vote file: `cardano-cli conway governance vote view`
  (feature, compatible, test)
  [PR 303](https://github.com/input-output-hk/cardano-cli/pull/303)

- Make `cardano-cli * transaction view` output JSON by default.
  (breaking, compatible, bugfix)
  [PR 319](https://github.com/input-output-hk/cardano-cli/pull/319)

## 8.12.0.0

- Add support for committee hot key witnesses
  (feature, compatible)
  [PR 338](https://github.com/input-output-hk/cardano-cli/pull/338)

- Make it possible to use cc hot keys for `conway governance vote create`
  (feature, compatible)
  [PR 337](https://github.com/input-output-hk/cardano-cli/pull/337)

- Move files that are not golden files into `input` directory
  (compatible, improvement)
  [PR 327](https://github.com/input-output-hk/cardano-cli/pull/327)

- create-poll, answer-poll, verify-poll: move to 'babbage governance' block
  (breaking, improvement)
  [PR 322](https://github.com/input-output-hk/cardano-cli/pull/322)

## 8.11.0.0

- Fix missing redeemers in certificate delegation and deregistration
  (bugfix)
  [PR 306](https://github.com/input-output-hk/cardano-cli/pull/306)

- Upgrade to `cardano-api-8.25.0.1`
  (compatible, maintenance)
  [PR 329](https://github.com/input-output-hk/cardano-cli/pull/329)

- Update to cardano-api-8.24
  (feature)
  [PR 324](https://github.com/input-output-hk/cardano-cli/pull/324)

- Add Cold Committee Key text envelope
  (bugfix)
  [PR 323](https://github.com/input-output-hk/cardano-cli/pull/323)

- Update to `cardano-api-8.23.1.0`
  (feature, compatible)
  [PR 320](https://github.com/input-output-hk/cardano-cli/pull/320)

- Tidy up query command structure
  (compatible, improvement)
  [PR 318](https://github.com/input-output-hk/cardano-cli/pull/318)

- Remove SPO registration bits from drep registration-certificate command
  Remove drep command from eras before Conway
  Remove `AnyRegistrationTarget`
  Remove `RegistrationTarget`
  (breaking, improvement)
  [PR 309](https://github.com/input-output-hk/cardano-cli/pull/309)

- Use `caseShelleyToBabbageOrConwayEraOnwards` from `cardano-api`
  (compatible, improvement)
  [PR 317](https://github.com/input-output-hk/cardano-cli/pull/317)

- Remove redundant conversions in JSON friendly instances
  (improvement)
  [PR 280](https://github.com/input-output-hk/cardano-cli/pull/280)

- Regularise era based command structure
  (improvement)
  [PR 279](https://github.com/input-output-hk/cardano-cli/pull/279)

- Simplify era handling
  (improvement)
  [PR 277](https://github.com/input-output-hk/cardano-cli/pull/277)

- A number of flags appeared in the parameters of new-committee multiple times, with the same long flag, making the parser break. This PR disambiguates the repeated occurences.
  (breaking)
  [PR 302](https://github.com/input-output-hk/cardano-cli/pull/302)

- Implementation of the create-info command
  (feature)
  [PR 292](https://github.com/input-output-hk/cardano-cli/pull/292)

- Update to the pre-commit script, so that it fails on hlint errors
  (improvement)
  [PR 296](https://github.com/input-output-hk/cardano-cli/pull/296)

## 8.10.0.0

- Rename `SomeWitness` to `SomeSigningWitness`.  Rename constructors to avoid name conflicts.  Move to `Key` module.
  (compatible, improvement)
  [PR 284](https://github.com/input-output-hk/cardano-cli/pull/284)

## 8.9.0.0

- Export `Cardano.CLI.Legacy.Options.pLegacyCardanoEra` for cardano-tesnet
  (compatible)
  [PR 286](https://github.com/input-output-hk/cardano-cli/pull/286)

- Include fixes from cardano-api 8.20.1.0: https://github.com/input-output-hk/cardano-api/blob/main/cardano-api/CHANGELOG.md#82010
  (compatible, bugfix)
  [PR 283](https://github.com/input-output-hk/cardano-cli/pull/283)

- Remove unused governance-related code
  (improvement)
  [PR 282](https://github.com/input-output-hk/cardano-cli/pull/282)

- Fix typo in stake-pool help text and clarify drep queries arguments
  (compatible)
  [PR 281](https://github.com/input-output-hk/cardano-cli/pull/281)

- Remove `--conway-era` flag
  (breaking, improvement)
  [PR 276](https://github.com/input-output-hk/cardano-cli/pull/276)

- Era-based `stake-pool` command
  (feature, compatible)
  [PR 275](https://github.com/input-output-hk/cardano-cli/pull/275)

- Fix git revision in `version` command
  (compatible, bugfix)
  [PR 274](https://github.com/input-output-hk/cardano-cli/pull/274)

- Add support for `--drep-script-hash` `--always-abstain` `--always-no-confidence` to `vote-delegation-certificate` command
  (feature, compatible)
  [PR 272](https://github.com/input-output-hk/cardano-cli/pull/272)

## 8.8.0.0

- Remove `Shelley` prefix on from errors
  (compatible, improvement)
  [PR 262](https://github.com/input-output-hk/cardano-cli/pull/262)

- Delete `governance drep delegation-certificate` command
  (breaking)
  [PR 265](https://github.com/input-output-hk/cardano-cli/pull/265)

- Accept script hash in `stake-and-vote-delegation-certificate` command
  (feature, compatible)
  [PR 264](https://github.com/input-output-hk/cardano-cli/pull/264)

- Port more commands to era-based command structure:
  * key
  * genesis
  * node
  * query
  * stake-pool
  * text-view
  (feature, compatible)
  [PR 266](https://github.com/input-output-hk/cardano-cli/pull/266)

- New `always-abstain-delegation-certificate` and `always-no-confidence-delegation-certificate` commands
  (feature, compatible)
  [PR 263](https://github.com/input-output-hk/cardano-cli/pull/263)

- New `stake-address vote-delegation-certificate` command
  (feature, compatible)
  [PR 261](https://github.com/input-output-hk/cardano-cli/pull/261)

- Change TreasuryWithdrawalCmd to a record
  (improvement)
  [PR 260](https://github.com/input-output-hk/cardano-cli/pull/260)

- New `stake-address stake-and-vote-delegation-certificate` command
  (feature, compatible)
  [PR 257](https://github.com/input-output-hk/cardano-cli/pull/257)

- Simplify `stake-address stake-delegation-certificate` command across eras
  (compatible, improvement)
  [PR 256](https://github.com/input-output-hk/cardano-cli/pull/256)

- Remove `EraBased` prefix and add `Cmd` suffix
  (compatible, improvement)
  [PR 254](https://github.com/input-output-hk/cardano-cli/pull/254)

- Move `genesis` run commands implementation into era based
  (compatible, improvement)
  [PR 235](https://github.com/input-output-hk/cardano-cli/pull/235)

- Move `node` run commands implementation into era based
  (compatible, improvement)
  [PR 242](https://github.com/input-output-hk/cardano-cli/pull/242)

- Remove `EraBased` prefix from era-based commands
  (compatible, improvement)
  [PR 244](https://github.com/input-output-hk/cardano-cli/pull/244)

- Update cardano-cli to newer cardano-ledger
  (breaking)
  - Use `cardano-api-8.20`.
  - Export `renderOpCertIntervalInformation`.
  - `DelegationsAndRewards` and `mergeDelegsAndRewards` moved to `cardano-api` (`8.20.0.0`).
  [PR 247](https://github.com/input-output-hk/cardano-cli/pull/247)

- Consistent naming for `stake-pool` command related types, functions and modules
  (compatible, improvement)
  [PR 246](https://github.com/input-output-hk/cardano-cli/pull/246)

- Era based `address` commands
  (feature, compatible)
  [PR 248](https://github.com/input-output-hk/cardano-cli/pull/248)

- Update error message for eras mismatch between node and cli
  (improvement)
  [PR 249](https://github.com/input-output-hk/cardano-cli/pull/249)

- Disable `redundant-constraints` warning only on `ghc-8.10.7`
  (improvement)
  [PR 245](https://github.com/input-output-hk/cardano-cli/pull/245)

- Update description fields in delegation certificates from `Stake Address Delegation Certificate` to respectively (Conway onwards):
  - `Stake Delegation Certificate`
  - `Vote Delegation Certificate`
  - `Stake and Vote Delegation Certificate`
  (feature)
  [PR 250](https://github.com/input-output-hk/cardano-cli/pull/250)

- Rename `delegation-certificate` to `stake-delegation-certificate` only in era-based command structure
  (breaking, improvement)
  [PR 243](https://github.com/input-output-hk/cardano-cli/pull/243)

- Update to `cardano-api-8.19`
  (improvement)
  [PR 209](https://github.com/input-output-hk/cardano-cli/pull/209)

- Move `query` run commands implementation into era based
  (compatible, improvement)
  [PR 236](https://github.com/input-output-hk/cardano-cli/pull/236)

- Move `key` run commands implementation into era based
  (compatible, improvement)
  [PR 237](https://github.com/input-output-hk/cardano-cli/pull/237)

- Era-based `stake-address` command group
  (feature)
  [PR 241](https://github.com/input-output-hk/cardano-cli/pull/241)

- Move `text-view` run commands implementation into era-based
  (compatible, improvement)
  [PR 238](https://github.com/input-output-hk/cardano-cli/pull/238)

- Remove era based prefix from errors
  (improvement)
  [PR 239](https://github.com/input-output-hk/cardano-cli/pull/239)

- Move `pool` run commands implementation into era based
  (compatible, improvement)
  [PR 234](https://github.com/input-output-hk/cardano-cli/pull/234)

- Prefix complex delegation certificate option
  (breaking)
  [PR 225](https://github.com/input-output-hk/cardano-cli/pull/225)

- Remove duplicate instances and add new `FeatureInEra ShelleyBasedEra` instance
  (compatible)
  [PR 240](https://github.com/input-output-hk/cardano-cli/pull/240)

- Era sensitive transaction run commands
  (feature)
  [PR 230](https://github.com/input-output-hk/cardano-cli/pull/230)

- Delete legacy `conway governance` commands
  (breaking)
  [PR 231](https://github.com/input-output-hk/cardano-cli/pull/231)

- Move `address` run command implementation into era-based
  (compatible)
  [PR 232](https://github.com/input-output-hk/cardano-cli/pull/232)

- Move `stake-address` run commands implementation into era based
  (compatible, improvement)
  [PR 233](https://github.com/input-output-hk/cardano-cli/pull/233)

- Ensure both proposal and constitution related commands will accept text, file and hash
  (breaking, bugfix)
  [PR 218](https://github.com/input-output-hk/cardano-cli/pull/218)

- Move transaction command code into era based
  (improvement)
  [PR 229](https://github.com/input-output-hk/cardano-cli/pull/229)

- Fix query `key-period-info`
  (bugfix)
  [PR 228](https://github.com/input-output-hk/cardano-cli/pull/228)

- Add legacy prefix to legacy run commands
  (improvement)
  [PR 217](https://github.com/input-output-hk/cardano-cli/pull/217)

- Consolidate legacy governance commands
  (improvement)
  [PR 216](https://github.com/input-output-hk/cardano-cli/pull/216)

## 8.7.0.0

- Change `--constitution-file` to `--proposal-file` in `transaction build`
  (breaking, improvement)
  [PR 219](https://github.com/input-output-hk/cardano-cli/pull/219)

- Remove error placeholders in `Cardano.CLI.Json.Friendly`
  (improvement)
  [PR 212](https://github.com/input-output-hk/cardano-cli/pull/212)

- Use record syntax for `TxBodyContent`
  (improvement)
  [PR 215](https://github.com/input-output-hk/cardano-cli/pull/215)

- Upgrade to `cardano-api-8.17.0.0`
  (improvement)
  [PR 210](https://github.com/input-output-hk/cardano-cli/pull/210)

- Remove `experimental` subcommand
  (breaking, improvement)
  [PR 211](https://github.com/input-output-hk/cardano-cli/pull/211)

- Add governance query commands
  (feature, compatible)
  [PR 189](https://github.com/input-output-hk/cardano-cli/pull/189)

- Read and write `VotingProcedures` files instead of `VotingEntry` files
  (breaking)
  [PR 203](https://github.com/input-output-hk/cardano-cli/pull/203)

- Enable `--drep-script-hash` option
  (feature, compatible)
  [PR 204](https://github.com/input-output-hk/cardano-cli/pull/204)

## 8.6.0.0

- conway related commands
  `create-constitution` now requires:
     - mainnet or testnet
     - Optionally the previous governance action id
     - A proposal url and an anchor data hash of the proposal
     - A constitution url
  `create-new-committee` now requires:
     - mainnet or testnet
     - A proposal url and an anchor data hash of the proposal
     - Optionally the previous governance action id
  `create-no-confidence` now requires:
     - mainnet or testnet
     - A proposal url and an anchor data hash of the proposal
  `create-treasury-withdrawal` now requires:
     - mainnet or testnet
     - A proposal url and an anchor data hash of the proposal
  `governance vote create` now requires:
     - The governance action identifier
  (feature, breaking)
  [PR 174](https://github.com/input-output-hk/cardano-cli/pull/174)

- Merge `LegacyClientCmdError` into `CmdError`
  (improvement)
  [PR 195](https://github.com/input-output-hk/cardano-cli/pull/195)

- New `governance drep id` command
  (feature, compatible)
  [PR 194](https://github.com/input-output-hk/cardano-cli/pull/194)

- Governance `drep` and `vote` command groups
  (breaking)
  [PR 191](https://github.com/input-output-hk/cardano-cli/pull/191)

- Move legacy errors out of legacy command structure modules
  (improvement)
  [PR 192](https://github.com/input-output-hk/cardano-cli/pull/192)

- Move command errors from era-based to `CmdError` module
  (improvement)
  [PR 188](https://github.com/input-output-hk/cardano-cli/pull/188)

- Add `--verification-key` option to `committee key-hash` command
  (compatible)
  [PR 187](https://github.com/input-output-hk/cardano-cli/pull/187)

- Changes:
  * Move to own modules under `Cardano.CLI.Types.Errors.GovernanceCmdError`:
    * `GovernanceCmdError`
    * `ShelleyStakeAddressCmdError`
    * `StakeAddressDelegationError`
    * `StakeAddressRegistrationError`
    * `ScriptDecodeError`
  (improvement)
  [PR 186](https://github.com/input-output-hk/cardano-cli/pull/186)

- Implement `conway governance committee create-cold-key-resignation` certificate
  (compatible)
  [PR 146](https://github.com/input-output-hk/cardano-cli/pull/146)

- Delete unused module `Cardano.CLI.Legacy.Options`
  (improvement)
  [PR 185](https://github.com/input-output-hk/cardano-cli/pull/185)

- Implement `conway governance committee create-hot-key-authorization-certificate` command
  (compatible)
  [PR 145](https://github.com/input-output-hk/cardano-cli/pull/145)

- Remove example era-based parsers we don't need anymore
  (breaking)
  [PR 180](https://github.com/input-output-hk/cardano-cli/pull/180)

- Remove all uses of `toS`, `purer`, `cborError` and `intercalate` from `Cardano.Prelude`
  (improvement)
  [PR 182](https://github.com/input-output-hk/cardano-cli/pull/182)

- Refactor: Use `maybeFeatureInEra` instead of `featureInEra` where possible
  (improvement)
  [PR 183](https://github.com/input-output-hk/cardano-cli/pull/183)

- Add goverance info action creation to era based cli. Note this currently does nothing as cardano-ledger provides a placeholder constructor for this governance action.
  (feature, compatible)
  [PR 179](https://github.com/input-output-hk/cardano-cli/pull/179)

- Add error type and messages in `Cardano.CLI.EraBased.Run`.
  (compatible, test)
  [PR 156](https://github.com/input-output-hk/cardano-cli/pull/156)

- Add create-no-confidence command to era based cardano-cli
  (feature, compatible)
  [PR 176](https://github.com/input-output-hk/cardano-cli/pull/176)

- Add create-new-committee command to era based cli
  (feature, compatible)
  [PR 175](https://github.com/input-output-hk/cardano-cli/pull/175)

- Add conway governance action create-treasury-withdrawal
  (feature, breaking)
  [PR 155](https://github.com/input-output-hk/cardano-cli/pull/155)

- Add create-protocol-parameters-update command to era based commands
  (feature)
  [PR 170](https://github.com/input-output-hk/cardano-cli/pull/170)

- Reenable all golden tests
  (compatible, test)
  [PR 164](https://github.com/input-output-hk/cardano-cli/pull/164)

- CLI command 'drep cardano-cli conway governance drep key-gen` for DRep key generation
  (feature, compatible)
  [PR 163](https://github.com/input-output-hk/cardano-cli/pull/163)

- Structuring legacy code so that era-based code does not depend on it
  (compatible)
  [PR 158](https://github.com/input-output-hk/cardano-cli/pull/158)

- Update to `cardano-api-8.13.0.0`
  (compatible)
  [PR 169](https://github.com/input-output-hk/cardano-cli/pull/169)

- Wire up constitution creation in the new era based cli commands
  (feature, compatible)
  [PR 142](https://github.com/input-output-hk/cardano-cli/pull/142)

- Fix deregistration-certificate: generate deregistration instead of registration certs
  (compatible, bugfix)
  [PR 159](https://github.com/input-output-hk/cardano-cli/pull/159)

- Plural for command groups
  (compatible)
  [PR 151](https://github.com/input-output-hk/cardano-cli/pull/151)

- Implement `conway governance committee key-hash` command
  (compatible)
  [PR 144](https://github.com/input-output-hk/cardano-cli/pull/144)

- Implement conway governance committee hot-key-gen command
  (feature; compatible)
  [PR 136](https://github.com/input-output-hk/cardano-cli/pull/136)

## 8.5.0.0

- Add always abstain, always no confidence and drep script hash options. NB: The drep script hash option is hidden until ledger exposed the necessary constructor.
  (feature; compatible)
  [PR 137](https://github.com/input-output-hk/cardano-cli/pull/137)

- Stub `governance committee key-gen-cold` command
  (feature; compatible)
  [PR 126](https://github.com/input-output-hk/cardano-cli/pull/126)

- Add vote creation command to new era based cli structure
  (feature; compatible)
  [PR 114](https://github.com/input-output-hk/cardano-cli/pull/114)

- Remove `--protocol-params-file`  from `transaction build`  command.
  (feature; breaking)
  [PR 34](https://github.com/input-output-hk/cardano-cli/pull/34)

## 8.4.1.0

- Add registration certificate creation command to the new era based cli
  structure. We can create the following registration certififcates:
    - Stake pool
    - Stake key
    - DRep key
  (feature; compatible)
  [PR 110](https://github.com/input-output-hk/cardano-cli/pull/110)

- User friendly error messages for Conway era
  (feature; no-api-changes)
  [PR 84](https://github.com/input-output-hk/cardano-cli/pull/84)

- Add rendering for ConwayCertificate in Cardano.Cli.Json.Friendly
  (bugfix; no-api-changes)
  [PR 113](https://github.com/input-output-hk/cardano-cli/pull/113)

- Refactoring to merge queries to share the same connection
  (feature; no-api-changes)
  [PR 29](https://github.com/input-output-hk/cardano-cli/pull/29)

- Add options to delegate voting stake
  (feature; breaking)
  [PR 109](https://github.com/input-output-hk/cardano-cli/pull/109)

- Share MIR certificates code between era-based and legacy CLI parsers
  (feature; breaking)
  [PR 107](https://github.com/input-output-hk/cardano-cli/pull/107)

## 8.4.0.0

- Era-sensitive command structure
  (feature; breaking)
  [PR 98](https://github.com/input-output-hk/cardano-cli/pull/98)

- Set default era to Babbage in stake-address, stake-pool and governance commmands
  (feature; compatible)
  [PR 90](https://github.com/input-output-hk/cardano-cli/pull/90)

- `CARDANO_ERA` environment variable support
  (feature; compatible)
  [PR 80](https://github.com/input-output-hk/cardano-cli/pull/80)

## 8.3.2.0

- Make it build with ghc-9.6
  (maintenance; compatible)
  [PR 89](https://github.com/input-output-hk/cardano-cli/pull/89)

## 8.3.1.0

- Make it build with ghc-9.6
  (maintenance; compatible)
  [PR 81](https://github.com/input-output-hk/cardano-cli/pull/81)

- Add alonzo era to `Parser AnyShelleyBasedEra`
  (bugfix; no-api-changes)
  [PR 67](https://github.com/input-output-hk/cardano-cli/pull/67)

## 8.3.0.0

- Add a query for the hash of the constitution
  (feature; compatible)
  [PR 63](https://github.com/input-output-hk/cardano-cli/pull/63)

- Update cardano-cli with the ability to create votes and governance actions.
  Update tx build with the ability to specify votes and governance actions.
  (feature; compatible)
  [PR 45](https://github.com/input-output-hk/cardano-cli/pull/45)

## 8.2.1

- The `--protocol-params-file` option of the `transaction build` command is
  now marked as deprecated in Usage help.  The option was already deprecated
  and ignored.
  (bugfix; compatible)
  [PR 28](https://github.com/input-output-hk/cardano-cli/pull/28)

- `cardano-cli ping`  changes:
  - timestamp format changed to `ISO8601`
  - support `NodeToNodeV_11`, `NodeToNodeV_12` and `NodeToClientV_16`.
  - added `--query-versions` flag, instread of doing version negotiation the remote side will reply with its set of supported versions.
  - fixed support for `node-to-client` mini-protocol over Unix socket.
  (feature, bugfix; compatible)
  [PR 30](https://github.com/input-output-hk/cardano-cli/pull/30)

## 8.2.0

- Add `--conway-era` flag.
  (feature; compatible)
  [PR 20](https://github.com/input-output-hk/cardano-cli/pull/20)

- Remove ApplicationName and ApplicationVersion config parameters
  [PR 5240](https://github.com/input-output-hk/cardano-node/pull/5240)

## 8.1.0

- Delete deprecated shelley command group
  ([PR5264](https://github.com/input-output-hk/cardano-node/pull/5264))

- Use cardano-api from CHaP
  ([PR5047](https://github.com/input-output-hk/cardano-node/pull/5047))

- Implement deposit handling when balancing transactions
  ([PR5218](https://github.com/input-output-hk/cardano-node/pull/5218))

- Make `renderScriptWitnessIndex` output more explicit.  This means
  slightly improved error messages for failed script execution
  errors.
  ([PR5221](https://github.com/input-output-hk/cardano-node/pull/5221))

- `--socket-path` option works across all commands
  ([PR5207](https://github.com/input-output-hk/cardano-node/pull/5207))

- Fix `toEraInMode` for conway.  This means for example that `query utxo`
  works in Conway.
  ([PR5175](https://github.com/input-output-hk/cardano-node/pull/5175))
- Update ledger dependency bounds.
  ([PR 5243](https://github.com/input-output-hk/cardano-node/pull/5243))
- Add the query flag in `Ping` parser. See [here](https://github.com/input-output-hk/ouroboros-network/issues/3907) for more details.
  ([PR 5243](https://github.com/input-output-hk/cardano-node/pull/5243))

## 8.0.0 -- May 2023

- Remove cardano-cli address build-script ([PR 4700](https://github.com/input-output-hk/cardano-node/pull/4700))
- Remove support for reading protocol parameters from Shelley genesis file ([PR 5053](https://github.com/input-output-hk/cardano-node/pull/5053))

- New commands for on-chain SPOs polls under `shelley governance`:
  - `create-poll`:
      For the current governing entities, as a means to create new polls.

  - `answer-poll`:
      For participants who want to answer a given poll.

  - `verify-poll`:
      For anyone who seek to verify a poll entry (e.g. explorers)

  The commands are built to fit and play nicely within the cardano-cli.
  The poll and answers structures are based on transaction metadata and
  require to be embedded in an actual transaction. The added commands
  however only works from metadata and raw "GovernancePoll" envelopes.

  See [CIP proposal](https://github.com/cardano-foundation/CIPs/pull/496) for details.

  - ([PR 5132](https://github.com/input-output-hk/cardano-node/pull/5132))
  - ([PR 5112](https://github.com/input-output-hk/cardano-node/pull/5112))
  - ([PR 5172](https://github.com/input-output-hk/cardano-node/pull/5172))
  - ([PR 5184](https://github.com/input-output-hk/cardano-node/pull/5184))

- Any command that takes a `--mainnet` flag or a `--testnet-magic` flag can have that setting
  supplied with the `CARDANO_NODE_NETWORK_ID=mainnet` or `CARDANO_NODE_NETWORK_ID=<number>`
  instead where `<number>` is the network id.
  ([PR5119](https://github.com/input-output-hk/cardano-node/pull/5119))
  ([PR5119](https://github.com/input-output-hk/cardano-node/pull/5119))

- Add `--out-file` argument to the shelley stake-pool id command.
  Add `--key-format-output` argument to the following commands:
  - `address key-gen`
  - `stake-address key-gen`
  - `node key-gen`
  - `node key-gen-KES`
  - `node key-gen-VRF`
  - `genesis create-staked`
  - `genesis create`
  [PR5058](https://github.com/input-output-hk/cardano-node/pull/5058)


### Features

- The `--socket-path` option is now a required CLI argument for relevant commands if `CARDANO_NODE_SOCKET_PATH` is not supplied.
  ([PR 5120](https://github.com/input-output-hk/cardano-node/pull/5120))

- Default to the ledger's CDDL format for transaction body creation by removing flags `--cddl-format` and `--cli-format` from `build` and `build-raw` ([PR 4303](https://github.com/input-output-hk/cardano-node/pull/4303))

- Add `query tx-mempool` ([PR 4276](https://github.com/input-output-hk/cardano-node/pull/4276))

- Allow assembling transactions with no witnesses ([PR 4408](https://github.com/input-output-hk/cardano-node/pull/4408))

- Add `slotInEpoch` and `slotsToEpochEnd` to output of `query tip` command ([PR 4912](https://github.com/input-output-hk/cardano-node/pull/4912))

- Add `--stake-address` option to the following CLI commands ([PR 3404](https://github.com/input-output-hk/cardano-node/pull/3404)):
  - address build
  - stake-address build
  - stake-address registration-certificate
  - stake-address delegation-certificate
  - stake-address deregistration-certificate

- Add `--socket-path` CLI option for CLI commands that use `CARDANO_NODE_SOCKET_PATH` ([PR 4910](https://github.com/input-output-hk/cardano-node/pull/4910))

- Add `utcTimeToSlotNo` function to support UTC -> slot number conversion ([PR 5130](https://github.com/input-output-hk/cardano-node/pull/5130))

- Add `query slot-number` command line option to support UTC -> slot number conversion ([PR 5149](https://github.com/input-output-hk/cardano-node/pull/5149))

- Remove `--stake-address` option from `stake-address build`
  ([PR 5061](https://github.com/input-output-hk/cardano-node/pull/5061))

- The bounds of many CLI arguments are now checked
  ([PR 4919](https://github.com/input-output-hk/cardano-node/pull/4919))

- Re-add support for decoding `GenesisExtendedKey` text envelope
  ([PR 4894](https://github.com/input-output-hk/cardano-node/pull/4894))

- Preserve `ScriptData` bytes with `HashableScriptData`
  ([PR 4886](https://github.com/input-output-hk/cardano-node/pull/4886))

- Disallow empty cost model for create update proposal
  ([PR 4885](https://github.com/input-output-hk/cardano-node/pull/4885))

- Detect invalid counter and certificate
  ([PR 4880](https://github.com/input-output-hk/cardano-node/pull/4880))

- Filter out duplicate collateral inputs in transaction build commands
  ([PR 4839](https://github.com/input-output-hk/cardano-node/pull/4839))

- Update cardano-cli banner
  ([PR 4816](https://github.com/input-output-hk/cardano-node/pull/4816))

- Better error message for `query utxo` command
  ([PR 4788](https://github.com/input-output-hk/cardano-node/pull/4788))

- Remove simple script distinction
  ([PR 4763](https://github.com/input-output-hk/cardano-node/pull/4763))

- Optimise `query stake-snapshot` command
  ([PR 4654](https://github.com/input-output-hk/cardano-node/pull/4754))

- Filter out duplicate collateral inputs in `transaction build` and `transaction build-raw` comands
  ([PR 4649](https://github.com/input-output-hk/cardano-node/pull/4749))

- Add support for `ghc-9.2` and partial support for `CHaP`
  ([PR 4701](https://github.com/input-output-hk/cardano-node/pull/4701))

- Update cli's help to indicate that Babbage is the default era
  ([PR 4674](https://github.com/input-output-hk/cardano-node/pull/4674))

- New `cardano-cli ping` command
  ([PR 4664](https://github.com/input-output-hk/cardano-node/pull/4664))

- Improved error message for failed asset name decode
  ([PR 4626](https://github.com/input-output-hk/cardano-node/pull/4626))

- Better pipe handling
  ([PR 4625](https://github.com/input-output-hk/cardano-node/pull/4625))

- Restore `--cddl-format`
  ([PR 4617](https://github.com/input-output-hk/cardano-node/pull/4617))

- Switch default era to Babbage
  ([PR 4485](https://github.com/input-output-hk/cardano-node/pull/4485))

- Update error message for incorrectly witnessed collateral inputs
  ([PR 4484](https://github.com/input-output-hk/cardano-node/pull/4484))

- Return `Lovelace` for `calculateMinimumUTxO`
  ([PR 4482](https://github.com/input-output-hk/cardano-node/pull/4482))

- Infer protocol params in `transaction build` command
  ([PR 4431](https://github.com/input-output-hk/cardano-node/pull/4431))

- Use `openFileBlocking` for reading signing keys
  ([PR 4342](https://github.com/input-output-hk/cardano-node/pull/4342))

- Multiple pools support in `query stake-snapshot`
  ([PR 4279](https://github.com/input-output-hk/cardano-node/pull/4279))

- Optimise `query leadership-schedule` command
  ([PR 4250](https://github.com/input-output-hk/cardano-node/pull/4250))

- Update `create-staked` with the ability to specify relays for all created stake pools
  ([PR 4234](https://github.com/input-output-hk/cardano-node/pull/4234))

- More memory efficient `query ledger-state` command
  ([PR 4205](https://github.com/input-output-hk/cardano-node/pull/4205))

- Render reference script hashes when using `--calculate-plutus-script-cost` option
  ([PR 4204](https://github.com/input-output-hk/cardano-node/pull/4204))

- Update build command to automatically calculate the total and return collateral values
  ([PR 4198](https://github.com/input-output-hk/cardano-node/pull/4198))

- Optimise `query stake-snapshot` command
  ([PR 4179](https://github.com/input-output-hk/cardano-node/pull/4179))

- New `query pool-state` command
  ([PR 4170](https://github.com/input-output-hk/cardano-node/pull/4170))

- Add `utxoCostPerByte` protocol parameter
  ([PR 4141](https://github.com/input-output-hk/cardano-node/pull/4141))

- Transaction build in any alonzo era when on babbage testnet
  ([PR 4135](https://github.com/input-output-hk/cardano-node/pull/4135))

- Expose Key interface in Cardano.Api.Shelley
  ([PR 4048](https://github.com/input-output-hk/cardano-node/pull/4048))

- Reduce memory usage of create staked command
  ([PR 4021](https://github.com/input-output-hk/cardano-node/pull/4021))

- Add new interim governance commands: {create, answer, verify}-poll
  ([PR 5112](https://github.com/input-output-hk/cardano-node/pull/5112))

- Frozen callstack for checkTextEnvelopeFormat function
  ([PR 5059](https://github.com/input-output-hk/cardano-node/pull/5059))

- Split serialisation from IO
  ([PR 5049](https://github.com/input-output-hk/cardano-node/pull/5049))

- Move parsers to reusable location
  ([PR 5046](https://github.com/input-output-hk/cardano-node/pull/5046))

- Remove unused error constructors
  ([PR 5041](https://github.com/input-output-hk/cardano-node/pull/5041))

- Integrate latest ledger dependencies
  ([PR 5013](https://github.com/input-output-hk/cardano-node/pull/5013))

- Remove error calls in Cardano.CLI.Shelley.Run.Transaction
  ([PR 4958](https://github.com/input-output-hk/cardano-node/pull/4958))

- Preserve ScriptData bytes fix
  ([PR 4926](https://github.com/input-output-hk/cardano-node/pull/4926))

- Reduce number of calls to toLedgerPParams
  ([PR 4903](https://github.com/input-output-hk/cardano-node/pull/4903))

- Simplify SerialiseAsRawBytes type class
  ([PR 4876](https://github.com/input-output-hk/cardano-node/pull/4876))

- Modify constructBalancedTx to take LedgerEpochInfo
  ([PR 4858](https://github.com/input-output-hk/cardano-node/pull/4858))

- Node 1.35.5
  ([PR 4851](https://github.com/input-output-hk/cardano-node/pull/4851))

- UTxO-HD: Make devops-shell compile again and fix cli parser
  ([PR 4843](https://github.com/input-output-hk/cardano-node/pull/4843))

- Add ReaderT of NodeToClientVersion to LocalStateQueryExpr
  ([PR 4809](https://github.com/input-output-hk/cardano-node/pull/4809))

- Move signing key reading to cardano-api
  ([PR 4698](https://github.com/input-output-hk/cardano-node/pull/4698))

- Replace Data.Map with Data.Map.Strict
  ([PR 4675](https://github.com/input-output-hk/cardano-node/pull/4675))

- Move implementation inside `runTransactionCmd` to toplevel definitions
  ([PR 4673]](https://github.com/input-output-hk/cardano-node/pull/4673))

- Remove error calls in renderShelleyTxCmdError
  ([PR 4644](https://github.com/input-output-hk/cardano-node/pull/4644))

### Bugs

- Allow reading signing keys from a pipe ([PR 4342](https://github.com/input-output-hk/cardano-node/pull/4342))

- Query protocol parameters from the node in the `transaction build` command ([PR 4431](https://github.com/input-output-hk/cardano-node/pull/4431))

- Fix `qKesKesKeyExpiry` in `kes-period-info` ([PR 4909](https://github.com/input-output-hk/cardano-node/pull/4909))

- Fix query era mismatch bug in transaction build command when using flag `--calculate-plutus-script-cost`
  ([PR 4538](https://github.com/input-output-hk/cardano-node/pull/4538))

- Fix bug - TxWitness text envelope format does not roundtrip in Shelley era
  ([PR 4501](https://github.com/input-output-hk/cardano-node/pull/4501))

- Fix query protocol-state
  ([PR 4102](https://github.com/input-output-hk/cardano-node/pull/4102))

- Fix help message for `--script-invalid` option of `build`/`build-raw`
  ([PR 4121](https://github.com/input-output-hk/cardano-node/pull/4121))

- Fix transaction build command era backwards incompatibility
  ([PR 4483](https://github.com/input-output-hk/cardano-node/pull/4483))

- Fix minUTxO calculation in `calculate-min-required-utxo`

- Fix key non extended key for `StakeExtendedVerificationKeyShelley_ed25519_bip32` envelope
  ([PR 4918](https://github.com/input-output-hk/cardano-node/pull/4918))

- Fix `qKesKesKeyExpiry` to not always be `null`
  ([PR 4909](https://github.com/input-output-hk/cardano-node/pull/4909))

- `create-staked` command: Fix UTxO size distribution
  ([PR 4765](https://github.com/input-output-hk/cardano-node/pull/4765))

- Fix bug in hash computation in `genesis create-cardano` command
  ([PR 4761](https://github.com/input-output-hk/cardano-node/pull/4761))

## 1.35.3 -- August 2022

- Update build and build-raw commands to accept simple reference minting scripts (#4087)
- Fix query protocol-state (#4102)
- Render reference script hashes when using `--calculate-plutus-script-cost` option (#4204)
- Transaction build in any alonzo era when on babbage testnet (#4135)

## 1.35.2 -- July 2022 (not released)

None

## 1.35.1 -- July 2022 (not released)

None

## 1.35.0 -- June 2022
- Add Vasil hardfork to cardano-api and cardano-cli (#3765)
- Reference script integration (#3953)
- Wire up remaining Plutusv2 reference script types (#4034)
- Add friendly printing of transactions (envelopes) with signatures (#3617)
- cardano-cli transaction view: Add friendly certificate printing (#3377)
- cardano-cli query kes-period-info: Always display metrics (#3683)
- JSON format for leadership schedule (#3687)
- Vasil cardano-cli update (#3810)
- Prevent return collateral from including reference scripts and datums (#3850)
- kes-period-info property test (#3718)
- Extend deserialiseFromRawBytesHex to produce error description (#3304)
- add genesis create-cardano command (#3832)
- Propagate protocol in block type (#3818)
- Fix kes period info command (#3945)
- Create VRF signing key file with correct permissions (#1948)
- Set local encoding to UTF-8 in cardano-cli (#4018)
- Update example-reference-script-usage.sh to also use inline datums (#4006)
- Wire up simple reference scripts in cardano-cli (#4014)
- Add read-only-tx-in-reference option to cardano-cli #(4042)

## 1.34.0 -- February 2022

- Fix some spelling errors in the CLI help text.  (#3499)
- Add a prettier rendering of update proposals. (#3208)
- Add support for CBOR-encoded blobs in the `transaction build` and `transaction
  build-raw` commands. (#3483)
- Implement a `leadership-schedule` command. This can calculate a stake pool's
  leadership schedule for the current and following epoch. It requires access to
  the VRF signing key for that stake pool.

  ```
  > cardano-cli query leadership-schedule \
     --testnet-magic 42 \
     --genesis example/shelley/genesis.json \
     --stake-pool-id  pool12t0y7agkqct89pf00eeytkvfjlquv76tjy27duannan9w63ckxv \
     --vrf-signing-key-file example/node-pool1/shelley/vrf.skey
     --current
     SlotNo                          UTC Time
     --------------------------------------------------------
     4073                   2021-12-29 17:26:54.998001755 UTC
     4126                   2021-12-29 17:27:00.298001755 UTC
     4206                   2021-12-29 17:27:08.298001755 UTC
     4256                   2021-12-29 17:27:13.298001755 UTC
     4309                   2021-12-29 17:27:18.598001755 UTC
     4376                   2021-12-29 17:27:25.298001755 UTC
     4423                   2021-12-29 17:27:29.998001755 UTC
     4433                   2021-12-29 17:27:30.998001755 UTC
  ``` (#3464, #3494)
- The CLI now supports outputting transaction bodies in ledger-compliant CDDL in
  the `transaction build` and `transaction build-raw` commands. This is
  specified by using the `--cddl-format` flag. (#3505)
- Implement a `kes-period-info` command in the CLI. This checks that your
  operational certificate is correct. It checks:
  - The counters match what is in the node's protocol state
  - The KES period in the operational certificate is correct (based on the
    current slot).
  ```
  > cardano-cli query kes-period-info --testnet-magic 42  \
    --op-cert-file example/node-pool1/shelley/node.cert
  ✓ The operational certificate counter agrees with the node protocol state counter
  ✓ Operational certificate's kes period is within the correct KES period interval
  {
      "qKesNodeStateOperationalCertificateNumber": 6,
      "qKesCurrentKesPeriod": 404,
      "qKesOnDiskOperationalCertificateNumber": 6,
      "qKesRemainingSlotsInKesPeriod": 3760228,
      "qKesMaxKESEvolutions": 62,
      "qKesKesKeyExpiry": "2022-03-20T21:44:51Z",
      "qKesEndKesInterval": 434,
      "qKesStartKesInterval": 372,
      "qKesSlotsPerKesPeriod": 129600
  }
  ``` (#3459, #3572, #3599)
- The CLI now displays collateral inputs in a nicer fashion. (#3463)
- The `transaction sign` command now allows for incremental signing by providing
  an already signed transaction via `--tx-file`. This allows more easily adding
  multiple signatures to a transaction. (#3549)
- The `transaction build` command now supports an option
  (`--calculate-plutus-script-cost`) to compute the cost for included scripts.
  ```
  cardano-cli transaction build \
  --alonzo-era \
  --cardano-mode \
  --testnet-magic "$TESTNET_MAGIC" \
  --change-address "$utxoaddr" \
  --tx-in "$plutusutxotxin" \
  --tx-in-collateral "$txinCollateral" \
  --tx-out "$dummyaddress+10000000" \
  --tx-in-script-file "$plutusscriptinuse" \
  --tx-in-datum-file "$datumfilepath"  \
  --protocol-params-file "$WORK/pparams.json" \
  --tx-in-redeemer-file "$redeemerfilepath" \
  --calculate-plutus-script-cost "$WORK/create-datum-output.scriptcost"
  > cat $WORK/create-datum-output.scriptcost
  [
    {
        "executionUnits": {
            "memory": 1700,
            "steps": 476468
        },
        "lovelaceCost": 133,
        "scriptHash": "67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656"
    }
  ]
  ``` (#3589)

## 1.33.0 -- December 2021
## 1.32.1 -- November 2021

- Default CLI commands to the Alonzo era. (#3339)
- Add defaults for building the Alonzo genesis. (#3346)

## 1.31.0 -- October 2021

- Restore support for deserialising transactions built by pre-1.27.0.0 node
  versions. (#3226)
- Various internal refactorings and improvements. (#3234)
- Use the new `GetChainBlockNo` and `GetChainPoint` queries in the query tip
  command. There is a fallback to the older method using the full chain sync
  query. (#3179)
- Allow provision of optional datums to a transaction using the CLI option
  `--tx-out-datum-embed-value`. This mechanism can for example be used to
  provide the actual script locking an output, for use when spending it. (#3171)
- Fix the use of withdrawals using the `transaction build` command. (#3317)
- Allow extended payment keys to be specified as a Plutus required signer.
  (#3319)

## 1.30.0 -- September 2021

- Allow the user to specify a signature as required when spending from a
  multisig/timelock script using the `build` or `build-raw` commands. Required
  signers *must* be present in the witnesses, and only required signers are
  visible to Plutus scripts. (#3123)
- Use a separate connection for the `query tip` command. This fixes an
  occasional bug where the `query tip` command would fail. (#3130)
- Print the Tx fee when using the `tx build` command. (#3032)
- The `tx build` command now validates its inputs (ensuring they are in the UTxO
  and that only basic VKey-locked inputs are used as collateral.) (#3151)
- Add a new comment to query the stake pools. (#3152)
- `tx build` now uses the set of existing stake pools to determing if a pool is
  already registered (and hence whether it must pay a deposit). (#3152)
- `calculate-min-req-utxo` now requires a transaction output, not just a value
  as before. This is required in the Alonzo era, and the change is made
  everywhere for consistency. (#3181)
- Allow the `tx build` command to spend the entirety of a UTxO and create no
  change output. (#3188)
- Add withdrawals to the `tx view` command. (#2613)
## 1.29.0 -- August 2021

- Add a "tx build" command to the CLI. This command takes care of calculating
  the appropriate fee for a transaction, and balancing the transaction
  appropriately. It does not do input selection for fees, so sufficient balance
  must be available in the inputs to pay the computed fee, and sufficient
  collateral must also be present when phase-2 validating scripts are used. The
  tx build command is capable of computing both the fees required from
  transaction size and the fees incurred by script execution. (#2921, #2953,
  #2995, #3025)
- Improve the output format for rational fields in protocol parameters and
  genesis. When these are simple, we now convert them to decimal format. (#2992)
- Various internal improvements. (#2932)
- Make the CLI help text more nicely formatted. (#2945)
- Introduce the `--script-valid` and `--script-invalid` flags. The latter can be
  used to mark a script as being known invalid, such that the node will allow it
  to be submitted anyway (whereas under normal operation it would reject such a
  transaction in order to avoid loss of collateral). This flag is only likely to
  be of use in testing. The `--script-valid` flag is set as a default. (#3050,
  #3091, #3093)
- Add colours to the CLI output. (#3023)
## 1.28.0 -- July 2021

- The query tip command is now tidier and shows various additional pieces of
  information:
  - The epoch number is now shown during the Byron era.  Previously this worked
    only in the Shelley and subsequent eras. (#2688)
  - The sync progress of the node. This will only be available with new network
    protocols (not yet in this release.) (#2842, #2899)
  (#2885)
- Attempting to use an IPv6/IPv4 address where the other is expected will now
  give a more helpful error message. (#2691)
- Queries should now work during the Alonzo era. (#2727, #2755)
- Support for submitting transactions during the Alonzo era. (#2774, #2798,
  #2806, #2811, #2823, #2863, #2848)
- `cardano-cli genesis create` now also creates the new Alonzo genesis file.
  (#2743)
- The UTxO CLI query now allows an additional `--tx-in` flag which allows
  filtering the UTxO by TxIn, and requires the addition of the `--whole-utxo`
  flag to return the complete UTxO set (which was previously the default).
  Returning the whole UTxO set is an expensive operation only useful in small
  testnets, so we don't want it as the default option. (#2843, #2854)
- The parser for rational units (as used in for example execution unit prices)
  now supports rational syntax (e.g. 1/2). (#2922)

## 1.27.0 -- April 2021

- The query tip now also returns the era (e.g. Shelley, Allegra, Alonzo).
  (#2561, #2562, #2598)
- The `address build` command now incorporates the functionality of the script
  address build command, which is now deprecated. (#2486, #2587)
- Add additional commands for creating MIR certificates to the CLI. This
  supports the ability to transfer funds to the treasury for Catalyst projects.
  (#2503)
- As a result of refactoring in preparation for the upcoming Alonzo release,
  there are a couple of breaking changes in CLI commands referring to scripts:
  - Auxiliary scripts (i.e. those included in the Tx auxiliary data, which are
    not required as transaction signers) must now be included with
    `--auxiliary-script-file` rather than with `--script-file`.
  - Scripts witnessing txins, certificates, withdrawals and minting must now be
    paired with the thing they are witnessing. E.g.
    ```
    --certificate-file  $certfile --certificate-script-file $scriptfile
    --tx-out $txout --mint-script-file $scriptfile
    --withdrawal $withdrawal --withdrawal-script-file $scriptfile
    --tx-in $txin --txin-script-file $scriptfile
    ```
  - Scripts should now be specified when creating the txbody, rather than when
    signing the transaction. (#2547)
- The transaction view command now additionally shows detailed of minted
  non-native tokens. (#2550)
- Removed support for Byron addresses using the Bech32 encoding. The only
  supported way to use Byron-era addresses is through a file, using the text
  envelope format. (#2605)
- Add a new command which computes the minimum ADA value/deposit for a
  multi-asset value. (#2612)
- Add two new query commands:
  - `query stake-snapshot` allows querying the three stake snapshots for a given
    stake pool.
  - `query pool-params` returns the current and future parameters, as well as
    the retiring information.
  (#2560)
- Updated the CLI reference documentation. (#2665)

## 1.26.1 -- March 2021
- It's no longer necessary to specify the era when making a CLI query. When not
  specified, the current era will be used as a default. (#2470)

## 1.26.0 -- March 2021
- Add three new queries to the CLI, exposing functionality already present in
  the API:
  - Protocol parameters
  - Stake distribution
  - Individual stake addresses
  (#2275, #2290)
- Fix the rendering of Byron-era `TxOut`s to be consistent with the rendering for
  Shelley-era addresses. (#2472)
- Add `cardano-cli transaction view`, which allows for pretty-printing details
  about a serialised transaction. (#2348)
- When constructing MIR certificates, the CLI now takes stake addresses rather
  than stake certificates. These are strictly more general and can be deduced
  from the certificates.
- Make the Mary era the default era in the CLI (#2415)
- Migrate the `cardano-submit-api` tool from `cardano-rest`. (#2370)
- The 'tip' query now additionally returns the epoch at the tip (#2440)
- Various internal improvements and refactoring (#2458)

## 1.25.0 -- January 2021
- Allow creating transactions with no outputs (#2223, #2226)
- Improved error messages for syntax errors in out-of-range lovelace quantities
  in transaction outputs (#2063, #2079)
- Improved reference documentation for simple scripts and their use (#2165)
- Refactoring in the Byron part of the CLI to make more extensive use of the
  Cardano API and reduce the maintenance burden (#2103, #2228)
- Remove support for changing the delegation from Genesis keys to operational
  keys in the Byron era. This feature was never used on the mainnet during the
  Byron era. (#2219)
- Clearer usage information in the CLI `--help` output (#2203)

## 1.24.2 -- December 2020

- Rename the flags `--lower-bound` and `--upper-bound` to be `--invalid-before`
  and `--invalid-hereafter` respectively, for naming consistency (#2186, #2190)
- Hide the deprecated `--ttl` flag in the `--help` output (#2189, #2190)

## 1.24.1 -- December 2020

- New command `transaction policyid` for making multi-asset policy ids (#2176)
- New command `byron transaction txid` to help scripts with getting the
  transaction id for Byron transactions made using the cli (#2169)
- New `--tx-file` flag for the command `transaction txid` to accept complete
  txs, not just tx bodies (#2169)
- Add a regression test for the "0" case of multi-asset tx out values (#2155)

## 1.24.0 -- December 2020

- CLI support for the Allegra and Mary eras, including creating transactions
  for the new eras, and support for the special new features in the new eras:
  script extensions, tx validity intervals, auxiliary scripts, multi-asset tx
  outputs and asset minting. (#2072, #2129, #2136)
- New flags for the `build-raw` command:
  + `--invalid-before` and `--invalid-hereafter` for the new Allegra-era feature
    of transaction validity intervals. The existing flag `--ttl` is equivalent to
    the new `--invalid-hereafter`, but it is now optional in the Allegra era.
  + `--script-file` for the new Allegra-era feature of being able to include
     auxiliary scripts in a transaction.
  + `--mint` for the Mary-era token minting feature.
- It is now necessary to specify the target era (e.g. `--allegra-era`) when
  creating a transaction (with `build-raw`) so that the right format and
  feature-set is used. The `--shelley-era` remains the default.
- It is necessary for now to specify the target era when using the CLI query
  commands. This may become automatic in future. The default is `--shelley-era`.
- Move all the Shelley sub-commands to the top level of the command line.
  For example `cardano-cli shelley transaction build-raw` becomes simply
  `cardano-cli transaction build-raw`. The existing names are also kept for
  compatibility. (#2076, #2145)
- Updated help text for the ledger/protocol state queries to clarify that they
  are primarily for debugging and are not stable interfaces (#2125, #2126, #2133)
- New command `genesis create-staked` to make it easier to set up Shelley-based
  testnets with stake pools and delegation set up from the genesis. (#2052)

## 1.23.0 -- November 2020

- Create VRF keys with the correct file permissions (#1948)
- New command to query the Shelley protocol (not just ledger) state (#2057)
- Skeletons of the new commands and flags for the multi-asset extensions (#2081)

## 1.22.1 -- October 2020

None

## 1.22.0 -- October 2020

- Adjust the ledger state dump to return the "extended" ledger state (#2019)
- Preliminary support for the upcoming Allegra and Mary eras (#1958, #2019)

## 1.21.2 -- October 2020

- Support bech32 and hex formats for reading verification keys (#1852)
- Minor help text improvements (#1661, #1956)
- Fix typo in KES docs (#1917, #1953)
- Improved documentation for CLI multi-signature support (#1976)

## 1.21.1 -- September 2020

None

## 1.21.0 -- September 2020
- Support for multi-signature scripts (#1788, #1880)

## 1.20.0 -- September 2020

- New command for creating genesis key delegation certificates (#1784)
- New command for converting more legacy signing key formats (#1756, #1822)
- Improved support for JSON to Tx metadata conversions, with two supported
  JSON schemas, suitable for different use cases (#1797)
- Support bech32 and hex formats for reading signing keys (#1790)
- Improved error messages for cli errors (#1801, #1839)

## 1.19.1 -- September 2020

- Fix the testnet vs mainnet argument for the genesis create command (#1761)
- Fix the --treasury flag for MIR cert creation (#1780)
- Fix the output rendering in the command to hash genesis files (#1713, #1767)
- Validate CBOR tx metadata when building tx bodies (#1432, #1677)

## 1.19.0 -- August 2020

- Support for converting ITN extended keys to Shelley stake keys (#1579)
- Support for converting password-protected Byron signing keys (#1633)
- Support for building script addresses (#1641)
- Improve the output of the stake-address-info query (#1546, #1636, #1671)
- Support for Bech32-encoded stake pool IDs (#1528, #1638, #1730)
- Reorganise the Byron CLI commands similarly to the Shelley ones (#1609, #1628)
- Code organisation refactoring (#1457, #1594)
- Extra tests and refactoring of tests (#1565, #1566, #1602, #1668)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

## 1.18.0 -- July 2020

- Properly display the tx hash in the UTxO query command output (#1526, #1535)
- Refactoring and minor improvements in tests (#1538, #1541)

## 1.17.0 -- July 2020

- Allow genesis keys as tx witnesses (#1483)
- Allow extended genesis delegate keys to sign operational certs (#1497)
- New cli "key" command with key utilities (#1487, #1493)
- More helpful flag defaults in cli command for fee calculation (#1516)
- Default to the Cardano protocol for talking to a node (#1515)

## 1.16.0 -- July 2020

- Accept either a pool id or verification key in delegation cli command (#1460)
- Improved bash completion for flags that accept files (#1459)
- More and improved integration tests (#1429, #1450, #1453)

## 1.15.1 -- July 2020

- Support for interacting with nodes running in Byron-only, Shelley-only or
  the composite Cardano mode (Byron;Shelley) (#1435)
- Add support for byron keys and extended ed25519 keys (#1411)
- Port the CLI command implementations to the new API (#1416)
- Fix the output of the calculate-min-fee command (#1408)
- New stake and VRF key hashing commands (#1407)
- Use JSON output format for the address info command (#1426)

## 1.15.0 -- July 2020

- Fix the ledger state dump query (#1333, #1334)
- Fix the format of Byron addresses used in Byron CLI commands (#1326)
- Port CLI commands to use the new API (#1341, #1375, #1396, #1397)
- Change to JSON output for the "query tip" command (#1340, #1365)
- Moving code around to eliminate the cardano-config package (#1289, #1316)

## 1.14.2 -- June 2020

- Fix the hashing of stake pool metadata
- Fix the query that dumps the ledger state as JSON (#1333)

## 1.14.1 -- June 2020

No changes in the cardano-cli. There were changes in the cardano-node.

## 1.14.0 -- June 2020

- New flags for transaction metadata in tx construction (#1233)
- New flags for reward account withdrawals in tx construction (#1237)
- New command for pool metadata JSON validation and hashing (#1234, #1299)
- New flags for pool metadata in pool registration cert command (#1234)
- New flags for pool relays in pool registration cert command (#1282, #1296)
- New command to convert ITN keys (#1070, #1136)
- New command to get the txid of a tx body (#1231)
- Return appropriate exit code for tx submission failures (#1226)
- Fix the query stake-address-info to accept stake addresses (#1194, #1197)
- More regression tests (pioneer exercises 2, 3, 4) (#1209, #1247, #1279, #1287)
- Start to migrate to using the new typed API from cardano-api lib (#1284, #1298)
- Fix reporting of git revision via version command (#1283)

## 1.13.0 -- June 2020

- Fix the parsing of the pool margin in pool registration certs (#1063, #1110)
- Change the Shelley cli command and flag names to be more consistent (#1068)
- Add a command to query stake addresses, balance and delegation (#1053, #1129)
- Add a command to get the stake pool id (#1069)
- Add a command to create MIR certificates (#1075)
- Improved human readable error messages for Shelley commands (#1021)
- Improve error message for tx-in parser errors (#1066)
- Use a better default value of eMax in generated example genesis files (#1145)
- Regression tests covering the "pioneer" exercises 1 (#1073)
- Prerequisites for Tx metadata support (but not full support yet) (#1080)
- Updated Shelley from scratch documentation (#1062)

## 1.12.0 -- May 2020

- Reorganise the `shelley` subcommands (#840, #845)
- New `shelley genesis create` command (#852, #864, #908, #926, #929)
- New key-gen commands for various Shelley  keys (#846, #870)
- New commands for Shelley  address construction (#870, #872, #887)
- New Shelley transaction sign command (#894, #900)
- New Shelley transaction submission command (#904)
- New node query commands (#880, #884, #903, #918, #920, #933, #994, #1008, #1016)
- New commands to create stake address certificates (#890, #919, #967)
- New commands to create stake pool certificates (#922)
- New system commands to update genesis delgations and create MIR certs (#895)
- New command to calculate the minimum fee for a transaction (#931)
- New command to view the content of the various binary files (#915)
- New command to create Shelley protocol param updates (#950, #1004)
- Byron update proposal vote creation and submission (#804)
- Various refactoring (#874, #875, #949, #958, #966, #972)
- Commands that talk to the node no longer require the node config file (#901,
  #907, #917, #913, #928)
- Improved human readable error messages for Byron commands (#1003)
- Documentation on constructing a Shelley chain from scratch (#893, #932, #1000)
- Add `version` command and `--version` flag, with git revision (#959)
- Additional tests (#898, #935, #941, #952)


## 1.11.0 -- April 2020

- First version of the CLI as a separate package. The package provides a CLI
  (command line interface) to various low level node-related functionality.

  The CLI is not yet stable in this release.

- Split the `cardano-cli` package out of `cardano-node` (#819)
- Initial structure of Shelley CLI commands with a top-level "shelley" command
- Group Byron commands under a top-level "byron" command
- Commands to generate Shelley KES and VRF keys (#816)
- Command to generate Shelley address keys (#824)
