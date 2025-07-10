{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Legacy.Genesis.Run
  ( runLegacyGenesisCmds
  )
where

import Cardano.Api
import Cardano.Api.Experimental

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Genesis.Command
  ( GenesisKeyGenGenesisCmdArgs (GenesisKeyGenGenesisCmdArgs)
  )
import Cardano.CLI.EraBased.Genesis.Command qualified as Cmd
import Cardano.CLI.EraBased.Genesis.CreateTestnetData.Run qualified as CreateTestnetData
import Cardano.CLI.EraBased.Genesis.Run
import Cardano.CLI.Legacy.Genesis.Command
import Cardano.CLI.Type.Common
import Cardano.Ledger.BaseTypes (NonZero)

import RIO

import Vary (Vary)

runLegacyGenesisCmds :: LegacyGenesisCmds -> CIO e ()
runLegacyGenesisCmds = \case
  GenesisKeyGenGenesis vk sk ->
    runLegacyGenesisKeyGenGenesisCmd vk sk
  GenesisKeyGenDelegate vk sk ctr ->
    runLegacyGenesisKeyGenDelegateCmd vk sk ctr
  GenesisKeyGenUTxO vk sk ->
    runLegacyGenesisKeyGenUTxOCmd vk sk
  GenesisCmdKeyHash vk ->
    runLegacyGenesisKeyHashCmd vk
  GenesisVerKey vk sk ->
    runLegacyGenesisVerKeyCmd vk sk
  GenesisTxIn vk nw mOutFile ->
    runLegacyGenesisTxInCmd vk nw mOutFile
  GenesisAddr vk nw mOutFile ->
    runLegacyGenesisAddrCmd vk nw mOutFile
  GenesisCreate eSbe fmt gd gn un ms am nw ->
    runLegacyGenesisCreateCmd eSbe fmt gd gn un ms am nw
  GenesisCreateCardano eSbe gd gn un ms am k slotLength sc nw bg sg ag cg mNodeCfg ->
    runLegacyGenesisCreateCardanoCmd eSbe gd gn un ms am k slotLength sc nw bg sg ag cg mNodeCfg
  GenesisCreateStaked eSbe fmt gd gn gp gl un ms am ds nw bf bp su relayJsonFp ->
    runLegacyGenesisCreateStakedCmd eSbe fmt gd gn gp gl un ms am ds nw bf bp su relayJsonFp
  GenesisHashFile gf ->
    runLegacyGenesisHashFileCmd gf

runLegacyGenesisKeyGenGenesisCmd
  :: ()
  => VerificationKeyFile Out
  -> SigningKeyFile Out
  -> CIO e ()
runLegacyGenesisKeyGenGenesisCmd vk sk = CreateTestnetData.runGenesisKeyGenGenesisCmd $ GenesisKeyGenGenesisCmdArgs vk sk

runLegacyGenesisKeyGenDelegateCmd
  :: ()
  => VerificationKeyFile Out
  -> SigningKeyFile Out
  -> OpCertCounterFile Out
  -> CIO e ()
runLegacyGenesisKeyGenDelegateCmd vkf skf okf =
  CreateTestnetData.runGenesisKeyGenDelegateCmd
    Cmd.GenesisKeyGenDelegateCmdArgs
      { Cmd.verificationKeyPath = vkf
      , Cmd.signingKeyPath = skf
      , Cmd.opCertCounterPath = okf
      }

runLegacyGenesisKeyGenUTxOCmd
  :: ()
  => VerificationKeyFile Out
  -> SigningKeyFile Out
  -> CIO e ()
runLegacyGenesisKeyGenUTxOCmd vk sk =
  CreateTestnetData.runGenesisKeyGenUTxOCmd
    Cmd.GenesisKeyGenUTxOCmdArgs
      { Cmd.verificationKeyPath = vk
      , Cmd.signingKeyPath = sk
      }

runLegacyGenesisKeyHashCmd :: VerificationKeyFile In -> CIO e ()
runLegacyGenesisKeyHashCmd = runGenesisKeyHashCmd

runLegacyGenesisVerKeyCmd
  :: VerificationKeyFile Out
  -> SigningKeyFile In
  -> CIO e ()
runLegacyGenesisVerKeyCmd vk sk =
  runGenesisVerKeyCmd
    Cmd.GenesisVerKeyCmdArgs
      { Cmd.verificationKeyPath = vk
      , Cmd.signingKeyPath = sk
      }

runLegacyGenesisTxInCmd
  :: ()
  => VerificationKeyFile In
  -> NetworkId
  -> Maybe (File () Out)
  -> CIO e ()
runLegacyGenesisTxInCmd vkt nid mOf =
  runGenesisTxInCmd
    Cmd.GenesisTxInCmdArgs
      { Cmd.verificationKeyPath = vkt
      , Cmd.network = nid
      , Cmd.mOutFile = mOf
      }

runLegacyGenesisAddrCmd
  :: ()
  => VerificationKeyFile In
  -> NetworkId
  -> Maybe (File () Out)
  -> CIO e ()
runLegacyGenesisAddrCmd vkf nid mOf =
  runGenesisAddrCmd
    Cmd.GenesisAddrCmdArgs
      { Cmd.verificationKeyPath = vkf
      , Cmd.network = nid
      , Cmd.mOutFile = mOf
      }

runLegacyGenesisCreateCmd
  :: ()
  => EraInEon ShelleyBasedEra
  -> Vary [FormatBech32, FormatTextEnvelope]
  -> GenesisDir
  -> Word
  -- ^ num genesis & delegate keys to make
  -> Word
  -- ^ num utxo keys to make
  -> Maybe SystemStart
  -> Maybe Coin
  -> NetworkId
  -> CIO e ()
runLegacyGenesisCreateCmd (EraInEon asbe) fmt genDir nGenKeys nUTxOKeys mStart mSupply network =
  runGenesisCreateCmd
    Cmd.GenesisCreateCmdArgs
      { Cmd.eon = asbe
      , Cmd.keyOutputFormat = fmt
      , Cmd.genesisDir = genDir
      , Cmd.numGenesisKeys = nGenKeys
      , Cmd.numUTxOKeys = nUTxOKeys
      , Cmd.mSystemStart = mStart
      , Cmd.mSupply = mSupply
      , Cmd.network = network
      }

runLegacyGenesisCreateCardanoCmd
  :: ()
  => EraInEon ShelleyBasedEra
  -> GenesisDir
  -> Word
  -- ^ num genesis & delegate keys to make
  -> Word
  -- ^ num utxo keys to make
  -> Maybe SystemStart
  -> Maybe Coin
  -> NonZero Word64
  -> Word
  -- ^ slot length in ms
  -> Rational
  -> NetworkId
  -> FilePath
  -- ^ Byron Genesis
  -> FilePath
  -- ^ Shelley Genesis
  -> FilePath
  -- ^ Alonzo Genesis
  -> FilePath
  -- ^ Conway Genesis
  -> Maybe FilePath
  -> CIO e ()
runLegacyGenesisCreateCardanoCmd
  (EraInEon sbe)
  genDir
  nGenKeys
  nUTxOKeys
  mStart
  mSupply
  security
  slotLength
  slotCoeff
  network
  byronGenesis
  shelleyGenesis
  alonzoGenesis
  conwayGenesis
  mNodeCfg =
    runGenesisCreateCardanoCmd
      Cmd.GenesisCreateCardanoCmdArgs
        { Cmd.eon = sbe
        , Cmd.genesisDir = genDir
        , Cmd.numGenesisKeys = nGenKeys
        , Cmd.numUTxOKeys = nUTxOKeys
        , Cmd.mSystemStart = mStart
        , Cmd.mSupply = mSupply
        , Cmd.security = security
        , Cmd.slotLength = slotLength
        , Cmd.slotCoeff = slotCoeff
        , Cmd.network = network
        , Cmd.byronGenesisTemplate = byronGenesis
        , Cmd.shelleyGenesisTemplate = shelleyGenesis
        , Cmd.alonzoGenesisTemplate = alonzoGenesis
        , Cmd.conwayGenesisTemplate = conwayGenesis
        , Cmd.mNodeConfigTemplate = mNodeCfg
        }

runLegacyGenesisCreateStakedCmd
  :: ()
  => Era era
  -> Vary [FormatBech32, FormatTextEnvelope]
  -- ^ key output format
  -> GenesisDir
  -> Word
  -- ^ num genesis & delegate keys to make
  -> Word
  -- ^ num utxo keys to make
  -> Word
  -- ^ num pools to make
  -> Word
  -- ^ num delegators to make
  -> Maybe SystemStart
  -> Maybe Coin
  -- ^ supply going to non-delegators
  -> Coin
  -- ^ supply going to delegators
  -> NetworkId
  -> Word
  -- ^ bulk credential files to write
  -> Word
  -- ^ pool credentials per bulk file
  -> Word
  -- ^ num stuffed UTxO entries
  -> Maybe FilePath
  -- ^ Specified stake pool relays
  -> CIO e ()
runLegacyGenesisCreateStakedCmd
  era
  keyOutputFormat
  genesisDir
  numGenesisKeys
  numUTxOKeys
  numPools
  numStakeDelegators
  mSystemStart
  mNonDelegatedSupply
  delegatedSupply
  network
  numBulkPoolCredFiles
  numBulkPoolsPerFile
  numStuffedUtxo
  mStakePoolRelaySpecFile =
    runGenesisCreateStakedCmd
      Cmd.GenesisCreateStakedCmdArgs
        { Cmd.eon = era
        , Cmd.keyOutputFormat = keyOutputFormat
        , Cmd.genesisDir = genesisDir
        , Cmd.numGenesisKeys = numGenesisKeys
        , Cmd.numUTxOKeys = numUTxOKeys
        , Cmd.numPools = numPools
        , Cmd.numStakeDelegators = numStakeDelegators
        , Cmd.mSystemStart = mSystemStart
        , Cmd.mNonDelegatedSupply = mNonDelegatedSupply
        , Cmd.delegatedSupply = delegatedSupply
        , Cmd.network = network
        , Cmd.numBulkPoolCredFiles = numBulkPoolCredFiles
        , Cmd.numBulkPoolsPerFile = numBulkPoolsPerFile
        , Cmd.numStuffedUtxo = numStuffedUtxo
        , Cmd.mStakePoolRelaySpecFile = mStakePoolRelaySpecFile
        }

-- | Hash a genesis file
runLegacyGenesisHashFileCmd
  :: ()
  => GenesisFile
  -> CIO e ()
runLegacyGenesisHashFileCmd = runGenesisHashFileCmd
