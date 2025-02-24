{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.CLI.Legacy.Run.Genesis
  ( runLegacyGenesisCmds
  )
where

import Cardano.Api
import Cardano.Api.Ledger (Coin (..))

import Cardano.CLI.EraBased.Commands.Genesis
  ( GenesisKeyGenGenesisCmdArgs (GenesisKeyGenGenesisCmdArgs)
  )
import Cardano.CLI.EraBased.Commands.Genesis qualified as Cmd
import Cardano.CLI.EraBased.Run.Genesis
import Cardano.CLI.EraBased.Run.Genesis.CreateTestnetData qualified as CreateTestnetData
import Cardano.CLI.Legacy.Commands.Genesis
import Cardano.CLI.Types.Common
import Cardano.CLI.Types.Errors.GenesisCmdError
import Cardano.Ledger.BaseTypes (NonZero)

import Data.Word (Word64)

runLegacyGenesisCmds :: LegacyGenesisCmds -> ExceptT GenesisCmdError IO ()
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
  -> ExceptT GenesisCmdError IO ()
runLegacyGenesisKeyGenGenesisCmd vk sk = CreateTestnetData.runGenesisKeyGenGenesisCmd $ GenesisKeyGenGenesisCmdArgs vk sk

runLegacyGenesisKeyGenDelegateCmd
  :: ()
  => VerificationKeyFile Out
  -> SigningKeyFile Out
  -> OpCertCounterFile Out
  -> ExceptT GenesisCmdError IO ()
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
  -> ExceptT GenesisCmdError IO ()
runLegacyGenesisKeyGenUTxOCmd vk sk =
  CreateTestnetData.runGenesisKeyGenUTxOCmd
    Cmd.GenesisKeyGenUTxOCmdArgs
      { Cmd.verificationKeyPath = vk
      , Cmd.signingKeyPath = sk
      }

runLegacyGenesisKeyHashCmd :: VerificationKeyFile In -> ExceptT GenesisCmdError IO ()
runLegacyGenesisKeyHashCmd = runGenesisKeyHashCmd

runLegacyGenesisVerKeyCmd
  :: VerificationKeyFile Out
  -> SigningKeyFile In
  -> ExceptT GenesisCmdError IO ()
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
  -> ExceptT GenesisCmdError IO ()
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
  -> ExceptT GenesisCmdError IO ()
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
  -> KeyOutputFormat
  -> GenesisDir
  -> Word
  -- ^ num genesis & delegate keys to make
  -> Word
  -- ^ num utxo keys to make
  -> Maybe SystemStart
  -> Maybe Coin
  -> NetworkId
  -> ExceptT GenesisCmdError IO ()
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
  -> ExceptT GenesisCmdError IO ()
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
  => EraInEon ShelleyBasedEra
  -> KeyOutputFormat
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
  -> ExceptT GenesisCmdError IO ()
runLegacyGenesisCreateStakedCmd
  (EraInEon sbe)
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
        { Cmd.eon = sbe
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
  -> ExceptT GenesisCmdError IO ()
runLegacyGenesisHashFileCmd = runGenesisHashFileCmd
