{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Genesis.Internal.Byron where

import Cardano.Api.Byron (rationalToLovelacePortion)
import Cardano.Api.Byron qualified as Byron hiding (GenesisParameters)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as Shelley

import Cardano.CLI.Byron.Genesis qualified as Byron
import Cardano.Crypto.ProtocolMagic qualified as Crypto

import Data.Aeson (toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import GHC.Word (Word32)

-- | We need to pass these values to create the Byron genesis file.
-- The values here don't matter as the testnet conditions are ultimately determined
-- by the Shelley genesis.
defaultProtocolParamsJsonValue :: Aeson.Value
defaultProtocolParamsJsonValue =
  Aeson.object
    [ "heavyDelThd" .= toJSON @String "300000000000"
    , "maxBlockSize" .= toJSON @String "2000000"
    , "maxTxSize" .= toJSON @String "4096"
    , "maxHeaderSize" .= toJSON @String "2000000"
    , "maxProposalSize" .= toJSON @String "700"
    , "mpcThd" .= toJSON @String "20000000000000"
    , "scriptVersion" .= toJSON @Int 0
    , "slotDuration" .= toJSON @String "1000"
    , "softforkRule"
        .= Aeson.object
          [ "initThd" .= toJSON @String "900000000000000"
          , "minThd" .= toJSON @String "600000000000000"
          , "thdDecrement" .= toJSON @String "50000000000000"
          ]
    , "txFeePolicy"
        .= Aeson.object
          [ "multiplier" .= toJSON @String "43946000000"
          , "summand" .= toJSON @String "155381000000000"
          ]
    , "unlockStakeEpoch" .= toJSON @String "18446744073709551615"
    , "updateImplicit" .= toJSON @String "10000"
    , "updateProposalThd" .= toJSON @String "100000000000000"
    , "updateVoteThd" .= toJSON @String "1000000000000"
    ]

mkGenesisParameters
  :: Word -> Word32 -> FilePath -> Shelley.ShelleyGenesis -> Byron.GenesisParameters
mkGenesisParameters numPools actualNetworkWord32 byronGenesisFp shelleyGenesis =
  Byron.GenesisParameters{..}
 where
  -- All arbitrary values come from cardano-testnet
  byronPoolNumber = max 1 numPools -- byron genesis creation needs a >= 1 number of pools
  gpStartTime = Shelley.sgSystemStart shelleyGenesis
  gpProtocolParamsFile = byronGenesisFp
  gpK = Byron.BlockCount 10
  protocolMagicId = Crypto.ProtocolMagicId actualNetworkWord32
  gpProtocolMagic = Crypto.AProtocolMagic (L.Annotated protocolMagicId ()) Crypto.RequiresMagic
  gpTestnetBalance =
    Byron.TestnetBalanceOptions
      0 -- poor adresses
      byronPoolNumber -- delegate addresses (BFT nodes)
      (fromJust $ Byron.toByronLovelace $ L.Coin $ 3_000_000_000 * fromIntegral byronPoolNumber)
      1
  gpFakeAvvmOptions =
    Byron.FakeAvvmOptions
      0 -- avvm entry count
      (fromJust $ Byron.toByronLovelace $ L.Coin 0) -- avvm entry balance
  gpAvvmBalanceFactor = rationalToLovelacePortion $ 1 % 1
  gpSeed = Nothing
