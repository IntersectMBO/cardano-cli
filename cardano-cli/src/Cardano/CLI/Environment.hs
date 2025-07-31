{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines constants derived from the environment.
module Cardano.CLI.Environment
  ( EnvCli (..)
  , envCliEra
  , getEnvCli
  , getEnvNetworkId
  , getEnvSocketPath
  )
where

import Cardano.Api
  ( AnyCardanoEra (..)
  , CardanoEra (..)
  , NetworkId (..)
  , NetworkMagic (..)
  )
import Cardano.Api.Experimental qualified as Exp

import Data.Word (Word32)
import System.Environment qualified as IO
import System.IO qualified as IO
import Text.Read (readMaybe)

data EnvCli = EnvCli
  { envCliNetworkId :: Maybe NetworkId
  , envCliSocketPath :: Maybe FilePath
  , envCliAnyCardanoEra :: Maybe AnyCardanoEra
  }

getEnvCli :: IO EnvCli
getEnvCli = do
  mNetworkId <- getEnvNetworkId
  mSocketPath <- getEnvSocketPath
  mCardanoEra <- getCardanoEra

  pure
    EnvCli
      { envCliNetworkId = mNetworkId
      , envCliSocketPath = mSocketPath
      , envCliAnyCardanoEra = mCardanoEra
      }

anyCardanoEraToEra :: AnyCardanoEra -> Maybe (Exp.Era Exp.ConwayEra)
anyCardanoEraToEra (AnyCardanoEra era) =
  case era of
    ByronEra -> Nothing
    ShelleyEra -> Nothing
    AllegraEra -> Nothing
    MaryEra -> Nothing
    AlonzoEra -> Nothing
    BabbageEra -> Nothing
    ConwayEra -> Just Exp.ConwayEra
    DijkstraEra -> Nothing

envCliEra :: EnvCli -> Maybe (Exp.Era Exp.ConwayEra)
envCliEra envCli = do
  anyCardanoEraToEra =<< envCliAnyCardanoEra envCli

-- | If the environment variable @CARDANO_NODE_NETWORK_ID@ is set, then return the network id therein.
-- Otherwise, return 'Nothing'.
getEnvNetworkId :: IO (Maybe NetworkId)
getEnvNetworkId = do
  mNetworkIdString <- IO.lookupEnv "CARDANO_NODE_NETWORK_ID"

  case mNetworkIdString of
    Nothing -> pure Nothing
    Just networkIdString -> do
      case networkIdString of
        "mainnet" -> pure $ Just Mainnet
        _ ->
          case readMaybe @Word32 networkIdString of
            Just networkId -> pure $ Just $ Testnet $ NetworkMagic networkId
            Nothing -> do
              IO.hPutStrLn IO.stderr $
                mconcat
                  [ "The network id specified in CARDANO_NODE_NETWORK_ID invalid: " <> networkIdString
                  , " It should be either 'mainnet' or a number."
                  ]
              pure Nothing

-- | If the environment variable @CARDANO_NODE_SOCKET_PATH@ is set, then return the set value.
-- Otherwise, return 'Nothing'.
getEnvSocketPath :: IO (Maybe FilePath)
getEnvSocketPath = IO.lookupEnv "CARDANO_NODE_SOCKET_PATH"

-- | If the environment variable @CARDANO_ERA@ is set, then return the set value.
-- Otherwise, return 'Nothing'.
getCardanoEra :: IO (Maybe AnyCardanoEra)
getCardanoEra = do
  mEraString <- IO.lookupEnv "CARDANO_ERA"

  case mEraString of
    Just eraString ->
      case eraString of
        "byron" -> pure $ Just $ AnyCardanoEra ByronEra
        "shelley" -> pure $ Just $ AnyCardanoEra ShelleyEra
        "allegra" -> pure $ Just $ AnyCardanoEra AllegraEra
        "mary" -> pure $ Just $ AnyCardanoEra MaryEra
        "alonzo" -> pure $ Just $ AnyCardanoEra AlonzoEra
        "babbage" -> pure $ Just $ AnyCardanoEra BabbageEra
        "conway" -> pure $ Just $ AnyCardanoEra ConwayEra
        unknown -> error $ "Unknown era: " <> unknown -- TODO improve error handling
    Nothing -> pure Nothing
