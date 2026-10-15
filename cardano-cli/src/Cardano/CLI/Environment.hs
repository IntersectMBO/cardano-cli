{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  , NetworkId (..)
  , NetworkMagic (..)
  , forEraMaybeEon
  , pretty
  )
import Cardano.Api.Experimental qualified as Exp

import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (toLower)
import Data.Foldable (find)
import Data.List (intercalate)
import Data.Traversable (forM)
import Data.Type.Equality
import Data.Word (Word32)
import GHC.Stack (HasCallStack)
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

envCliEra
  :: forall era
   . Exp.IsEra era
  => EnvCli
  -> Maybe (Exp.Era era)
envCliEra envCli = do
  AnyCardanoEra cardanoEra <- envCliAnyCardanoEra envCli
  era1 <- forEraMaybeEon cardanoEra
  Refl <- testEquality era1 (Exp.useEra @era)
  pure era1

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

-- | If the environment variable @CARDANO_ERA@ is set to a known era, then return that era.
-- If it is set to an unknown era, fail with an error listing the valid eras.
-- Otherwise, return 'Nothing'.
getCardanoEra :: (HasCallStack, MonadIO m) => m (Maybe AnyCardanoEra)
getCardanoEra = do
  mEraString <- liftIO $ IO.lookupEnv "CARDANO_ERA"
  forM mEraString $ \eraString ->
    maybe (error $ invalidEraMessage eraString) pure $ parseCardanoEra eraString
 where
  invalidEraMessage :: String -> String
  invalidEraMessage eraString =
    mconcat
      [ "The era specified in CARDANO_ERA is invalid: " <> eraString <> "."
      , " It should be one of: " <> intercalate ", " (cardanoEraName <$> allCardanoEras) <> "."
      ]

  -- Parse an era name, ignoring case, using the names that cardano-api renders for its eras.
  parseCardanoEra :: String -> Maybe AnyCardanoEra
  parseCardanoEra eraString =
    find ((== map toLower eraString) . cardanoEraName) allCardanoEras

  allCardanoEras :: [AnyCardanoEra]
  allCardanoEras = enumFrom minBound

  cardanoEraName :: AnyCardanoEra -> String
  cardanoEraName = map toLower . show . pretty
