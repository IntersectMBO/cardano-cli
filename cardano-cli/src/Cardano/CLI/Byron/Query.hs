{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.Byron.Query
  ( runGetLocalNodeTip
  ) where

import Cardano.Api

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LB
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text

{- HLINT ignore "Reduce duplication" -}

--------------------------------------------------------------------------------
-- Query local node's chain tip
--------------------------------------------------------------------------------

runGetLocalNodeTip
  :: SocketPath
  -> NetworkId
  -> IO ()
runGetLocalNodeTip socketPath networkId = do
  let connctInfo =
        LocalNodeConnectInfo
          { localNodeSocketPath = socketPath
          , localNodeNetworkId = networkId
          , localConsensusModeParams = ByronModeParams (EpochSlots 21600)
          }

  tip <- getLocalChainTip connctInfo
  Text.putStrLn . Text.decodeUtf8 . LB.toStrict $ encodePretty tip
