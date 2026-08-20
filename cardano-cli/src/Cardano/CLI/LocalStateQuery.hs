{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.LocalStateQuery
  ( checkNodeNetworkId
  )
where

import Cardano.Api
import Cardano.Api.Network qualified as Consensus

import Cardano.CLI.Compatible.Exception (throwCliError)
import Cardano.CLI.Type.Error.NodeNetworkIdMismatchError

import Control.Exception (IOException, try)
import Control.Monad ((>=>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Either.Extra (eitherToMaybe)

-- | Check that the network id the CLI was given matches the network id in the
-- node's genesis, and throw a 'NodeNetworkIdMismatchError' otherwise. This is
-- meant to be run once, before a command that talks to the node.
--
-- The node-to-client handshake only compares network magics, so a 'NetworkId'
-- with the right magic but the wrong tag (for example
-- @CARDANO_NODE_NETWORK_ID=764824073@ instead of @CARDANO_NODE_NETWORK_ID=mainnet@)
-- connects successfully and would otherwise make the CLI render addresses for
-- the wrong network.
--
-- When there is no evidence of a mismatch (the node cannot be reached, it is
-- still in the Byron era, or it does not support the necessary queries) the
-- check passes, and connection problems are left to be reported by the command
-- itself.
checkNodeNetworkId :: MonadIO m => LocalNodeConnectInfo -> m ()
checkNodeNetworkId connectInfo = do
  result <-
    liftIO . try @IOException $
      executeLocalStateQueryExpr connectInfo Consensus.VolatileTip queryNodeNetworkId
  case result of
    Right (Right (Just nodeNetId))
      | nodeNetId /= cliNetId ->
          throwCliError $ NodeNetworkIdMismatchError cliNetId nodeNetId
    _ -> pure ()
 where
  cliNetId = localNodeNetworkId connectInfo

-- | The network id from the node's genesis, or 'Nothing' when it cannot be
-- obtained (the node is still in the Byron era, or it does not support the
-- necessary queries): the absence of an answer is not treated as a mismatch.
queryNodeNetworkId
  :: LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO (Maybe NetworkId)
queryNodeNetworkId = runMaybeT $ do
  AnyCardanoEra era <- MaybeT $ eitherToMaybe <$> queryCurrentEra
  sbe <- MaybeT . pure $ forEraMaybeEon era
  genesisParameters <- MaybeT $ (eitherToMaybe >=> eitherToMaybe) <$> queryGenesisParameters sbe
  pure $ protocolParamNetworkId genesisParameters
