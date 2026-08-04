{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.LocalStateQuery
  ( executeLocalStateQueryExprWithNetworkIdCheck
  )
where

import Cardano.Api
import Cardano.Api.Network qualified as Consensus

import Cardano.CLI.Compatible.Exception (throwCliError)
import Cardano.CLI.Type.Error.NodeNetworkIdMismatchError

import Control.Monad ((>=>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Either.Extra (eitherToMaybe)

-- | Like 'executeLocalStateQueryExpr', but before running the given expression
-- it checks that the network id the CLI was given matches the network id in the
-- node's genesis, and throws a 'NodeNetworkIdMismatchError' otherwise.
--
-- The node-to-client handshake only compares network magics, so a 'NetworkId'
-- with the right magic but the wrong tag (for example
-- @CARDANO_NODE_NETWORK_ID=764824073@ instead of @CARDANO_NODE_NETWORK_ID=mainnet@)
-- connects successfully and would otherwise make the CLI render addresses for
-- the wrong network.
executeLocalStateQueryExprWithNetworkIdCheck
  :: LocalNodeConnectInfo
  -> Consensus.Target ChainPoint
  -> LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO a
  -> IO (Either AcquiringFailure a)
executeLocalStateQueryExprWithNetworkIdCheck connectInfo target f =
  executeLocalStateQueryExpr connectInfo target $ do
    checkNodeNetworkId (localNodeNetworkId connectInfo)
    f

checkNodeNetworkId
  :: NetworkId
  -> LocalStateQueryExpr BlockInMode ChainPoint QueryInMode () IO ()
checkNodeNetworkId cliNetId =
  queryNodeNetworkId >>= \case
    Just nodeNetId
      | nodeNetId /= cliNetId ->
          throwCliError $ NodeNetworkIdMismatchError cliNetId nodeNetId
    _ -> pure ()

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
