{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Run.Governance.Query
  ( runGovernanceQueryCmds
  , GovernanceQueryError(..)
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as Ledger

import qualified Cardano.CLI.EraBased.Commands.Governance.Query as Cmd
import           Cardano.CLI.Read
import           Cardano.CLI.Types.Errors.CmdError
import           Cardano.CLI.Types.Errors.GovernanceQueryError
import qualified Ouroboros.Consensus.Cardano.Block as Consensus

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
import           Data.Aeson ((.=))
import qualified Data.Aeson as A
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor (second)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Function
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Lens.Micro ((^.))


runGovernanceQueryCmds
  :: Cmd.GovernanceQueryCmds era
  -> ExceptT CmdError IO ()
runGovernanceQueryCmds = firstExceptT CmdGovernanceQueryError . \case
  Cmd.GovernanceQueryConstitutionCmd cOn args           -> runQueryConstitution cOn args
  Cmd.GovernanceQueryGovStateCmd cOn args               -> runQueryGovState cOn args
  Cmd.GovernanceQueryDRepStateCmd cOn args              -> runQueryDRepState cOn args
  Cmd.GovernanceQueryDRepStakeDistributionCmd cOn args  -> runQueryDRepStakeDistribution cOn args
  Cmd.GovernanceQueryCommitteeStateCmd cOn args         -> runQueryCommitteeState cOn args

runQueryConstitution
  :: ConwayEraOnwards era
  -> Cmd.NoArgQueryCmd
  -> ExceptT GovernanceQueryError IO ()
runQueryConstitution w (Cmd.NoArgQueryCmd socketPath (AnyConsensusModeParams cModeParams) network mFile) = conwayEraOnwardsConstraints w $ do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath
      sbe = conwayEraOnwardsToShelleyBasedEra w
      cEra = conwayEraOnwardsToCardanoEra w
      cMode = consensusModeOnly cModeParams

  eraInMode <- toEraInMode cEra cMode
    & hoistMaybe (GovernanceQueryEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra cEra))

  constitution <- runQuery localNodeConnInfo $ queryConstitution eraInMode sbe
  writeOutput mFile constitution

runQueryGovState
  :: ConwayEraOnwards era
  -> Cmd.NoArgQueryCmd
  -> ExceptT GovernanceQueryError IO ()
runQueryGovState w (Cmd.NoArgQueryCmd socketPath (AnyConsensusModeParams cModeParams) network mFile) = conwayEraOnwardsConstraints w $ do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath
      sbe = conwayEraOnwardsToShelleyBasedEra w
      cEra = conwayEraOnwardsToCardanoEra w
      cMode = consensusModeOnly cModeParams

  eraInMode <- toEraInMode cEra cMode
    & hoistMaybe (GovernanceQueryEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra cEra))

  govState <- runQuery localNodeConnInfo $ queryGovState eraInMode sbe
  writeOutput mFile govState

runQueryDRepState
  :: ConwayEraOnwards era
  -> Cmd.DRepStateQueryCmd
  -> ExceptT GovernanceQueryError IO ()
runQueryDRepState w (Cmd.DRepStateQueryCmd socketPath (AnyConsensusModeParams cModeParams) network drepKeys mFile) = conwayEraOnwardsConstraints w $ do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath
      sbe = conwayEraOnwardsToShelleyBasedEra w
      cEra = conwayEraOnwardsToCardanoEra w
      cMode = consensusModeOnly cModeParams

  eraInMode <- toEraInMode cEra cMode
    & hoistMaybe (GovernanceQueryEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra cEra))

  drepCreds <- Set.fromList <$> mapM (firstExceptT GovernanceQueryDRepKeyError . getDRepCredentialFromVerKeyHashOrFile) drepKeys

  drepState <- runQuery localNodeConnInfo $ queryDRepState eraInMode sbe drepCreds
  writeOutput mFile $
    second drepStateToJson <$> Map.assocs drepState
  where
    drepStateToJson ds = A.object
      [ "expiry" .= (ds ^. Ledger.drepExpiryL)
      , "anchor" .= (ds ^. Ledger.drepAnchorL)
      , "deposit" .= (ds ^. Ledger.drepDepositL)
      ]

runQueryDRepStakeDistribution
  :: ConwayEraOnwards era
  -> Cmd.DRepStakeDistributionQueryCmd
  -> ExceptT GovernanceQueryError IO ()
runQueryDRepStakeDistribution w (Cmd.DRepStakeDistributionQueryCmd socketPath (AnyConsensusModeParams cModeParams) network drepKeys mFile) = conwayEraOnwardsConstraints w $ do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath
      sbe = conwayEraOnwardsToShelleyBasedEra w
      cEra = conwayEraOnwardsToCardanoEra w
      cMode = consensusModeOnly cModeParams

  let drepFromVrfKey = fmap Ledger.DRepCredential
                     . firstExceptT GovernanceQueryDRepKeyError
                     . getDRepCredentialFromVerKeyHashOrFile
  dreps <- Set.fromList <$> mapM drepFromVrfKey drepKeys

  eraInMode <- toEraInMode cEra cMode
    & hoistMaybe (GovernanceQueryEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra cEra))

  drepStakeDistribution <- runQuery localNodeConnInfo $ queryDRepStakeDistribution eraInMode sbe dreps
  writeOutput mFile $
    Map.assocs drepStakeDistribution

runQueryCommitteeState
  :: ConwayEraOnwards era
  -> Cmd.NoArgQueryCmd
  -> ExceptT GovernanceQueryError IO ()
runQueryCommitteeState w (Cmd.NoArgQueryCmd socketPath (AnyConsensusModeParams cModeParams) network mFile) = conwayEraOnwardsConstraints w $ do
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network socketPath
      sbe = conwayEraOnwardsToShelleyBasedEra w
      cEra = conwayEraOnwardsToCardanoEra w
      cMode = consensusModeOnly cModeParams

  eraInMode <- toEraInMode cEra cMode
    & hoistMaybe (GovernanceQueryEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra cEra))

  committeeState <- runQuery localNodeConnInfo $ queryCommitteeState eraInMode sbe
  writeOutput mFile $
    Map.assocs $ committeeState ^. Ledger.csCommitteeCredsL

runQuery :: LocalNodeConnectInfo mode
         -> LocalStateQueryExpr
             (BlockInMode mode)
             ChainPoint
             (QueryInMode mode)
             ()
             IO
             (Either
                UnsupportedNtcVersionError
                (Either Consensus.EraMismatch a))
         -> ExceptT GovernanceQueryError IO a
runQuery localNodeConnInfo query =
  firstExceptT GovernanceQueryAcqireFailureError
    ( newExceptT $ executeLocalStateQueryExpr localNodeConnInfo Nothing query)
      & onLeft (left . GovernanceQueryUnsupportedNtcVersion)
      & onLeft (left . GovernanceQueryEraMismatch)

writeOutput :: ToJSON b
            => Maybe (File a Out)
            -> b
            -> ExceptT GovernanceQueryError IO ()
writeOutput mFile content = case mFile of
  Nothing -> liftIO . LBS.putStrLn . encodePretty $ content
  Just (File f) ->
    handleIOExceptT (GovernanceQueryWriteFileError . FileIOError f) $
      LBS.writeFile f (encodePretty content)

