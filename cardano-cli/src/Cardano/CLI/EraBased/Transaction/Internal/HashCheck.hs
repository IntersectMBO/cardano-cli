{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Transaction.Internal.HashCheck
  ( checkCertificateHashes
  , checkVotingProcedureHashes
  , checkProposalHashes
  )
where

import Cardano.Api
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.EraIndependent.Hash.Internal.Common (carryHashChecks)
import Cardano.CLI.Type.Common (MustCheckHash (..), PotentiallyCheckedAnchor (..))
import Cardano.CLI.Type.Error.TxCmdError (TxCmdError (..))

import Control.Monad (forM_)

-- | Check the hash of the anchor data against the hash in the anchor
checkAnchorMetadataHash :: L.Anchor -> ExceptT TxCmdError IO ()
checkAnchorMetadataHash anchor =
  firstExceptT (TxCmdHashCheckError $ L.anchorUrl anchor) $
    carryHashChecks
      ( PotentiallyCheckedAnchor
          { pcaMustCheck = CheckHash
          , pcaAnchor = anchor
          }
      )

-- | Find references to anchor data and check the hashes are valid
-- and they match the linked data.
checkCertificateHashes :: Certificate era -> ExceptT TxCmdError IO ()
checkCertificateHashes cert = do
  mAnchor <- withExceptT TxCmdPoolMetadataHashError $ except $ getAnchorDataFromCertificate cert
  maybe (return mempty) checkAnchorMetadataHash mAnchor

-- | Find references to anchor data in voting procedures and check the hashes are valid
-- and they match the linked data.
checkVotingProcedureHashes
  :: ShelleyBasedEra era -> VotingProcedures era -> ExceptT TxCmdError IO ()
checkVotingProcedureHashes eon (VotingProcedures (L.VotingProcedures voterMap)) =
  shelleyBasedEraConstraints eon $
    forM_
      voterMap
      ( mapM $ \(L.VotingProcedure _ mAnchor) ->
          forM_ mAnchor checkAnchorMetadataHash
      )

-- | Find references to anchor data in proposals and check the hashes are valid
-- and they match the linked data.
checkProposalHashes
  :: forall era. ShelleyBasedEra era -> Proposal era -> ExceptT TxCmdError IO ()
checkProposalHashes
  eon
  ( Proposal
      ( L.ProposalProcedure
          { L.pProcGovAction = govAction
          , L.pProcAnchor = anchor
          }
        )
    ) =
    shelleyBasedEraConstraints eon $ do
      checkAnchorMetadataHash anchor
      maybe (return ()) checkAnchorMetadataHash (getAnchorDataFromGovernanceAction govAction)

-- Only the `NewConstitution` governance action contains a checkable hash with a corresponding URL.
