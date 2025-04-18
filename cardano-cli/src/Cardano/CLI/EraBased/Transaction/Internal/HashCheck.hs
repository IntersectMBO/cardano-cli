{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Transaction.Internal.HashCheck
  ( checkCertificateHashes
  , checkVotingProcedureHashes
  , checkProposalHashes
  )
where

import Cardano.Api
  ( Certificate (..)
  , ExceptT
  , except
  , firstExceptT
  , getAnchorDataFromCertificate
  , getAnchorDataFromGovernanceAction
  , withExceptT
  )
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley qualified as Shelley

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
  :: Shelley.ShelleyBasedEra era -> Shelley.VotingProcedures era -> ExceptT TxCmdError IO ()
checkVotingProcedureHashes eon (Shelley.VotingProcedures (L.VotingProcedures voterMap)) =
  Shelley.shelleyBasedEraConstraints eon $
    forM_
      voterMap
      ( mapM $ \(L.VotingProcedure _ mAnchor) ->
          forM_ mAnchor checkAnchorMetadataHash
      )

-- | Find references to anchor data in proposals and check the hashes are valid
-- and they match the linked data.
checkProposalHashes
  :: forall era. Shelley.ShelleyBasedEra era -> Shelley.Proposal era -> ExceptT TxCmdError IO ()
checkProposalHashes
  eon
  ( Shelley.Proposal
      ( L.ProposalProcedure
          { L.pProcGovAction = govAction
          , L.pProcAnchor = anchor
          }
        )
    ) =
    Shelley.shelleyBasedEraConstraints eon $ do
      checkAnchorMetadataHash anchor
      maybe (return ()) checkAnchorMetadataHash (getAnchorDataFromGovernanceAction govAction)

-- Only the `NewConstitution` governance action contains a checkable hash with a corresponding URL.
