{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.Transaction.HashCheck
  ( checkCertificateHashes
  , checkVotingProcedureHashes
  , checkProposalHashes
  )
where

import           Cardano.Api (Certificate (..), ExceptT, except, firstExceptT,
                   getAnchorDataFromCertificate, getAnchorDataFromGovernanceAction,
                   isDRepRegOrUpdateCert, validateGovActionAnchorData, withExceptT)
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Shelley as Shelley

import           Cardano.CLI.Run.Hash (carryHashChecks)
import           Cardano.CLI.Types.Common (MustCheckHash (..), PotentiallyCheckedAnchor (..))
import           Cardano.CLI.Types.Errors.TxCmdError (TxCmdError (..))

import           Control.Monad (forM_)
import           Data.ByteString (ByteString)

-- | Check the hash of the anchor data against the hash in the anchor
checkAnchorMetadataHash
  :: (ByteString -> Either String ()) -> L.Anchor L.StandardCrypto -> ExceptT TxCmdError IO ()
checkAnchorMetadataHash validationFunction anchor =
  firstExceptT (TxCmdHashCheckError $ L.anchorUrl anchor) $
    carryHashChecks
      validationFunction
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
  maybe
    (return mempty)
    ( checkAnchorMetadataHash
        ( if isDRepRegOrUpdateCert cert
            then validateGovActionAnchorData Shelley.CIP119
            else const $ return ()
        )
    )
    mAnchor

-- | Find references to anchor data in voting procedures and check the hashes are valid
-- and they match the linked data.
checkVotingProcedureHashes
  :: Shelley.ShelleyBasedEra era -> Shelley.VotingProcedures era -> ExceptT TxCmdError IO ()
checkVotingProcedureHashes eon (Shelley.VotingProcedures (L.VotingProcedures voterMap)) =
  Shelley.shelleyBasedEraConstraints eon $
    forM_
      voterMap
      ( mapM $ \(L.VotingProcedure _ mAnchor) ->
          forM_ mAnchor $ checkAnchorMetadataHash $ validateGovActionAnchorData Shelley.CIP108
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
      checkAnchorMetadataHash (validateGovActionAnchorData Shelley.CIP108) anchor
      maybe
        (return ())
        (checkAnchorMetadataHash $ validateGovActionAnchorData Shelley.CIP108)
        (getAnchorDataFromGovernanceAction govAction)

-- Only the `NewConstitution` governance action contains a checkable hash with a corresponding URL.
