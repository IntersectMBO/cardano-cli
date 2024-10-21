{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.CLI.EraBased.HashChecking
  ( checkCertificateHashes
  , checkVotingProcedureHashes
  , checkProposalHashes
  )
where

import           Cardano.Api (Certificate (..), ExceptT, firstExceptT)
import qualified Cardano.Api.Ledger as L
import qualified Cardano.Api.Shelley as Shelley

import           Cardano.CLI.Run.Hash (carryHashChecks)
import           Cardano.CLI.Types.Common (MustCheckHash (..), PotentiallyCheckedAnchor (..))
import           Cardano.CLI.Types.Errors.TxCmdError (TxCmdError (..))
import qualified Cardano.Ledger.Api.Governance as L

import           Control.Monad (forM_)
import           Control.Monad.Trans.Except.Extra (left)

-- | Check the hash of the anchor data against the hash in the anchor
checkAnchorMetadataHash :: L.Anchor L.StandardCrypto -> ExceptT TxCmdError IO ()
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
checkCertificateHashes c =
  case c of
    ShelleyRelatedCertificate _ shelleyCert ->
      case shelleyCert of
        L.ShelleyTxCertDelegCert shelleyDelegCert ->
          case shelleyDelegCert of
            L.ShelleyRegCert _ -> return ()
            L.ShelleyUnRegCert _ -> return ()
            L.ShelleyDelegCert _ _ -> return ()
        L.ShelleyTxCertPool shelleyPoolCert ->
          case shelleyPoolCert of
            L.RegPool poolParams -> forM_ (L.ppMetadata poolParams) checkPoolMetadataHash
            L.RetirePool _ _ -> return ()
        L.ShelleyTxCertGenesisDeleg _ -> return ()
        L.ShelleyTxCertMir _ -> return ()
    ConwayCertificate ceo conwayCert ->
      Shelley.conwayEraOnwardsConstraints ceo $
        case conwayCert of
          L.ConwayTxCertDeleg _ -> return ()
          L.ConwayTxCertPool conwayPoolCert ->
            case conwayPoolCert of
              L.RegPool poolParams -> forM_ (L.ppMetadata poolParams) checkPoolMetadataHash
              L.RetirePool _ _ -> return ()
          L.ConwayTxCertGov govCert ->
            case govCert of
              L.ConwayRegDRep _ _ mAnchor -> forM_ mAnchor checkAnchorMetadataHash
              L.ConwayUnRegDRep _ _ -> return ()
              L.ConwayUpdateDRep _ mAnchor -> forM_ mAnchor checkAnchorMetadataHash
              L.ConwayAuthCommitteeHotKey _ _ -> return ()
              L.ConwayResignCommitteeColdKey _ mAnchor -> forM_ mAnchor checkAnchorMetadataHash
 where
  checkPoolMetadataHash :: L.PoolMetadata -> ExceptT TxCmdError IO ()
  checkPoolMetadataHash (L.PoolMetadata{L.pmUrl = url, L.pmHash = hashBytes}) = do
    let mHash = L.hashFromBytes hashBytes
    hash <- maybe (left $ TxCmdPoolMetadataHashError url) return mHash
    let safeHash = L.unsafeMakeSafeHash hash
    checkAnchorMetadataHash
      ( L.Anchor
          { L.anchorUrl = url
          , L.anchorDataHash = safeHash
          }
      )

-- | Find references to anchor data in voting procedures and check the hashes are valid
-- and they match the linked data.
checkVotingProcedureHashes
  :: Shelley.ShelleyBasedEra era -> Shelley.VotingProcedures era -> ExceptT TxCmdError IO ()
checkVotingProcedureHashes eon (Shelley.VotingProcedures (L.VotingProcedures voterMap)) =
  Shelley.shelleyBasedEraConstraints eon $
    forM_
      voterMap
      ( \vpMap ->
          forM_
            vpMap
            ( \(L.VotingProcedure _ mAnchor) ->
                forM_ mAnchor checkAnchorMetadataHash
            )
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
      checkGovActionHashes govAction
   where
    checkGovActionHashes
      :: L.GovAction (Shelley.ShelleyLedgerEra era) -> ExceptT TxCmdError IO ()
    checkGovActionHashes govAction' =
      Shelley.shelleyBasedEraConstraints eon $
        case govAction' of
          L.ParameterChange{} -> return ()
          L.HardForkInitiation _ _ -> return ()
          L.TreasuryWithdrawals _ _ -> return ()
          L.NoConfidence _ -> return ()
          L.UpdateCommittee{} -> return ()
          L.NewConstitution _ constitution -> checkAnchorMetadataHash $ L.constitutionAnchor constitution
          L.InfoAction -> return ()
