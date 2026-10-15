{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.Compatible.Transaction.Run
  ( runCompatibleTransactionCmd
  )
where

import Cardano.Api hiding (VotingProcedures)
import Cardano.Api qualified as OldApi
import Cardano.Api.Compatible
import Cardano.Api.Compatible.Certificate qualified as Compatible
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as Exp.Plutus
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L hiding
  ( VotingProcedures
  )

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Read qualified as Compatible
import Cardano.CLI.Compatible.Transaction.Command
import Cardano.CLI.Compatible.Transaction.TxOut
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraBased.Transaction.Internal.Common (partitionSomeWitnesses)
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.ProtocolParamsError

import Control.Monad
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Lens.Micro

runCompatibleTransactionCmd
  :: forall era e
   . CompatibleTransactionCmds era
  -> CIO e ()
runCompatibleTransactionCmd
  ( CreateCompatibleSignedTransaction
      sbe
      ins
      insCollateral
      outs
      mUpdateProposal
      proposals
      mVotes
      witnesses
      mNetworkId
      fee
      certificates
      mProtocolParamsFile
      outputFp
    ) = shelleyBasedEraConstraints sbe $ do
    sks <- mapM (fromEitherIOCli . readWitnessSigningData) witnesses

    (allOuts, extraDatumsMapList) <- mapAndUnzipM (toTxOutInAnyEra sbe) outs
    let extraDatums = Map.unions extraDatumsMapList

    certFilesAndMaybeScriptWits <-
      readCertificateScriptWitnesses' sbe certificates

    certsAndMaybeScriptWits <-
      liftIO $
        sequenceA
          [ fmap (,mSwit) $
              fromEitherIOCli $
                readFileTextEnvelope $
                  File certFile
          | (CertificateFile certFile, mSwit) <- certFilesAndMaybeScriptWits
          ]

    (protocolUpdates, votes) :: (AnyProtocolUpdate era, AnyVote era) <-
      inEonForShelleyBasedEra
        ( do
            pparamUpdate <-
              fromMaybe (NoPParamsUpdate sbe) <$> forM mUpdateProposal (readUpdateProposalFile sbe)
            return (pparamUpdate, NoVotes)
        )
        ( \w -> do
            pparamUpdate <- readProposalProcedureFile sbe proposals
            votesAndWits :: [(OldApi.VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))] <-
              obtainCommonConstraints (convert w) $ readVotingProceduresFiles mVotes
            votingProcedures :: (Exp.TxVotingProcedures (Exp.LedgerEra era)) <-
              obtainCommonConstraints (convert w) $
                fromEitherCli
                  ( Exp.mkTxVotingProcedures
                      [ (obtainCommonConstraints (convert w) $ OldApi.unVotingProcedures vp, anyW)
                      | (vp, anyW) <- votesAndWits
                      ]
                  )
            return (pparamUpdate, VotingProcedures w $ obtainCommonConstraints (convert w) votingProcedures)
        )
        sbe

    let txCerts = mkTxCertificatesSbe sbe certsAndMaybeScriptWits

    pparams <- forM mProtocolParamsFile $ \(ProtocolParamsFile path) -> do
      bytes <- readFileCli path
      fromEitherCli $ first (ProtocolParamsErrorJSON path . Text.pack) $ Aeson.eitherDecodeStrict' bytes

    transaction@(ShelleyTx _ ledgerTx) <-
      fromEitherCli $
        createCompatibleTx sbe $
          (defaultCompatibleTxBodyContent sbe)
            { compatibleTxIns = [(txIn, Exp.AnyKeyWitnessPlaceholder) | txIn <- ins]
            , compatibleTxOuts = allOuts
            , compatibleTxSupplementalDatums = extraDatums
            , compatibleTxFee = fee
            , compatibleTxProtocolUpdate = protocolUpdates
            , compatibleTxVotingProcedures = votes
            , compatibleTxCertificates = txCerts
            , compatibleTxInsCollateral = insCollateral
            , compatibleTxProtocolParams = pparams
            }

    let txBody = ledgerTx ^. L.bodyTxL

    let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeSigningWitness sks

    byronWitnesses <-
      forM sksByron $
        fromEitherCli
          . mkShelleyBootstrapWitness sbe mNetworkId txBody

    let newShelleyKeyWits = makeShelleyKeyWitness' sbe txBody <$> sksShelley
        allKeyWits = newShelleyKeyWits ++ byronWitnesses
        signedTx = addWitnesses allKeyWits transaction

    fromEitherIOCli $
      writeTxFileTextEnvelope sbe outputFp signedTx

readCertificateScriptWitnesses'
  :: ShelleyBasedEra era
  -> [(CertificateFile, Maybe AnyNonAssetScript)]
  -> CIO e [(CertificateFile, Exp.AnyWitness (ShelleyLedgerEra era))]
readCertificateScriptWitnesses' sbe =
  mapM
    ( \(certFile, mSWit) -> do
        case mSWit of
          Nothing -> return (certFile, Exp.AnyKeyWitnessPlaceholder)
          Just cert -> do
            sWit <- readCertificateScriptWitnessSbe sbe cert
            return (certFile, sWit)
    )

readCertificateScriptWitnessSbe
  :: forall era e
   . ShelleyBasedEra era
  -> AnyNonAssetScript
  -> CIO e (Exp.AnyWitness (ShelleyLedgerEra era))
readCertificateScriptWitnessSbe sbe (AnyNonAssetScriptSimple simpleReq) =
  case simpleReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      ss <- Compatible.readFileSimpleScript sFp
      let serialisedSS = serialiseToCBOR ss
      let simpleScriptE :: Either DecoderError (Exp.SimpleScript (ShelleyLedgerEra era)) = shelleyBasedEraConstraints sbe $ Exp.deserialiseSimpleScript serialisedSS
      simpleScript <- fromEitherCli simpleScriptE
      return $ Exp.AnySimpleScriptWitness $ Exp.SScript simpleScript
    ReferenceSimpleScript refTxin ->
      return . Exp.AnySimpleScriptWitness $ Exp.SReferenceScript refTxin
readCertificateScriptWitnessSbe sbe (AnyNonAssetScriptPlutus plutusReq) =
  case plutusReq of
    OnDiskPlutusNonAssetScript scriptFp redeemerFile execUnits -> do
      let plutusScriptFp = unFile scriptFp
      Exp.Plutus.AnyPlutusScript anyPlutusScript <- Compatible.readFilePlutusScript sbe plutusScriptFp
      let
        lang = Exp.Plutus.plutusScriptInEraSLanguage anyPlutusScript
      let script' = Exp.PScript anyPlutusScript

      redeemer <-
        fromExceptTCli $
          readScriptDataOrFile redeemerFile

      let sw =
            Exp.Plutus.PlutusScriptWitness
              lang
              script'
              Exp.Plutus.NoScriptDatum
              redeemer
              execUnits
      return $
        Exp.AnyPlutusScriptWitness $
          Exp.AnyPlutusCertifyingScriptWitness sw
    ReferencePlutusNonAssetScript refInput (AnySLanguage lang) redeemerFile execUnits -> do
      redeemer <-
        fromExceptTCli $
          readScriptDataOrFile redeemerFile
      return $
        Exp.AnyPlutusScriptWitness $
          Exp.AnyPlutusCertifyingScriptWitness $
            Exp.Plutus.PlutusScriptWitness
              lang
              (Exp.Plutus.PReferenceScript refInput)
              Exp.Plutus.NoScriptDatum
              redeemer
              execUnits

-- | Create 'TxCertificates'. Note that 'Certificate era' will be deduplicated. A witness is attached
-- only to certificates which require one.
--
-- Note that, when building a transaction in Conway era, a witness is not required for staking credential
-- registration, but this is only the case during the transitional period of Conway era and only for staking
-- credential registration certificates without a deposit. Future eras will require a witness for
-- registration certificates, because the one without a deposit will be removed.
mkTxCertificatesSbe
  :: forall era
   . ShelleyBasedEra era
  -> [(Exp.Certificate (ShelleyLedgerEra era), Exp.AnyWitness (ShelleyLedgerEra era))]
  -> Exp.TxCertificates (ShelleyLedgerEra era)
mkTxCertificatesSbe era certs = Exp.TxCertificates . OMap.fromList $ map attachWitness certs
 where
  attachWitness
    :: (Exp.Certificate (ShelleyLedgerEra era), Exp.AnyWitness (ShelleyLedgerEra era))
    -> ( Exp.Certificate (ShelleyLedgerEra era)
       , Maybe (Exp.AnyWitness (ShelleyLedgerEra era))
       )
  attachWitness (c@(Exp.Certificate cert), wit) =
    (c, wit <$ Compatible.getTxCertWitness (convert era) cert)

readUpdateProposalFile
  :: forall e era
   . ShelleyBasedEra era
  -> UpdateProposalFile
  -> CIO e (AnyProtocolUpdate era)
readUpdateProposalFile sbe (UpdateProposalFile file) =
  case sbe of
    ShelleyBasedEraShelley -> ProtocolUpdate ShelleyToBabbageEraShelley <$> readUpdateProposal
    ShelleyBasedEraAllegra -> ProtocolUpdate ShelleyToBabbageEraAllegra <$> readUpdateProposal
    ShelleyBasedEraMary -> ProtocolUpdate ShelleyToBabbageEraMary <$> readUpdateProposal
    ShelleyBasedEraAlonzo -> ProtocolUpdate ShelleyToBabbageEraAlonzo <$> readUpdateProposal
    ShelleyBasedEraBabbage -> ProtocolUpdate ShelleyToBabbageEraBabbage <$> readUpdateProposal
    ShelleyBasedEraConway -> pure $ NoPParamsUpdate sbe
    ShelleyBasedEraDijkstra -> pure $ NoPParamsUpdate sbe
 where
  readUpdateProposal :: CIO e (UpdateProposal era)
  readUpdateProposal =
    fromEitherIOCli @(FileError TextEnvelopeError) $
      shelleyBasedEraConstraints sbe $
        readFileTextEnvelope (File file)

readProposalProcedureFile
  :: forall e era
   . ShelleyBasedEra era
  -> [(ProposalFile In, Maybe AnyNonAssetScript)]
  -> CIO e (AnyProtocolUpdate era)
readProposalProcedureFile sbe [] = return $ NoPParamsUpdate sbe
readProposalProcedureFile sbe proposals =
  case sbe of
    ShelleyBasedEraShelley -> return $ NoPParamsUpdate sbe
    ShelleyBasedEraAllegra -> return $ NoPParamsUpdate sbe
    ShelleyBasedEraMary -> return $ NoPParamsUpdate sbe
    ShelleyBasedEraAlonzo -> return $ NoPParamsUpdate sbe
    ShelleyBasedEraBabbage -> return $ NoPParamsUpdate sbe
    ShelleyBasedEraConway -> ProposalProcedures ConwayEraOnwardsConway <$> readProcedures
    ShelleyBasedEraDijkstra -> ProposalProcedures ConwayEraOnwardsDijkstra <$> readProcedures
 where
  readProcedures
    :: forall era'
     . Exp.IsEra era'
    => CIO e (Exp.TxProposalProcedures (ShelleyLedgerEra era'))
  readProcedures = Exp.obtainCommonConstraints (Exp.useEra @era') $ do
    props :: [(Proposal era', Exp.AnyWitness (Exp.LedgerEra era'))] <-
      mapM readProposal proposals
    pure $
      Exp.mkTxProposalProcedures
        [(govProp, swit) | (Proposal govProp, swit) <- props]
