{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import Cardano.Api.Experimental.Plutus qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L hiding
  ( VotingProcedures
  )

import Cardano.Binary
import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Read qualified as Compatible
import Cardano.CLI.Compatible.Transaction.Command
import Cardano.CLI.Compatible.Transaction.TxOut
import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Proposal.Read
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Vote.Read
import Cardano.CLI.EraBased.Transaction.Run
import Cardano.CLI.Read
import Cardano.CLI.Type.Common

import Control.Monad
import Data.ByteString.Short qualified as SBS
import Data.Map.Ordered.Strict qualified as OMap
import Data.Typeable
import Lens.Micro

runCompatibleTransactionCmd
  :: forall era e
   . CompatibleTransactionCmds era
  -> CIO e ()
runCompatibleTransactionCmd
  ( CreateCompatibleSignedTransaction
      sbe
      ins
      outs
      mUpdateProposal
      mProposalProcedure
      mVotes
      witnesses
      mNetworkId
      fee
      certificates
      outputFp
    ) = shelleyBasedEraConstraints sbe $ do
    sks <- mapM (fromEitherIOCli . readWitnessSigningData) witnesses

    allOuts <- mapM (toTxOutInAnyEra sbe) outs

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
      caseShelleyToBabbageOrConwayEraOnwards
        ( const $ do
            case mUpdateProposal of
              Nothing -> return (NoPParamsUpdate sbe, NoVotes)
              Just p -> do
                pparamUpdate <- readUpdateProposalFile p
                return (pparamUpdate, NoVotes)
        )
        ( \w ->
            case mProposalProcedure of
              Nothing -> return (NoPParamsUpdate sbe, NoVotes)
              Just prop -> do
                pparamUpdate <- readProposalProcedureFile prop
                votesAndWits :: [(OldApi.VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))] <-
                  obtainCommonConstraints (convert w) $ readVotingProceduresFiles mVotes
                votingProcedures :: (Exp.TxVotingProcedures (Exp.LedgerEra era)) <-
                  obtainTypeable w $
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

    transaction@(ShelleyTx _ ledgerTx) <-
      fromEitherCli $
        createCompatibleTx sbe ins allOuts fee protocolUpdates votes txCerts

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
  -> [(CertificateFile, Maybe (ScriptRequirements Exp.CertItem))]
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
  -> ScriptRequirements Exp.CertItem
  -> CIO e (Exp.AnyWitness (ShelleyLedgerEra era))
readCertificateScriptWitnessSbe sbe (OnDiskSimpleScript scriptFp) = do
  let sFp = unFile scriptFp
  ss <- Compatible.readFileSimpleScript sFp
  let serialisedSS = serialiseToCBOR ss
  let simpleScriptE :: Either DecoderError (Exp.SimpleScript (ShelleyLedgerEra era)) = shelleyBasedEraConstraints sbe $ Exp.deserialiseSimpleScript serialisedSS
  simpleScript <- fromEitherCli simpleScriptE
  return $ Exp.AnySimpleScriptWitness $ Exp.SScript simpleScript
readCertificateScriptWitnessSbe
  sbe
  ( OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits)
    ) = do
    let plutusScriptFp = unFile scriptFp
    Compatible.AnyPlutusScript plutusScriptVer (PlutusScriptSerialised sBytes) <-
      Compatible.readFilePlutusScript plutusScriptFp
    let anyLang :: Exp.AnyPlutusScriptLanguage = case plutusScriptVer of
          PlutusScriptV1 -> Exp.AnyPlutusScriptLanguage L.SPlutusV1
          PlutusScriptV2 -> Exp.AnyPlutusScriptLanguage L.SPlutusV2
          PlutusScriptV3 -> Exp.AnyPlutusScriptLanguage L.SPlutusV3
          PlutusScriptV4 -> Exp.AnyPlutusScriptLanguage L.SPlutusV4
        bs = SBS.fromShort sBytes

        eAnyPlutusScript :: Either DecoderError (Exp.AnyPlutusScript (ShelleyLedgerEra era)) = shelleyBasedEraConstraints sbe $ Exp.decodeAnyPlutusScript bs anyLang
    Exp.AnyPlutusScript anyPlutusScript <- fromEitherCli eAnyPlutusScript
    let
      lang = Exp.plutusScriptInEraSLanguage anyPlutusScript
    let script' = Exp.PScript anyPlutusScript

    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile

    let sw =
          Exp.PlutusScriptWitness
            lang
            script'
            Exp.NoScriptDatum
            redeemer
            execUnits
    return $
      Exp.AnyPlutusScriptWitness $
        Exp.AnyPlutusCertifyingScriptWitness sw
readCertificateScriptWitnessSbe
  _
  ( PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refInput
          (AnySLanguage lang)
          Exp.NoScriptDatumAllowed
          NoPolicyId
          redeemerFile
          execUnits
        )
    ) = do
    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile
    return $
      Exp.AnyPlutusScriptWitness $
        Exp.AnyPlutusCertifyingScriptWitness $
          Exp.PlutusScriptWitness
            lang
            (Exp.PReferenceScript refInput)
            Exp.NoScriptDatum
            redeemer
            execUnits
readCertificateScriptWitnessSbe _ (SimpleReferenceScript (SimpleRefScriptArgs refTxin NoPolicyId)) =
  return . Exp.AnySimpleScriptWitness $ Exp.SReferenceScript refTxin

-- | Create 'TxCertificates'. Note that 'Certificate era' will be deduplicated. Only Certificates with a
-- stake credential will be in the result.
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
mkTxCertificatesSbe era certs = Exp.TxCertificates . OMap.fromList $ map getStakeCred certs
 where
  getStakeCred
    :: (Exp.Certificate (ShelleyLedgerEra era), Exp.AnyWitness (ShelleyLedgerEra era))
    -> ( Exp.Certificate (ShelleyLedgerEra era)
       , Maybe (StakeCredential, Exp.AnyWitness (ShelleyLedgerEra era))
       )
  getStakeCred (c@(Exp.Certificate cert), wit) =
    (c, (,wit) <$> Compatible.getTxCertWitness (convert era) cert)

obtainTypeable
  :: ConwayEraOnwards era
  -> (Typeable (Exp.LedgerEra era) => r)
  -> r
obtainTypeable ConwayEraOnwardsConway r = r
obtainTypeable ConwayEraOnwardsDijkstra r = r

readUpdateProposalFile
  :: Featured ShelleyToBabbageEra era (Maybe UpdateProposalFile)
  -> CIO e (AnyProtocolUpdate era)
readUpdateProposalFile (Featured sToB Nothing) =
  return $ NoPParamsUpdate $ convert sToB
readUpdateProposalFile (Featured sToB (Just updateProposalFile)) = do
  prop <-
    fromExceptTCli $ readTxUpdateProposal sToB updateProposalFile
  case prop of
    TxUpdateProposalNone -> return $ NoPParamsUpdate $ convert sToB
    TxUpdateProposal _ proposal -> return $ ProtocolUpdate sToB proposal

readProposalProcedureFile
  :: forall e era
   . Featured ConwayEraOnwards era [(ProposalFile In, Maybe (ScriptRequirements Exp.ProposalItem))]
  -> CIO e (AnyProtocolUpdate era)
readProposalProcedureFile (Featured cEraOnwards []) =
  let sbe = convert cEraOnwards
   in return $ NoPParamsUpdate sbe
readProposalProcedureFile (Featured cEraOnwards proposals) = do
  let era = convert cEraOnwards
  props :: [(Proposal era, Exp.AnyWitness (Exp.LedgerEra era))] <-
    Exp.obtainCommonConstraints era $ mapM readProposal proposals

  return $
    Exp.obtainCommonConstraints era $
      ProposalProcedures cEraOnwards $
        Exp.mkTxProposalProcedures
          [(govProp, swit) | (Proposal govProp, swit) <- props]
