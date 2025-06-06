{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.CLI.EraBased.Script.Certificate.Read
  ( Certificate (..)
  , convertToNewCertificate
  , convertToOldApiCertificate
  , mkTxCertificates
  , readCertificateScriptWitness
  , readCertificateScriptWitnesses
  )
where

import Cardano.Api.Experimental
import Cardano.Api.Internal.Script (ToLedgerPlutusLanguage)
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley
  ( AnyPlutusScriptVersion (..)
  , Convert (..)
  , File (..)
  , TxCertificates (..)
  , fromAllegraTimelock
  , sbeToSimpleScriptLanguageInEra
  )
import Cardano.Api.Shelley qualified as Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Certificate.Type
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.Orphan ()
import Cardano.CLI.Type.Common (CertificateFile)
import Cardano.Ledger.Allegra.Scripts qualified as L
import Cardano.Ledger.Binary qualified as CBOR
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Ledger.Plutus.Language qualified as Plutus

import GHC.IsList

-- TODO: Move to cardano-api experimental api
-- We must convert to the old certificate type for the sake of TxBodyContent
-- however the experimental API will be geared towards using the ledger's
-- Tx type so we can avoid creating another abtractions for a tx/tx body
-- and we can rely on type classes/accessor functions already implemented
-- in cardano-ledger.
-- We aim to remove the use of TxBodyContent totally in cardano-cli
data Certificate era where
  Certificate :: IsEra era => L.TxCert (LedgerEra era) -> Certificate era

convertToOldApiCertificate :: Era era -> Certificate era -> Api.Certificate era
convertToOldApiCertificate ConwayEra (Certificate cert) =
  Api.ConwayCertificate Api.ConwayEraOnwardsConway cert

convertToNewCertificate :: Era era -> Api.Certificate era -> Certificate era
convertToNewCertificate ConwayEra (Api.ConwayCertificate _ cert) = Certificate cert
convertToNewCertificate ConwayEra (Api.ShelleyRelatedCertificate sToBab _) =
  case sToBab :: Api.ShelleyToBabbageEra ConwayEra of {}

mkTxCertificates
  :: forall era
   . IsEra era
  => [(Certificate era, AnyWitness (LedgerEra era))]
  -> TxCertificates Api.BuildTx era
mkTxCertificates [] = TxCertificatesNone
mkTxCertificates certs =
  TxCertificates (convert useEra) $ fromList $ map (getStakeCred useEra) certs
 where
  getStakeCred
    :: Era era
    -> (Certificate era, AnyWitness (LedgerEra era))
    -> ( Api.Certificate era
       , Api.BuildTxWith
           Api.BuildTx
           (Maybe (Api.StakeCredential, Api.Witness Api.WitCtxStake era))
       )
  getStakeCred ConwayEra (Certificate cert, AnyKeyWitnessPlaceholder) =
    (Api.ConwayCertificate (convert ConwayEra) cert, Api.BuildTxWith Nothing)
  getStakeCred ConwayEra (Certificate cert, AnySimpleScriptWitness ss) =
    let oldApiCert = Api.ConwayCertificate (convert ConwayEra) cert
        mStakeCred = Api.selectStakeCredentialWitness oldApiCert
        wit = Api.ScriptWitness Api.ScriptWitnessForStakeAddr $ newToOldSimpleScriptWitness ConwayEra ss
     in ( oldApiCert
        , pure $ (,wit) <$> mStakeCred
        )
  getStakeCred ConwayEra (Certificate cert, AnyPlutusScriptWitness psw) =
    let oldApiCert = Api.ConwayCertificate (convert ConwayEra) cert
        mStakeCred = Api.selectStakeCredentialWitness oldApiCert
        wit =
          Api.ScriptWitness Api.ScriptWitnessForStakeAddr $
            newToOldPlutusCertificateScriptWitness ConwayEra psw
     in ( oldApiCert
        , pure $ (,wit) <$> mStakeCred
        )

newToOldSimpleScriptWitness
  :: L.AllegraEraScript (LedgerEra era)
  => Era era -> SimpleScriptOrReferenceInput (LedgerEra era) -> Api.ScriptWitness Api.WitCtxStake era
newToOldSimpleScriptWitness era simple =
  case simple of
    SScript (SimpleScript script) ->
      Api.SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert era)
        (Api.SScript $ fromAllegraTimelock script)
    SReferenceScript inp ->
      Api.SimpleScriptWitness
        (sbeToSimpleScriptLanguageInEra $ convert era)
        (Api.SReferenceScript inp)

newToOldPlutusCertificateScriptWitness
  :: Era era -> PlutusScriptWitness lang purpose (LedgerEra era) -> Api.ScriptWitness Api.WitCtxStake era
newToOldPlutusCertificateScriptWitness ConwayEra (PlutusScriptWitness Plutus.SPlutusV1 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV1InConway
    Api.PlutusScriptV1
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (PlutusScriptWitness Plutus.SPlutusV2 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV2InConway
    Api.PlutusScriptV2
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits
newToOldPlutusCertificateScriptWitness ConwayEra (PlutusScriptWitness Plutus.SPlutusV3 scriptOrRef _ redeemer execUnits) =
  Api.PlutusScriptWitness
    Api.PlutusScriptV3InConway
    Api.PlutusScriptV3
    (newToOldPlutusScriptOrReferenceInput ConwayEra scriptOrRef)
    Api.NoScriptDatumForStake
    redeemer
    execUnits

newToOldPlutusScriptOrReferenceInput
  :: Era era
  -> PlutusScriptOrReferenceInput lang (LedgerEra era)
  -> Api.PlutusScriptOrReferenceInput oldlang
newToOldPlutusScriptOrReferenceInput ConwayEra (PReferenceScript txin) = Api.PReferenceScript txin
newToOldPlutusScriptOrReferenceInput ConwayEra (PScript (PlutusScriptInEra plutusRunnable)) =
  let oldScript = L.unPlutusBinary . L.plutusBinary $ L.plutusFromRunnable plutusRunnable
   in Api.PScript $ Api.PlutusScriptSerialised oldScript

--------------------------------------------------------------
-- this uses the old api so you need to swith to the new one
-- look at CertificateScriptWitness to confirm
readCertificateScriptWitness
  :: forall era e
   . IsEra era
  => CliCertificateScriptRequirements
  -> CIO e (AnyWitness (LedgerEra era))
readCertificateScriptWitness (OnDiskSimpleScript scriptFp) = do
  let sFp = unFile scriptFp
  s <-
    fromExceptTCli $
      readFileSimpleScript sFp
  let nativeScript :: SimpleScript (LedgerEra era) = convertTotimelock useEra s
  return $
    AnySimpleScriptWitness $
      SScript nativeScript
readCertificateScriptWitness (OnDiskPlutusScript (OnDiskPlutusScriptCliArgs scriptFp redeemerFile execUnits)) = do
  let plutusScriptFp = unFile scriptFp
  AnyPlutusScript sVer apiScript <-
    fromExceptTCli $
      readFilePlutusScript plutusScriptFp

  let lang = toPlutusSLanguage sVer
  script <- decodePlutusScript useEra sVer apiScript

  redeemer <-
    fromExceptTCli $
      readScriptDataOrFile redeemerFile
  return $
    AnyPlutusScriptWitness $
      PlutusScriptWitness
        lang
        script
        NoScriptDatum
        redeemer
        execUnits
readCertificateScriptWitness
  ( OnDiskPlutusRefScript
      (PlutusRefScriptCliArgs refInput (AnyPlutusScriptVersion sVer) redeemerFile execUnits)
    ) = do
    let lang = toPlutusSLanguage sVer
    redeemer <-
      fromExceptTCli $
        readScriptDataOrFile redeemerFile
    return $
      AnyPlutusScriptWitness $
        PlutusScriptWitness
          lang
          (PReferenceScript refInput)
          NoScriptDatum
          redeemer
          execUnits

decodePlutusScript
  :: forall era lang e
   . Era era
  -> Api.PlutusScriptVersion lang
  -> Api.PlutusScript lang
  -> CIO e (PlutusScriptOrReferenceInput (ToLedgerPlutusLanguage lang) (LedgerEra era))
decodePlutusScript era sVer (Api.PlutusScriptSerialised script) = obtainConstraints sVer $ do
  let runnableScriptBs = L.Plutus $ L.PlutusBinary $ script
  plutusRunnable <-
    fromEitherCli $
      Plutus.decodePlutusRunnable
        (getVersion era)
        runnableScriptBs
  return $ PScript (PlutusScriptInEra plutusRunnable)

obtainConstraints
  :: Api.PlutusScriptVersion lang
  -> (L.PlutusLanguage (ToLedgerPlutusLanguage lang) => a)
  -> a
obtainConstraints v =
  case v of
    Api.PlutusScriptV1 -> id
    Api.PlutusScriptV2 -> id
    Api.PlutusScriptV3 -> id

getVersion :: forall era. Era era -> CBOR.Version
getVersion e = obtainCommonConstraints e $ L.eraProtVerLow @(LedgerEra era)

convertTotimelock
  :: forall era
   . Era era
  -> Api.Script Api.SimpleScript'
  -> SimpleScript (LedgerEra era)
convertTotimelock era (Api.SimpleScript s) =
  let native :: L.NativeScript (LedgerEra era) = obtainCommonConstraints era $ Api.toAllegraTimelock s
   in obtainCommonConstraints era $ SimpleScript native

readCertificateScriptWitnesses
  :: IsEra era
  => [(CertificateFile, Maybe CliCertificateScriptRequirements)]
  -> CIO e [(CertificateFile, AnyWitness (LedgerEra era))]
readCertificateScriptWitnesses certs =
  mapM
    ( \(vFile, mCert) -> do
        case mCert of
          Nothing -> return (vFile, AnyKeyWitnessPlaceholder)
          Just cert -> do
            sWit <- readCertificateScriptWitness cert
            return (vFile, sWit)
    )
    certs
