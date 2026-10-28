{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Guard.Read
  ( readConwayRequiredSigners
  , readDijkstraGuards
  , getTxGuardReferenceInputs
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as P
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Guard.Type
import Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import Cardano.CLI.EraBased.Transaction.Internal.Common (findDuplicates)
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (RequiredSigner)
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.Plutus.Language qualified as L

import Control.Monad (forM, unless)
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import GHC.Exts (toList)

-- | Conway retains the separate required-signers field in the transaction body.
readConwayRequiredSigners :: [RequiredSigner] -> CIO e [Hash PaymentKey]
readConwayRequiredSigners = mapM (fromEitherIOCli . readRequiredSigner)

-- | Read both key and script guards into the API's unified collection.
-- The required-signer CLI flags supply key guards in Dijkstra. Their signatures
-- are added during signing; Nothing here means no script witness is attached.
readDijkstraGuards
  :: [Guard]
  -> CIO e (Exp.TxGuards (Exp.LedgerEra Exp.DijkstraEra))
readDijkstraGuards guards = do
  entries <- mapM readGuard guards
  -- Preserve the existing order: scripts first, then sorted, distinct key guards.
  -- Changing guard positions would change Plutus redeemer indices.
  let scripts = [(credential, witness) | (credential@L.ScriptHashObj{}, witness) <- entries]
      keys = Set.toAscList $ Set.fromList [credential | (credential@L.KeyHashObj{}, _) <- entries]
      duplicates = findDuplicates (map fst scripts)
  unless (null duplicates) $ throwCliError $ TxCmdDuplicateGuards duplicates
  fromEitherCli $ first TxCmdGuardsError $ Exp.mkTxGuards (scripts ++ [(key, Nothing) | key <- keys])

getTxGuardReferenceInputs :: TxGuardRequirements era -> [TxIn]
getTxGuardReferenceInputs (ConwayRequiredSigners _) = []
getTxGuardReferenceInputs (DijkstraGuards guards) =
  mapMaybe (\(_, witness) -> witness >>= getGuardScriptReferenceInput) (toList guards)

getGuardScriptReferenceInput :: Exp.ScriptGuardWitness era -> Maybe TxIn
getGuardScriptReferenceInput (Exp.NativeGuardWitness (Exp.SReferenceScript input)) = Just input
getGuardScriptReferenceInput (Exp.NativeGuardWitness (Exp.SScript _)) = Nothing
getGuardScriptReferenceInput (Exp.PlutusGuardWitness (P.PlutusScriptWitness _ source _ _ _)) =
  case source of
    P.PReferenceScript input -> Just input
    P.PScript _ -> Nothing

readGuard
  :: Guard
  -> CIO e (L.Credential L.Guard, Maybe (Exp.ScriptGuardWitness (Exp.LedgerEra Exp.DijkstraEra)))
readGuard (KeyGuard signer) = do
  PaymentKeyHash key <- fromEitherIOCli $ readRequiredSigner signer
  pure (L.KeyHashObj (coerceKeyRole key), Nothing)
readGuard (ScriptGuard source mRedeemer) = do
  (credential, witness) <- readScriptGuard
  pure (credential, Just witness)
 where
  readScriptGuard = do
    redeemer <- forM mRedeemer $ \(datum, units) -> do
      value <- fromExceptTCli $ readScriptDataOrFile datum
      pure (value, units)
    case source of
      GuardScriptOnDisk (File path) -> do
        script <- readAnyScript @_ @Exp.DijkstraEra path
        case script of
          Exp.AnySimpleScript native -> do
            let credential = L.ScriptHashObj (Exp.hashSimpleScript native)
            maybe
              (pure (credential, Exp.NativeGuardWitness (Exp.SScript native)))
              (const $ throwCliError $ TxCmdNativeGuardWithRedeemer credential)
              redeemer
          Exp.AnyPlutusScript scriptInEra ->
            case P.plutusScriptInEraSLanguage scriptInEra of
              L.SPlutusV1 -> throwCliError $ TxCmdTopLevelGuardUnsupportedPlutusLanguage path L.PlutusV1
              L.SPlutusV2 -> throwCliError $ TxCmdTopLevelGuardUnsupportedPlutusLanguage path L.PlutusV2
              L.SPlutusV3 -> throwCliError $ TxCmdTopLevelGuardUnsupportedPlutusLanguage path L.PlutusV3
              L.SPlutusV4 -> do
                let credential = L.ScriptHashObj (P.hashPlutusScriptInEra scriptInEra)
                maybe
                  (throwCliError $ TxCmdPlutusGuardMissingRedeemer credential)
                  (\(datum, units) -> pure (credential, plutusWitness (P.PScript scriptInEra) datum units))
                  redeemer
      GuardScriptReference input (ScriptHash scriptHash) ->
        pure
          ( L.ScriptHashObj scriptHash
          , maybe
              (Exp.NativeGuardWitness (Exp.SReferenceScript input))
              (uncurry $ plutusWitness (P.PReferenceScript input))
              redeemer
          )
  plutusWitness
    :: P.PlutusScriptOrReferenceInput L.PlutusV4 era
    -> HashableScriptData
    -> ExecutionUnits
    -> Exp.ScriptGuardWitness era
  plutusWitness scriptSource datum units =
    Exp.PlutusGuardWitness $
      P.PlutusScriptWitness L.SPlutusV4 scriptSource P.NoScriptDatum datum units
