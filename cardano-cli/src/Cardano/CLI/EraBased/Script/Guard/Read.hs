{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Guard.Read (readGuardScriptWitnesses) where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Plutus qualified as P
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Guard.Type
import Cardano.CLI.EraBased.Script.Read.Common (readScriptDataOrFile)
import Cardano.CLI.Read
import Cardano.CLI.Type.Error.TxCmdError

import Control.Monad (forM, unless)
import Data.List qualified as List

readGuardScriptWitnesses
  :: Exp.Era era
  -> [GuardScriptWitness]
  -> CIO e [(L.Credential L.Guard, Exp.AnyWitness (Exp.LedgerEra era))]
readGuardScriptWitnesses Exp.ConwayEra [] = pure []
readGuardScriptWitnesses Exp.ConwayEra _ = throwCliError TxCmdGuardScriptsNotSupported
readGuardScriptWitnesses Exp.DijkstraEra guards = do
  witnesses <- mapM readGuard guards
  let duplicates = [credential | credential : _ : _ <- List.group (List.sort (map fst witnesses))]
  unless (null duplicates) $ throwCliError $ TxCmdDuplicateGuards duplicates
  pure witnesses
 where
  readGuard (GuardScriptWitness source mRedeemer) = do
    redeemer <- forM mRedeemer $ \(datum, units) -> do
      value <- fromExceptTCli $ readScriptDataOrFile datum
      pure (value, units)
    case source of
      GuardScriptOnDisk (File path) -> do
        script <- readAnyScript @_ @Exp.DijkstraEra path
        case script of
          Exp.AnySimpleScript native -> do
            let credential = L.ScriptHashObj (Exp.hashSimpleScript native)
            case redeemer of
              Just _ -> throwCliError $ TxCmdNativeGuardWithRedeemer credential
              Nothing -> pure (credential, Exp.AnySimpleScriptWitness (Exp.SScript native))
          Exp.AnyPlutusScript scriptInEra ->
            case P.plutusScriptInEraSLanguage scriptInEra of
              L.SPlutusV4 -> do
                let credential = L.ScriptHashObj (P.hashPlutusScriptInEra scriptInEra)
                case redeemer of
                  Nothing -> throwCliError $ TxCmdPlutusGuardMissingRedeemer credential
                  Just (datum, units) -> pure (credential, plutusWitness (P.PScript scriptInEra) datum units)
              _ ->
                throwCliError $
                  TxCmdTopLevelGuardUnsupportedPlutusLanguage path (P.plutusScriptInEraLanguage scriptInEra)
      GuardScriptReference input (ScriptHash scriptHash) ->
        pure
          ( L.ScriptHashObj scriptHash
          , case redeemer of
              Nothing -> Exp.AnySimpleScriptWitness (Exp.SReferenceScript input)
              Just (datum, units) -> plutusWitness (P.PReferenceScript input) datum units
          )
  plutusWitness source datum units =
    Exp.AnyPlutusScriptWitness $
      Exp.AnyPlutusGuardingScriptWitness $
        P.PlutusScriptWitness L.SPlutusV4 source P.NoScriptDatum datum units
