{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Guard.Option (pTxGuardArguments) where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Script.Guard.Type
import Cardano.CLI.Type.Common

import Control.Applicative ((<|>))
import Options.Applicative (Parser, many, optional)

pTxGuardArguments :: Exp.Era era -> BalanceTxExecUnits -> Parser (TxGuardArguments era)
pTxGuardArguments Exp.ConwayEra _ = ConwayRequiredSignerArguments <$> many pRequiredSigner
pTxGuardArguments Exp.DijkstraEra balance =
  DijkstraGuardArguments <$> many ((KeyGuard <$> pRequiredSigner) <|> pScriptGuard balance)

pScriptGuard :: BalanceTxExecUnits -> Parser Guard
pScriptGuard balance =
  ( ScriptGuard . GuardScriptOnDisk
      <$> pScriptFor
        "guard-script-file"
        Nothing
        "Native or Plutus V4 script whose approval this transaction requires. Plutus guards require a redeemer; native guards accept neither redeemers nor execution units."
      <*> optional pRedeemer
  )
    <|> ( ScriptGuard
            <$> ( NativeGuardScriptReference
                    <$> pReferenceTxIn "simple-guard-script-" "native"
                    <*> pGuardScriptHash
                )
            <*> pure Nothing
        )
    <|> ( ScriptGuard
            <$> ( PlutusGuardScriptReference
                    <$> pReferenceTxIn "guard-" "Plutus V4"
                    <* pPlutusV4ReferenceLanguage Exp.DijkstraEra "guard-"
                    <*> pGuardScriptHash
                )
            <*> (Just <$> pRedeemer)
        )
 where
  pGuardScriptHash =
    pScriptHash
      "guard-script-hash"
      "Hash of the guard script at the preceding reference input."
  pRedeemer =
    (,)
      <$> pScriptDataOrFile
        "guard-redeemer"
        "Redeemer for the preceding Plutus V4 guard."
        "Redeemer file for the preceding Plutus V4 guard."
      <*> case balance of
        AutoBalance -> pure (ExecutionUnits 0 0)
        ManualBalance -> pExecutionUnits "guard"
