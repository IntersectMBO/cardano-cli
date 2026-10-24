{-# LANGUAGE GADTs #-}

module Cardano.CLI.EraBased.Script.Guard.Option (pGuardScriptWitnesses) where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.CLI.EraBased.Common.Option
import Cardano.CLI.EraBased.Script.Guard.Type
import Cardano.CLI.Type.Common

import Control.Applicative ((<|>))
import Options.Applicative (Parser, many, optional)

pGuardScriptWitnesses :: Exp.Era era -> BalanceTxExecUnits -> Parser [GuardScriptWitness]
pGuardScriptWitnesses Exp.ConwayEra _ = pure []
pGuardScriptWitnesses Exp.DijkstraEra balance =
  many $
    GuardScriptWitness
      <$> ( GuardScriptOnDisk
              <$> pScriptFor
                "guard-script-file"
                Nothing
                "Native or Plutus V4 script whose approval this transaction requires. Plutus guards require a redeemer; native guards accept neither redeemers nor execution units."
              <|> GuardScriptReference
                <$> pReferenceTxIn "guard-" "native or Plutus V4"
                <*> pScriptHash
                  "guard-script-hash"
                  "Hash of the guard script at the preceding reference input. Supply a redeemer for Plutus V4; omit it for a native script."
          )
      <*> optional
        ( (,)
            <$> pScriptDataOrFile
              "guard-redeemer"
              "Redeemer for the preceding Plutus V4 guard."
              "Redeemer file for the preceding Plutus V4 guard."
            <*> case balance of
              AutoBalance -> pure (ExecutionUnits 0 0)
              ManualBalance -> pExecutionUnits "guard"
        )
