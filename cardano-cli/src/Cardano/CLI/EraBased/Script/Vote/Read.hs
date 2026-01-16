{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Script.Vote.Read
  ( readVotingProceduresFiles
  , readVoteScriptWitness
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus qualified as Exp

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.EraBased.Script.Read.Common
import Cardano.CLI.EraBased.Script.Type
import Cardano.CLI.EraBased.Script.Type qualified as Exp
import Cardano.CLI.Read
import Cardano.CLI.Type.Common (AnySLanguage (..))
import Cardano.CLI.Type.Governance

import Control.Monad

readVoteScriptWitness
  :: forall era e
   . Exp.IsEra era
  => (VoteFile In, Maybe (ScriptRequirements Exp.VoterItem))
  -> CIO e (VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))
readVoteScriptWitness (voteFp, Nothing) = do
  votProceds <-
    conwayEraOnwardsConstraints (convert $ Exp.useEra @era) $
      fromEitherIOCli $
        readFileTextEnvelope voteFp
  return (votProceds, Exp.AnyKeyWitnessPlaceholder)
readVoteScriptWitness (voteFp, Just certScriptReq) = do
  votProceds <-
    conwayEraOnwardsConstraints (convert $ Exp.useEra @era) $
      fromEitherIOCli $
        readFileTextEnvelope voteFp
  case certScriptReq of
    OnDiskSimpleScript scriptFp -> do
      let sFp = unFile scriptFp
      s <-
        Exp.AnySimpleScriptWitness . Exp.SScript <$> readFileSimpleScript sFp (Exp.useEra @era)

      return
        ( votProceds
        , s
        )
    OnDiskPlutusScript
      (OnDiskPlutusScriptCliArgs scriptFp Exp.NoScriptDatumAllowed redeemerFile execUnits) -> do
        let plutusScriptFp = unFile scriptFp
        Exp.AnyPlutusScript script <-
          readFilePlutusScript @_ @era plutusScriptFp
        redeemer <-
          fromExceptTCli $
            readScriptDataOrFile redeemerFile

        let pScript = Exp.PScript script
            lang = Exp.plutusScriptInEraSLanguage script
        let sw =
              Exp.PlutusScriptWitness
                lang
                pScript
                Exp.NoScriptDatum
                redeemer
                execUnits
        return
          ( votProceds
          , Exp.AnyPlutusScriptWitness $ AnyPlutusCertifyingScriptWitness sw
          )
    PlutusReferenceScript
      ( PlutusRefScriptCliArgs
          refTxIn
          (AnySLanguage lang)
          Exp.NoScriptDatumAllowed
          Exp.NoPolicyId
          redeemerFile
          execUnits
        ) -> do
        redeemer <-
          fromExceptTCli $ readScriptDataOrFile redeemerFile

        return
          ( votProceds
          , Exp.AnyPlutusScriptWitness $
              AnyPlutusCertifyingScriptWitness $
                Exp.PlutusScriptWitness
                  lang
                  (Exp.PReferenceScript refTxIn)
                  Exp.NoScriptDatum
                  redeemer
                  execUnits
          )
    SimpleReferenceScript (SimpleRefScriptArgs refTxIn _) ->
      return
        ( votProceds
        , Exp.AnySimpleScriptWitness $ Exp.SReferenceScript refTxIn
        )

-- Because the 'Voter' type is contained only in the 'VotingProcedures'
-- type, we must read a single vote as 'VotingProcedures'. The cli will
-- not read vote files with multiple votes in them because this will
-- complicate the code further in terms of contructing the redeemer map
-- when it comes to script witnessed votes.
readVotingProceduresFiles
  :: Exp.IsEra era
  => [(VoteFile In, Maybe (ScriptRequirements Exp.VoterItem))]
  -> CIO e [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))]
readVotingProceduresFiles files =
  forM files readVoteScriptWitness
