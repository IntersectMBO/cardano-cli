{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Helpers shared by the transaction and sub-transaction body builders.
module Cardano.CLI.EraBased.Transaction.Internal.Common
  ( toTxOutInEra
  , toTxOutInShelleyBasedEra
  , getAllReferenceInputs
  , createTxMintValue
  , convertWithdrawals
  , convertVotingProcedures
  , scriptInEraToSimpleScript
  , partitionSomeWitnesses
  )
where

import Cardano.Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScriptWitness qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Compatible.Transaction.TxOut
import Cardano.CLI.Read
import Cardano.CLI.Type.Common
import Cardano.CLI.Type.Error.TxCmdError
import Cardano.Ledger.Hashes (DataHash)

import RIO hiding (toList)

import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Exts (IsList (..))

toTxOutInEra
  :: forall era e
   . Exp.IsEra era
  => TxOutAnyEra
  -> CIO e (Exp.TxOut (Exp.LedgerEra era), Map.Map DataHash (L.Data (Exp.LedgerEra era)))
toTxOutInEra (TxOutAnyEra addr' val' mDatumHash refScriptFp) = do
  let sbe = convert (Exp.useEra @era)
      addr = anyAddressInShelleyBasedEra sbe addr'
  obtainCommonConstraints (Exp.useEra @era) $
    mkTxOut sbe addr val' mDatumHash refScriptFp

toTxOutInShelleyBasedEra
  :: forall era e
   . Exp.IsEra era
  => TxOutShelleyBasedEra
  -> CIO e (Exp.TxOut (Exp.LedgerEra era), Map.Map DataHash (L.Data (Exp.LedgerEra era)))
toTxOutInShelleyBasedEra (TxOutShelleyBasedEra addr' val' mDatumHash refScriptFp) = do
  let sbe = convert (Exp.useEra @era)
      addr = shelleyAddressInEra sbe addr'
  obtainCommonConstraints (Exp.useEra @era) $
    mkTxOut sbe addr val' mDatumHash refScriptFp

getAllReferenceInputs
  :: [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [Exp.AnyScriptWitness (Exp.LedgerEra era)]
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -- \^ Certificate witnesses
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [Exp.AnyWitness (Exp.LedgerEra era)]
  -> [TxIn]
  -- \^ Read only reference inputs
  -> [TxIn]
getAllReferenceInputs
  spendingWitnesses
  mintWitnesses
  certScriptWitnesses
  withdrawals
  votingProceduresAndMaybeScriptWits
  propProceduresAnMaybeScriptWits
  readOnlyRefIns = do
    let txinsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput spendingWitnesses
        mintingRefInputs = mapMaybe Exp.getAnyScriptWitnessReferenceInput mintWitnesses
        certsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput certScriptWitnesses
        withdrawalsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput withdrawals
        votesWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput votingProceduresAndMaybeScriptWits
        propsWitByRefInputs = mapMaybe Exp.getAnyWitnessReferenceInput propProceduresAnMaybeScriptWits

    concat
      [ txinsWitByRefInputs
      , mintingRefInputs
      , certsWitByRefInputs
      , withdrawalsWitByRefInputs
      , votesWitByRefInputs
      , propsWitByRefInputs
      , mapMaybe Just readOnlyRefIns
      ]

-- TODO: Currently we specify the policyId with the '--mint' option on the cli
-- and we added a separate '--policy-id' parser that parses the policy id for the
-- given reference input (since we don't have the script in this case). To avoid asking
-- for the policy id twice (in the build command) we can potentially query the UTxO and
-- access the script (and therefore the policy id).
createTxMintValue
  :: (L.MultiAsset, [(PolicyId, Exp.AnyScriptWitness (Exp.LedgerEra era))])
  -> Either TxCmdError (Exp.TxMintValue (Exp.LedgerEra era))
createTxMintValue (val, scriptWitnesses) =
  if mempty == val && List.null scriptWitnesses
    then return $ Exp.TxMintValue Map.empty
    else do
      let policiesWithAssets :: Map PolicyId PolicyAssets
          policiesWithAssets = multiAssetToPolicyAssets val
          -- The set of policy ids for which we need witnesses:
          witnessesNeededSet :: Set PolicyId
          witnessesNeededSet = Map.keysSet policiesWithAssets

      let witnessesProvidedMap = fromList scriptWitnesses
          witnessesProvidedSet :: Set PolicyId
          witnessesProvidedSet = Map.keysSet witnessesProvidedMap

      -- Check not too many, nor too few:
      validateAllWitnessesProvided witnessesNeededSet witnessesProvidedSet
      validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet
      pure $
        Exp.TxMintValue $
          Map.intersectionWith
            (,)
            policiesWithAssets
            witnessesProvidedMap
 where
  validateAllWitnessesProvided witnessesNeeded witnessesProvided
    | null witnessesMissing = return ()
    | otherwise = Left (TxCmdPolicyIdsMissing witnessesMissing (toList witnessesProvided))
   where
    witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

  validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
    | null witnessesExtra = return ()
    | otherwise = Left (TxCmdPolicyIdsExcess witnessesExtra)
   where
    witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

convertWithdrawals
  :: [(StakeAddress, L.Coin, Exp.AnyWitness (Exp.LedgerEra era))]
  -> Exp.TxWithdrawals (Exp.LedgerEra era)
convertWithdrawals = Exp.TxWithdrawals

convertVotingProcedures
  :: forall era
   . Exp.IsEra era
  => [(VotingProcedures era, Exp.AnyWitness (Exp.LedgerEra era))]
  -> [(L.VotingProcedures (Exp.LedgerEra era), Exp.AnyWitness (Exp.LedgerEra era))]
convertVotingProcedures =
  map
    ( \(VotingProcedures vp, wit) ->
        (obtainCommonConstraints (Exp.useEra @era) vp, wit)
    )

scriptInEraToSimpleScript
  :: forall era. Exp.IsEra era => ScriptInEra era -> Maybe (Exp.SimpleScript (Exp.LedgerEra era))
scriptInEraToSimpleScript s =
  obtainCommonConstraints (Exp.useEra @era) $
    Exp.SimpleScript
      <$> L.getNativeScript (obtainCommonConstraints (Exp.useEra @era) $ toShelleyScript s)

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [ShelleyWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . Foldable.foldl' go mempty
 where
  reversePartitionedWits (bw, skw) =
    (reverse bw, reverse skw)

  go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
    case byronOrShelleyWit of
      AByronWitness byronWit ->
        (byronWit : byronAcc, shelleyKeyAcc)
      AShelleyKeyWitness shelleyKeyWit ->
        (byronAcc, shelleyKeyWit : shelleyKeyAcc)
