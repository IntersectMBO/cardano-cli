{-# LANGUAGE FlexibleContexts #-}

module Test.Cli.Json.Friendly
  ( hprop_friendly_auxiliary_plutus_scripts
  , hprop_friendly_dangling_redeemer
  )
where

import Cardano.Api (ScriptData (ScriptDataNumber), toAlonzoData, unsafeHashableScriptData)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.CLI.Compatible.Json.Friendly (friendlyTxBodyImpl)
import Cardano.CLI.Type.MonadWarning (runWarningIO)
import Cardano.Ledger.Api.Tx qualified as Ledger
  ( AlonzoEraTxAuxData
  , nativeScriptsTxAuxDataL
  , plutusScriptsTxAuxDataL
  )
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Plutus.Language qualified as Plutus

import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Short qualified as SBS
import Data.Foldable (for_, toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import GHC.Exts (fromList)
import Lens.Micro ((&), (.~))

import Hedgehog (Property, (===))
import Hedgehog.Extras qualified as H

-- | The CLI cannot build a transaction with a Plutus script in its auxiliary
-- data, so this builds one with the ledger and renders it: native scripts
-- first, then Plutus scripts by language, in both eras.
hprop_friendly_auxiliary_plutus_scripts :: Property
hprop_friendly_auxiliary_plutus_scripts = H.propertyOnce $ do
  conway <- liftIO $ auxiliaryScripts Exp.ConwayEra
  conway === Just expected
  dijkstra <- liftIO $ auxiliaryScripts Exp.DijkstraEra
  dijkstra === Just expected
 where
  expected =
    Aeson.toJSON
      [ Aeson.object
          [ "type" .= Aeson.String "native"
          , "script" .= Aeson.object ["type" .= Aeson.String "after", "slot" .= (1000 :: Int)]
          ]
      , plutus "PlutusV2" "43010203"
      , plutus "PlutusV3" "43040506"
      ]
  plutus :: Aeson.Value -> Aeson.Value -> Aeson.Value
  plutus version script =
    Aeson.object
      [ "type" .= Aeson.String "plutus"
      , "plutus version" .= version
      , -- The script's bytes, CBOR-encoded, in hex
        "script" .= script
      ]

-- | The rendered auxiliary scripts of a transaction whose auxiliary data has a
-- native script, and a Plutus V3 and a Plutus V2 script, in that order.
auxiliaryScripts
  :: Ledger.AlonzoEraTxAuxData (Exp.LedgerEra era)
  => Exp.Era era
  -> IO (Maybe Aeson.Value)
auxiliaryScripts era = Exp.obtainCommonConstraints era $ do
  let auxData =
        Ledger.mkBasicTxAuxData
          & Ledger.nativeScriptsTxAuxDataL .~ fromList [L.mkTimeStart (L.SlotNo 1000)]
          & Ledger.plutusScriptsTxAuxDataL
            .~ fromList
              [ (Plutus.PlutusV3, Plutus.PlutusBinary (SBS.pack [4, 5, 6]) :| [])
              , (Plutus.PlutusV2, Plutus.PlutusBinary (SBS.pack [1, 2, 3]) :| [])
              ]
      tx = Ledger.mkBasicTx Ledger.mkBasicTxBody & Ledger.auxDataTxL .~ L.SJust auxData
  pairs <- runWarningIO $ friendlyTxBodyImpl era (Exp.UnsignedTx tx)
  pure $ lookup "auxiliary scripts" pairs

-- | A redeemer that points past the last item of its kind: the view says
-- which kind of item is missing.
hprop_friendly_dangling_redeemer :: Property
hprop_friendly_dangling_redeemer = H.propertyOnce $ do
  for_ conwayCases $ \(pointer, message) -> do
    purposes <- liftIO $ danglingRedeemerPurposes Exp.ConwayEra pointer
    purposes === [Just (Aeson.object ["error" .= message])]
  -- Only Dijkstra has guards.
  purposes <- liftIO $ danglingRedeemerPurposes Exp.DijkstraEra (L.DijkstraGuarding (L.AsIx 7))
  purposes === [Just (Aeson.object ["error" .= Aeson.String "Could not find the guard at index 7"])]
 where
  conwayCases :: [(L.PlutusPurpose L.AsIx (Exp.LedgerEra Exp.ConwayEra), Aeson.Value)]
  conwayCases =
    [ (L.ConwaySpending (L.AsIx 7), "Could not find the input at index 7")
    , (L.ConwayMinting (L.AsIx 7), "Could not find the minting policy at index 7")
    , (L.ConwayCertifying (L.AsIx 7), "Could not find the certificate at index 7")
    , (L.ConwayWithdrawing (L.AsIx 7), "Could not find the withdrawal at index 7")
    , (L.ConwayVoting (L.AsIx 7), "Could not find the voter at index 7")
    , (L.ConwayProposing (L.AsIx 7), "Could not find the proposal at index 7")
    ]

-- | The rendered purposes of a transaction whose only redeemer has this
-- pointer, and an empty body.
danglingRedeemerPurposes
  :: Exp.Era era -> L.PlutusPurpose L.AsIx (Exp.LedgerEra era) -> IO [Maybe Aeson.Value]
danglingRedeemerPurposes era pointer = Exp.obtainCommonConstraints era $ do
  let redeemer = (toAlonzoData (unsafeHashableScriptData (ScriptDataNumber 42)), L.ExUnits 0 0)
      tx =
        Ledger.mkBasicTx Ledger.mkBasicTxBody
          & Ledger.witsTxL . L.rdmrsTxWitsL .~ L.Redeemers (Map.singleton pointer redeemer)
  pairs <- runWarningIO $ friendlyTxBodyImpl era (Exp.UnsignedTx tx)
  pure
    [ KeyMap.lookup "purpose" fields
    | Aeson.Array redeemers <- toList $ lookup "redeemers" pairs
    , Aeson.Object fields <- toList redeemers
    ]
