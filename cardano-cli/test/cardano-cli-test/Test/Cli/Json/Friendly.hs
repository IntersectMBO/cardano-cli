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
import Cardano.Ledger.Api.Tx qualified as Ledger (plutusScriptsTxAuxDataL)
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
import Lens.Micro ((&), (.~))

import Hedgehog (Property, (===))
import Hedgehog.Extras qualified as H

-- | The CLI cannot build a transaction with a Plutus script in its auxiliary
-- data, so this builds one with the ledger and renders it.
hprop_friendly_auxiliary_plutus_scripts :: Property
hprop_friendly_auxiliary_plutus_scripts = H.propertyOnce $ do
  let auxData :: Ledger.TxAuxData (Exp.LedgerEra Exp.ConwayEra)
      auxData =
        Ledger.mkBasicTxAuxData
          & Ledger.plutusScriptsTxAuxDataL
            .~ Map.singleton Plutus.PlutusV3 (Plutus.PlutusBinary (SBS.pack [1, 2, 3]) :| [])
      tx = Ledger.mkBasicTx Ledger.mkBasicTxBody & Ledger.auxDataTxL .~ L.SJust auxData
  pairs <- liftIO . runWarningIO $ friendlyTxBodyImpl Exp.ConwayEra (Exp.UnsignedTx tx)
  lookup "auxiliary scripts" pairs
    === Just
      ( Aeson.toJSON
          [ Aeson.object
              [ "type" .= Aeson.String "plutus"
              , "plutus version" .= Aeson.String "PlutusV3"
              , -- The script's bytes, CBOR-encoded, in hex
                "script" .= Aeson.String "43010203"
              ]
          ]
      )

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
