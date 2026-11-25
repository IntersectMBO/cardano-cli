module Test.Cli.Json.Friendly
  ( hprop_friendly_auxiliary_plutus_scripts
  )
where

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
import Data.ByteString.Short qualified as SBS
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
