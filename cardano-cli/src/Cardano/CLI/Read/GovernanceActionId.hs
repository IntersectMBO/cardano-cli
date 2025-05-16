module Cardano.CLI.Read.GovernanceActionId
  ( readGoveranceActionIdHexText
  )
where

import Cardano.Api.Ledger qualified as L
import Cardano.Api.Shelley

import Cardano.CLI.EraBased.Common.Option (parseTxIn)

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec qualified as Text

readGoveranceActionIdHexText :: Text -> Either Text.ParseError L.GovActionId
readGoveranceActionIdHexText hexText = do
  TxIn txid (TxIx index) <- Text.parse parseTxIn "" $ Text.unpack hexText
  return $ createGovernanceActionId txid $ fromIntegral index
