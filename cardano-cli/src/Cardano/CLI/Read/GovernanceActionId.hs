module Cardano.CLI.Read.GovernanceActionId
  ( readGoveranceActionIdHexText
  )
where

import Cardano.Api.Governance
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Parser.Text as P
import Cardano.Api.Tx

import Data.Text (Text)

readGoveranceActionIdHexText :: Text -> Either String L.GovActionId
readGoveranceActionIdHexText hexText = do
  TxIn txid (TxIx index) <- P.runParser parseTxIn hexText
  return $ createGovernanceActionId txid $ fromIntegral index
