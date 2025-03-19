module Cardano.CLI.Read.GovernanceActionId
  ( readGoveranceActionIdHexText
  ) where 
    
import Cardano.Api.Shelley
import qualified Cardano.Api.Ledger as L
import Cardano.CLI.EraBased.Common.Option ( parseTxIn ) 
import qualified Text.Parsec as Text 
import qualified Data.Text as Text
import Data.Text (Text)


readGoveranceActionIdHexText :: Text -> Either Text.ParseError (L.GovActionId L.StandardCrypto)
readGoveranceActionIdHexText hexText = do
    TxIn txid (TxIx index) <- Text.parse parseTxIn "" $ Text.unpack hexText
    return $ createGovernanceActionId txid $ fromIntegral index