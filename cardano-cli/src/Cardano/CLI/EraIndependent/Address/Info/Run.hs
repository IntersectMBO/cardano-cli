{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.EraIndependent.Address.Info.Run
  ( runAddressInfoCmd
  )
where

import Cardano.Api

import Cardano.CLI.Compatible.Exception
import Cardano.CLI.Type.Error.AddressInfoError

import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Options.Applicative (Alternative (..))

data AddressInfo = AddressInfo
  { aiType :: !Text
  , aiEra :: !Text
  , aiEncoding :: !Text
  , aiAddress :: !Text
  , aiProtected :: !Bool
  , aiReceivingAuthorization :: !(Maybe Aeson.Value)
  , aiBase16 :: !Text
  }

instance ToJSON AddressInfo where
  toJSON addrInfo =
    object $
      [ "type" .= aiType addrInfo
      , "era" .= aiEra addrInfo
      , "encoding" .= aiEncoding addrInfo
      , "address" .= aiAddress addrInfo
      , "base16" .= aiBase16 addrInfo
      ]
        <> ["protected" .= True | aiProtected addrInfo]
        <> maybe
          []
          (\authorization -> ["receivingAuthorization" .= authorization])
          (aiReceivingAuthorization addrInfo)

runAddressInfoCmd :: Text -> Maybe (File () Out) -> CIO e ()
runAddressInfoCmd addrTxt mOutputFp = do
  addrInfo <- case (Left <$> deserialiseAddress AsAddressAny addrTxt)
    <|> (Right <$> deserialiseAddress AsStakeAddress addrTxt) of
    Nothing ->
      throwCliError $ ShelleyAddressInvalid addrTxt
    Just (Left (AddressByron payaddr)) ->
      pure $
        AddressInfo
          { aiType = "payment"
          , aiEra = "byron"
          , aiEncoding = "base58"
          , aiAddress = addrTxt
          , aiProtected = False
          , aiReceivingAuthorization = Nothing
          , aiBase16 = serialiseToRawBytesHexText payaddr
          }
    Just (Left (AddressShelley payaddr)) ->
      pure $
        AddressInfo
          { aiType = "payment"
          , aiEra = if isProtectedShelleyAddress payaddr then "dijkstra" else "shelley"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          , aiProtected = isProtectedShelleyAddress payaddr
          , aiReceivingAuthorization = receivingAuthorization payaddr
          , aiBase16 = serialiseToRawBytesHexText payaddr
          }
    Just (Right addr) ->
      pure $
        AddressInfo
          { aiType = "stake"
          , aiEra = "shelley"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          , aiProtected = False
          , aiReceivingAuthorization = Nothing
          , aiBase16 = serialiseToRawBytesHexText addr
          }

  case mOutputFp of
    Just (File fpath) -> liftIO $ LBS.writeFile fpath $ encodePretty addrInfo
    Nothing -> liftIO $ LBS.putStrLn $ encodePretty addrInfo

-- The same payment credential authorizes creation and later spending.
-- A key recipient signs the agreed transaction body with the existing witness
-- workflow; a script recipient supplies a Receiving script witness.
receivingAuthorization :: Address ShelleyAddr -> Maybe Aeson.Value
receivingAuthorization addr
  | not (isProtectedShelleyAddress addr) = Nothing
  | otherwise =
      let (_, credential, _) = shelleyAddressCredentials addr
       in Just $ case fromShelleyPaymentCredential credential of
            PaymentCredentialByKey key ->
              object
                [ "paymentKeyHash" .= serialiseToRawBytesHexText key
                , "requiresBodySignature" .= True
                ]
            PaymentCredentialByScript script ->
              object
                [ "scriptHash" .= serialiseToRawBytesHexText script
                , "purpose" .= ("receiving" :: Text)
                ]
