{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.CLI.Run.Address.Info
  ( runAddressInfoCmd
  )
where

import Cardano.Api

import Cardano.CLI.Types.Errors.AddressInfoError

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text (Text)
import Options.Applicative (Alternative (..))

data AddressInfo = AddressInfo
  { aiType :: !Text
  , aiEra :: !Text
  , aiEncoding :: !Text
  , aiAddress :: !Text
  , aiBase16 :: !Text
  }

instance ToJSON AddressInfo where
  toJSON addrInfo =
    object
      [ "type" .= aiType addrInfo
      , "era" .= aiEra addrInfo
      , "encoding" .= aiEncoding addrInfo
      , "address" .= aiAddress addrInfo
      , "base16" .= aiBase16 addrInfo
      ]

runAddressInfoCmd :: Text -> Maybe (File () Out) -> ExceptT AddressInfoError IO ()
runAddressInfoCmd addrTxt mOutputFp = do
  addrInfo <- case (Left <$> deserialiseAddress AsAddressAny addrTxt)
    <|> (Right <$> deserialiseAddress AsStakeAddress addrTxt) of
    Nothing ->
      left $ ShelleyAddressInvalid addrTxt
    Just (Left (AddressByron payaddr)) ->
      pure $
        AddressInfo
          { aiType = "payment"
          , aiEra = "byron"
          , aiEncoding = "base58"
          , aiAddress = addrTxt
          , aiBase16 = serialiseToRawBytesHexText payaddr
          }
    Just (Left (AddressShelley payaddr)) ->
      pure $
        AddressInfo
          { aiType = "payment"
          , aiEra = "shelley"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          , aiBase16 = serialiseToRawBytesHexText payaddr
          }
    Just (Right addr) ->
      pure $
        AddressInfo
          { aiType = "stake"
          , aiEra = "shelley"
          , aiEncoding = "bech32"
          , aiAddress = addrTxt
          , aiBase16 = serialiseToRawBytesHexText addr
          }

  case mOutputFp of
    Just (File fpath) -> liftIO $ LBS.writeFile fpath $ encodePretty addrInfo
    Nothing -> liftIO $ LBS.putStrLn $ encodePretty addrInfo
