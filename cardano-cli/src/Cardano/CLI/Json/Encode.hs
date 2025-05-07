module Cardano.CLI.Json.Encode
  ( encodeJson
  , encodeYaml
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Yaml.Pretty qualified as Yaml

-- | Encode JSON with pretty printing.
encodeJson :: Aeson.ToJSON a => a -> LBS.ByteString
encodeJson =
  Aeson.encodePretty' jsonConfig

-- | Encode YAML with pretty printing.
encodeYaml :: Aeson.ToJSON a => a -> LBS.ByteString
encodeYaml =
  LBS.fromStrict . Yaml.encodePretty yamlConfig

jsonConfig :: Aeson.Config
jsonConfig =
  Aeson.defConfig
    { Aeson.confCompare = compare
    }

yamlConfig :: Yaml.Config
yamlConfig =
  Yaml.setConfCompare compare Yaml.defConfig
