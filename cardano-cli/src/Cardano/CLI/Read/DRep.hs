
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Cardano.CLI.Read.DRep
  ( AnyDrepVerificationKey(..)

    -- * Read bech32 or hex encoded DRep verification key
  , readDRepBech32VerificationKeyText
  , readDRepHexVerificationKeyText

    -- * Read TextEnvelope DRep verification key file
  , readDrepVerificationKeyFile
  )
where

import Prelude
import Cardano.Api
import Data.Validation
import Data.Text (Text)
import qualified Cardano.Prelude as Text
import Cardano.CLI.Read




data AnyDrepVerificationKey where 
    AnyDrepVerificationKey :: VerificationKey DRepKey -> AnyDrepVerificationKey
    AnyDrepExtendedVerificationKey :: VerificationKey DRepExtendedKey -> AnyDrepVerificationKey

deriving instance Show AnyDrepVerificationKey

readDRepBech32VerificationKeyText :: Text -> Validation [Bech32DecodeError] AnyDrepVerificationKey
readDRepBech32VerificationKeyText drep = 
    let vkey = liftError return $ AnyDrepVerificationKey <$> deserialiseFromBech32 (AsVerificationKey AsDRepKey) drep
        extendedVkey = liftError return $ AnyDrepExtendedVerificationKey <$> deserialiseFromBech32 (AsVerificationKey AsDRepExtendedKey) drep
    in vkey <> extendedVkey 

readDRepHexVerificationKeyText :: Text -> Validation [RawBytesHexError] AnyDrepVerificationKey
readDRepHexVerificationKeyText drepText = 
    let drepBs = Text.encodeUtf8 drepText
        vkey = liftError return $ AnyDrepVerificationKey <$> deserialiseFromRawBytesHex (AsVerificationKey AsDRepKey) drepBs
        extendedVkey = liftError return $ AnyDrepExtendedVerificationKey <$> deserialiseFromRawBytesHex (AsVerificationKey AsDRepExtendedKey) drepBs
    in vkey <> extendedVkey 


readDrepVerificationKeyFile 
  :: FileOrPipe -> IO (Either (FileError TextEnvelopeError) AnyDrepVerificationKey)
readDrepVerificationKeyFile = readFileOrPipeTextEnvelopeAnyOf types 
 where
  types = [ FromSomeType (AsVerificationKey AsDRepKey) AnyDrepVerificationKey
          , FromSomeType (AsVerificationKey AsDRepExtendedKey) AnyDrepExtendedVerificationKey
          ]