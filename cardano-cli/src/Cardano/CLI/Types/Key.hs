{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Shelley CLI option data types and functions for cryptographic keys.
module Cardano.CLI.Types.Key
  ( VerificationKeyOrFile (..)
  , readVerificationKeyOrFile
  , readVerificationKeyOrTextEnvFile

  , VerificationKeyTextOrFile (..)
  , VerificationKeyTextOrFileError (..)
  , readVerificationKeyTextOrFileAnyOf
  , renderVerificationKeyTextOrFileError

  , VerificationKeyOrHashOrFile (..)
  , readVerificationKeyOrHashOrFile
  , readVerificationKeyOrHashOrTextEnvFile

  , PaymentVerifier(..)
  , StakeIdentifier(..)
  , StakeVerifier(..)

  , generateKeyPair
  --- Legacy
  , DelegationTarget(..) -- TODO: Remove me
  , StakePoolRegistrationParserRequirements(..)

  -- NewEraBased
  , AnyDelegationTarget(..)
  , StakeTarget (..)

  , AnyRegistrationTarget(..)
  , RegistrationTarget(..)

  , ColdVerificationKeyOrFile(..)
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Legacy

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


------------------------------------------------------------------------------
-- Verification key deserialisation
------------------------------------------------------------------------------

-- | Either a verification key or path to a verification key file.
data VerificationKeyOrFile keyrole
  = VerificationKeyValue !(VerificationKey keyrole)
  -- ^ A verification key.
  | VerificationKeyFilePath !(VerificationKeyFile In)
  -- ^ A path to a verification key file.
  -- Note that this file hasn't been validated at all (whether it exists,
  -- contains a key of the correct type, etc.)

deriving instance Show (VerificationKey keyrole)
  => Show (VerificationKeyOrFile keyrole)

deriving instance Eq (VerificationKey keyrole)
  => Eq (VerificationKeyOrFile keyrole)

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrFile
  :: ( HasTextEnvelope (VerificationKey keyrole)
     , SerialiseAsBech32 (VerificationKey keyrole)
     )
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath (File fp) ->
      readKeyFile
        (AsVerificationKey asType)
        (NE.fromList [InputFormatBech32, InputFormatHex, InputFormatTextEnvelope])
        fp

-- | Read a verification key or verification key file and return a
-- verification key.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrTextEnvFile
  :: HasTextEnvelope (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (VerificationKey keyrole))
readVerificationKeyOrTextEnvFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure (Right vk)
    VerificationKeyFilePath fp -> readKeyFileTextEnvelope (AsVerificationKey asType) fp

data PaymentVerifier
  = PaymentVerifierKey VerificationKeyTextOrFile
  | PaymentVerifierScriptFile ScriptFile
  deriving (Eq, Show)

data StakeVerifier
  = StakeVerifierKey (VerificationKeyOrFile StakeKey)
  | StakeVerifierScriptFile ScriptFile
  deriving (Eq, Show)

data StakeIdentifier
  = StakeIdentifierVerifier StakeVerifier
  | StakeIdentifierAddress StakeAddress
  deriving (Eq, Show)


data AnyRegistrationTarget where
  ShelleyToBabbageStakePoolRegTarget
    :: ShelleyToBabbageEra era
    -> StakePoolRegistrationParserRequirements
    -> AnyRegistrationTarget

  ShelleyToBabbageStakeKeyRegTarget
    :: ShelleyToBabbageEra era
    -> StakeIdentifier
    -> AnyRegistrationTarget

  ConwayOnwardRegTarget
    :: ConwayEraOnwards era
    -> RegistrationTarget era
    -> AnyRegistrationTarget

data StakePoolRegistrationParserRequirements
 = StakePoolRegistrationParserRequirements
     { sprStakePoolKey :: VerificationKeyOrFile StakePoolKey
     -- ^ Stake pool verification key.
     , sprVrfKey :: VerificationKeyOrFile VrfKey
     -- ^ VRF Verification key.
     , sprPoolPledge :: Lovelace
     -- ^ Pool pledge.
     , sprPoolCost :: Lovelace
     -- ^ Pool cost.
     , sprPoolMargin :: Rational
     -- ^ Pool margin.
     , sprRewardAccountKey :: VerificationKeyOrFile StakeKey
     -- ^ Reward account verification staking key.
     , spoPoolOwnerKeys :: [VerificationKeyOrFile StakeKey]
     -- ^ Pool owner verification staking key(s).
     , sprRelays :: [StakePoolRelay]
     -- ^ Stake pool relays.
     , sprMetadata :: Maybe StakePoolMetadataReference
     -- ^ Stake pool metadata.
     , sprNetworkId :: NetworkId
     }


data RegistrationTarget era where
  RegisterStakePool
    :: ConwayEraOnwards era
    -> StakePoolRegistrationParserRequirements
    -> RegistrationTarget era

  RegisterStakeKey
    :: ConwayEraOnwards era
    -> StakeIdentifier
    -> Lovelace
    -> RegistrationTarget era

  RegisterDRep
    :: ConwayEraOnwards era
    -> VerificationKeyOrHashOrFile DRepKey
    -> Lovelace
    -> RegistrationTarget era

-- | A resource that identifies the delegation target. We can delegate
-- our stake for two reasons:
-- 1. To gain rewards. This is limited to choosing a stake pool
-- 2. To delegate voting power. We can delegate this to a DRep, always
-- abstain our vote or vote no confidence

data AnyDelegationTarget where
  ShelleyToBabbageDelegTarget
    :: ShelleyToBabbageEra era
    -> VerificationKeyOrHashOrFile StakePoolKey -- ^ Stake pool target
    -> AnyDelegationTarget

  ConwayOnwardDelegTarget
    :: ConwayEraOnwards era
    -> StakeTarget era
    -> AnyDelegationTarget

deriving instance Show AnyDelegationTarget

data StakeTarget era where
  -- This delegates stake to earn rewards
  TargetStakePool
    :: ConwayEraOnwards era
    -> VerificationKeyOrHashOrFile StakePoolKey
    -> StakeTarget era

  -- This delegates stake for voting
  TargetVotingDrep
    :: ConwayEraOnwards era
    -> VerificationKeyOrHashOrFile DRepKey
    -> StakeTarget era

  -- This delegates stake for voting and rewards
  TargetVotingDrepAndStakePool
    :: ConwayEraOnwards era
    -> VerificationKeyOrHashOrFile DRepKey
    -> VerificationKeyOrHashOrFile StakePoolKey
    -> StakeTarget era

  TargetAlwaysAbstain
    :: ConwayEraOnwards era
    -> StakeTarget era

  TargetAlwaysNoConfidence
    :: ConwayEraOnwards era
    -> StakeTarget era

  TargetVotingDRepScriptHash
    :: ConwayEraOnwards era
    -> ScriptHash
    -> StakeTarget era

deriving instance Show (StakeTarget era)

newtype DelegationTarget
  = StakePoolDelegationTarget (VerificationKeyOrHashOrFile StakePoolKey)
  deriving Show

-- | Either an unvalidated text representation of a verification key or a path
-- to a verification key file.
data VerificationKeyTextOrFile
  = VktofVerificationKeyText !Text
  | VktofVerificationKeyFile !(VerificationKeyFile In)
  deriving (Eq, Show)

-- | An error in deserialising a 'VerificationKeyTextOrFile' to a
-- 'VerificationKey'.
data VerificationKeyTextOrFileError
  = VerificationKeyTextError !InputDecodeError
  | VerificationKeyFileError !(FileError InputDecodeError)
  deriving Show

-- | Render an error message for a 'VerificationKeyTextOrFileError'.
renderVerificationKeyTextOrFileError :: VerificationKeyTextOrFileError -> Text
renderVerificationKeyTextOrFileError vkTextOrFileErr =
  case vkTextOrFileErr of
    VerificationKeyTextError err -> renderInputDecodeError err
    VerificationKeyFileError err -> Text.pack (displayError err)

-- | Deserialise a verification key from text or a verification key file.
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyTextOrFileAnyOf
  :: VerificationKeyTextOrFile
  -> IO (Either VerificationKeyTextOrFileError SomeAddressVerificationKey)
readVerificationKeyTextOrFileAnyOf verKeyTextOrFile =
  case verKeyTextOrFile of
    VktofVerificationKeyText vkText ->
      pure $ first VerificationKeyTextError $
        deserialiseAnyVerificationKey (Text.encodeUtf8 vkText)
    VktofVerificationKeyFile (File fp) -> do
      vkBs <- liftIO $ BS.readFile fp
      pure $ first VerificationKeyTextError $
        deserialiseAnyVerificationKey vkBs


-- | Verification key, verification key hash, or path to a verification key
-- file.
data VerificationKeyOrHashOrFile keyrole
  = VerificationKeyOrFile !(VerificationKeyOrFile keyrole)
  -- ^ Either a verification key or path to a verification key file.
  | VerificationKeyHash !(Hash keyrole)
  -- ^ A verification key hash.

deriving instance (Show (VerificationKeyOrFile keyrole), Show (Hash keyrole))
  => Show (VerificationKeyOrHashOrFile keyrole)

deriving instance (Eq (VerificationKeyOrFile keyrole), Eq (Hash keyrole))
  => Eq (VerificationKeyOrHashOrFile keyrole)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, the file can either be formatted as Bech32, hex,
-- or text envelope.
readVerificationKeyOrHashOrFile
  :: (Key keyrole, SerialiseAsBech32 (VerificationKey keyrole))
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrHashOrTextEnvFile
  :: Key keyrole
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> IO (Either (FileError InputDecodeError) (Hash keyrole))
readVerificationKeyOrHashOrTextEnvFile asType verKeyOrHashOrFile =
  case verKeyOrHashOrFile of
    VerificationKeyOrFile vkOrFile -> do
      eitherVk <- readVerificationKeyOrTextEnvFile asType vkOrFile
      pure (verificationKeyHash <$> eitherVk)
    VerificationKeyHash vkHash -> pure (Right vkHash)

generateKeyPair ::
#if __GLASGOW_HASKELL__ >= 904
-- GHC 8.10 considers the HasTypeProxy constraint redundant but ghc-9.4 and above complains if its
-- not present.
    (Key keyrole, HasTypeProxy keyrole) =>
#else
    Key keyrole =>
#endif
    AsType keyrole -> IO (VerificationKey keyrole, SigningKey keyrole)
generateKeyPair asType = do
  skey <- generateSigningKey asType
  return (getVerificationKey skey, skey)

-- | Either a stake pool verification key, genesis delegate verification key,
-- or a path to a cold verification key file.
--
-- Note that a "cold verification key" refers to either a stake pool or
-- genesis delegate verification key.
--
-- TODO: A genesis delegate extended key should also be valid here.
data ColdVerificationKeyOrFile
  = ColdStakePoolVerificationKey !(VerificationKey StakePoolKey)
  | ColdGenesisDelegateVerificationKey !(VerificationKey GenesisDelegateKey)
  | ColdVerificationKeyFile !(VerificationKeyFile In)
  deriving Show
