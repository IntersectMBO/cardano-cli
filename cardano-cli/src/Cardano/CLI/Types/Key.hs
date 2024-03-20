{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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

  , VerificationKeyOrHashOrFileOrScript(..)
  , VerificationKeyOrHashOrFileOrScriptHash(..)
  , readVerificationKeyOrHashOrFileOrScriptHash

  , PaymentVerifier(..)
  , StakeIdentifier(..)
  , StakeVerifier(..)

  , generateKeyPair

  --- Legacy
  , StakePoolRegistrationParserRequirements(..)

  -- NewEraBased
  , AnyDelegationTarget(..)
  , StakeTarget (..)

  , ColdVerificationKeyOrFile(..)

  , DRepHashSource(..)

  , readDRepCredential

  , SomeSigningKey(..)
  , withSomeSigningKey
  , readSigningKeyFile
  ) where

import           Cardano.Api
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley

import           Cardano.CLI.Types.Common

import           Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
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
  :: MonadIOTransError (FileError InputDecodeError) t m
  => HasTextEnvelope (VerificationKey keyrole)
  => SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> t m (VerificationKey keyrole)
readVerificationKeyOrFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure vk
    VerificationKeyFilePath (File fp) -> hoistIOEither $
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
  :: MonadIOTransError (FileError InputDecodeError) t m
  => HasTextEnvelope (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrFile keyrole
  -> t m (VerificationKey keyrole)
readVerificationKeyOrTextEnvFile asType verKeyOrFile =
  case verKeyOrFile of
    VerificationKeyValue vk -> pure vk
    VerificationKeyFilePath fp -> hoistIOEither $ readKeyFileTextEnvelope (AsVerificationKey asType) fp

data PaymentVerifier
  = PaymentVerifierKey VerificationKeyTextOrFile
  | PaymentVerifierScriptFile ScriptFile
  deriving (Eq, Show)

data StakeVerifier
  = StakeVerifierKey (VerificationKeyOrHashOrFile StakeKey)
  | StakeVerifierScriptFile ScriptFile
  deriving (Eq, Show)

data StakeIdentifier
  = StakeIdentifierVerifier StakeVerifier
  | StakeIdentifierAddress StakeAddress
  deriving (Eq, Show)

data StakePoolRegistrationParserRequirements
 = StakePoolRegistrationParserRequirements
     { sprStakePoolKey :: VerificationKeyOrFile StakePoolKey
     -- ^ Stake pool verification key.
     , sprVrfKey :: VerificationKeyOrFile VrfKey
     -- ^ VRF Verification key.
     , sprPoolPledge :: L.Coin
     -- ^ Pool pledge.
     , sprPoolCost :: L.Coin
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
renderVerificationKeyTextOrFileError :: VerificationKeyTextOrFileError -> Doc ann
renderVerificationKeyTextOrFileError vkTextOrFileErr =
  case vkTextOrFileErr of
    VerificationKeyTextError err -> renderInputDecodeError err
    VerificationKeyFileError err -> prettyError err

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
  :: MonadIOTransError (FileError InputDecodeError) t m
  => Key keyrole
  => SerialiseAsBech32 (VerificationKey keyrole)
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> t m (Hash keyrole)
readVerificationKeyOrHashOrFile asType  =
  \case
    VerificationKeyOrFile vkOrFile ->
      verificationKeyHash <$> readVerificationKeyOrFile asType vkOrFile
    VerificationKeyHash vkHash -> pure vkHash

-- | Read a verification key or verification key hash or verification key file
-- and return a verification key hash.
--
-- If a filepath is provided, it will be interpreted as a text envelope
-- formatted file.
readVerificationKeyOrHashOrTextEnvFile
  :: MonadIOTransError (FileError InputDecodeError) t m
  => Key keyrole
  => AsType keyrole
  -> VerificationKeyOrHashOrFile keyrole
  -> t m (Hash keyrole)
readVerificationKeyOrHashOrTextEnvFile asType =
  \case
    VerificationKeyOrFile vkOrFile ->
      verificationKeyHash <$> readVerificationKeyOrTextEnvFile asType vkOrFile
    VerificationKeyHash vkHash -> pure vkHash

generateKeyPair
  :: MonadIO m
  => Key keyrole
  => HasTypeProxy keyrole
  => AsType keyrole
  -> m (VerificationKey keyrole, SigningKey keyrole)
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

data DRepHashSource
  = DRepHashSourceScript
      ScriptHash
  | DRepHashSourceVerificationKey
      (VerificationKeyOrHashOrFile DRepKey)
  deriving (Eq, Show)

readDRepCredential
  :: MonadIOTransError (FileError InputDecodeError) t m
  => DRepHashSource
  -> t m (L.Credential L.DRepRole L.StandardCrypto)
readDRepCredential = \case
  DRepHashSourceScript (ScriptHash scriptHash) ->
    pure (L.ScriptHashObj scriptHash)
  DRepHashSourceVerificationKey drepVKeyOrHashOrFile -> do
    L.KeyHashObj . unDRepKeyHash <$>
      readVerificationKeyOrHashOrTextEnvFile AsDRepKey drepVKeyOrHashOrFile

data VerificationKeyOrHashOrFileOrScript keyrole
  = VkhfsKeyHashFile !(VerificationKeyOrHashOrFile keyrole)
  | VkhfsScript !(File ScriptInAnyLang In)

deriving instance (Eq (VerificationKeyOrHashOrFile c)) => Eq (VerificationKeyOrHashOrFileOrScript c)
deriving instance (Show (VerificationKeyOrHashOrFile c)) => Show (VerificationKeyOrHashOrFileOrScript c)

data VerificationKeyOrHashOrFileOrScriptHash keyrole
  = VkhfshKeyHashFile !(VerificationKeyOrHashOrFile keyrole)
  | VkhfshScriptHash !ScriptHash

deriving instance (Eq (VerificationKeyOrHashOrFile c)) => Eq (VerificationKeyOrHashOrFileOrScriptHash c)
deriving instance (Show (VerificationKeyOrHashOrFile c)) => Show (VerificationKeyOrHashOrFileOrScriptHash c)

readVerificationKeyOrHashOrFileOrScriptHash
  :: MonadIOTransError (FileError InputDecodeError) t m
  => Key keyrole
  => AsType keyrole
  -> (Hash keyrole -> L.KeyHash kr L.StandardCrypto)
  -> VerificationKeyOrHashOrFileOrScriptHash keyrole
  -> t m (L.Credential kr L.StandardCrypto)
readVerificationKeyOrHashOrFileOrScriptHash asType extractHash = \case
  VkhfshScriptHash (ScriptHash scriptHash) ->
    pure (L.ScriptHashObj scriptHash)
  VkhfshKeyHashFile vKeyOrHashOrFile -> do
    L.KeyHashObj . extractHash <$>
      readVerificationKeyOrHashOrTextEnvFile asType vKeyOrHashOrFile

data SomeSigningKey
  = AByronSigningKey                    (SigningKey ByronKey)
  | APaymentSigningKey                  (SigningKey PaymentKey)
  | APaymentExtendedSigningKey          (SigningKey PaymentExtendedKey)
  | AStakeSigningKey                    (SigningKey StakeKey)
  | AStakeExtendedSigningKey            (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey                (SigningKey StakePoolKey)
  | AGenesisSigningKey                  (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey          (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey          (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey  (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey              (SigningKey GenesisUTxOKey)
  | ADRepSigningKey                     (SigningKey DRepKey)
  | ADRepExtendedSigningKey             (SigningKey DRepExtendedKey)
  | ACommitteeColdSigningKey            (SigningKey CommitteeColdKey)
  | ACommitteeColdExtendedSigningKey    (SigningKey CommitteeColdExtendedKey)
  | ACommitteeHotSigningKey             (SigningKey CommitteeHotKey)
  | ACommitteeHotExtendedSigningKey     (SigningKey CommitteeHotExtendedKey)
  | AVrfSigningKey                      (SigningKey VrfKey)
  | AKesSigningKey                      (SigningKey KesKey)

withSomeSigningKey :: ()
  => SomeSigningKey
  -> (forall keyrole. (Key keyrole, HasTypeProxy keyrole) => SigningKey keyrole -> a)
  -> a
withSomeSigningKey ssk f =
    case ssk of
      AByronSigningKey                    sk -> f sk
      APaymentSigningKey                  sk -> f sk
      APaymentExtendedSigningKey          sk -> f sk
      AStakeSigningKey                    sk -> f sk
      AStakeExtendedSigningKey            sk -> f sk
      AStakePoolSigningKey                sk -> f sk
      AGenesisSigningKey                  sk -> f sk
      AGenesisExtendedSigningKey          sk -> f sk
      AGenesisDelegateSigningKey          sk -> f sk
      AGenesisDelegateExtendedSigningKey  sk -> f sk
      AGenesisUTxOSigningKey              sk -> f sk
      ADRepSigningKey                     sk -> f sk
      ADRepExtendedSigningKey             sk -> f sk
      ACommitteeColdSigningKey            sk -> f sk
      ACommitteeColdExtendedSigningKey    sk -> f sk
      ACommitteeHotSigningKey             sk -> f sk
      ACommitteeHotExtendedSigningKey     sk -> f sk
      AVrfSigningKey                      sk -> f sk
      AKesSigningKey                      sk -> f sk

readSigningKeyFile :: ()
  => SigningKeyFile In
  -> ExceptT (FileError InputDecodeError) IO SomeSigningKey
readSigningKeyFile skFile =
    newExceptT $
      readKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
  where
    -- If you update these variables, consider updating the ones with the same
    -- names in Cardano.CLI.Read
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)                   AByronSigningKey
      , FromSomeType (AsSigningKey AsPaymentKey)                 APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)         APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)                   AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)           AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)               AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)                 AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)         AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)         AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey) AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)             AGenesisUTxOSigningKey
      , FromSomeType (AsSigningKey AsDRepKey)                    ADRepSigningKey
      , FromSomeType (AsSigningKey AsDRepExtendedKey)            ADRepExtendedSigningKey
      , FromSomeType (AsSigningKey AsCommitteeColdKey)           ACommitteeColdSigningKey
      , FromSomeType (AsSigningKey AsCommitteeColdExtendedKey)   ACommitteeColdExtendedSigningKey
      , FromSomeType (AsSigningKey AsCommitteeHotKey)            ACommitteeHotSigningKey
      , FromSomeType (AsSigningKey AsCommitteeHotExtendedKey)    ACommitteeHotExtendedSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)                     AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)                     AKesSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey)         APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey) APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)           AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)   AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)       AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsVrfKey)             AVrfSigningKey
      , FromSomeType (AsSigningKey AsKesKey)             AKesSigningKey
      ]
