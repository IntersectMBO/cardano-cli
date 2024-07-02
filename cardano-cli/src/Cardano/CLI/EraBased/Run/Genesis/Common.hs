{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.CLI.EraBased.Run.Genesis.Common
  ( decodeShelleyGenesisFile
  , decodeAlonzoGenesisFile
  , decodeConwayGenesisFile
  , genStuffedAddress
  , getCurrentTimePlus30
  , readRelays

    -- * Protocol Parameters
  , readProtocolParameters
  )
where

import           Cardano.Api hiding (ConwayEra)
import           Cardano.Api.Ledger (AlonzoGenesis, ConwayGenesis, StandardCrypto)
import qualified Cardano.Api.Ledger as L
import           Cardano.Api.Shelley (Address (ShelleyAddress), ShelleyGenesis, ShelleyLedgerEra,
                   decodeAlonzoGenesis)

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Errors.GenesisCmdError
import           Cardano.CLI.Types.Errors.ProtocolParamsError
import           Cardano.Crypto.Hash (HashAlgorithm)
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Random as Crypto

import qualified Data.Aeson as A
import qualified Data.Binary.Get as Bin
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Coerce (coerce)
import           Data.Data (Proxy (..))
import           Data.Map.Strict (Map)
import qualified Data.Text as Text
import           Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import           Data.Word (Word64)

import           Crypto.Random (getRandomBytes)

decodeShelleyGenesisFile
  :: MonadIOTransError GenesisCmdError t m
  => FilePath
  -> t m (ShelleyGenesis StandardCrypto)
decodeShelleyGenesisFile = readAndDecodeGenesisFileWith A.eitherDecode

-- | Decode Alonzo Genesis file. See 'Cardano.Api.Genesis.decodeAlonzoGenesis' haddocks for details.
decodeAlonzoGenesisFile
  :: MonadIOTransError GenesisCmdError t m
  => Maybe (CardanoEra era)
  -- ^ Optional era in which we're decoding alonzo genesis.
  -> FilePath
  -> t m AlonzoGenesis
decodeAlonzoGenesisFile mEra = readAndDecodeGenesisFileWith (runExcept . decodeAlonzoGenesis mEra)

decodeConwayGenesisFile
  :: MonadIOTransError GenesisCmdError t m
  => FilePath
  -> t m (ConwayGenesis StandardCrypto)
decodeConwayGenesisFile = readAndDecodeGenesisFileWith A.eitherDecode

readAndDecodeGenesisFileWith
  :: MonadIOTransError GenesisCmdError t m
  => (LBS.ByteString -> Either String a)
  -> FilePath
  -> t m a
readAndDecodeGenesisFileWith decode' fpath = do
  lbs <-
    handleIOExceptionsLiftWith (GenesisCmdGenesisFileError . FileIOError fpath) . liftIO $
      LBS.readFile fpath
  modifyError (GenesisCmdGenesisFileDecodeError fpath . Text.pack)
    . hoistEither
    $ decode' lbs

genStuffedAddress :: L.Network -> IO (AddressInEra ShelleyEra)
genStuffedAddress network = do
  paymentCredential <-
    L.KeyHashObj . mkKeyHash . read64BitInt <$> Crypto.runSecureRandom (getRandomBytes 8)
  pure . shelleyAddressInEra ShelleyBasedEraShelley $
    ShelleyAddress network paymentCredential L.StakeRefNull
 where
  read64BitInt :: ByteString -> Int
  read64BitInt =
    (fromIntegral :: Word64 -> Int)
      . Bin.runGet Bin.getWord64le
      . LBS.fromStrict

  mkDummyHash :: forall h a. HashAlgorithm h => Proxy h -> Int -> Hash.Hash h a
  mkDummyHash _ = coerce . L.hashWithSerialiser @h L.toCBOR

  mkKeyHash :: forall c discriminator. L.Crypto c => Int -> L.KeyHash discriminator c
  mkKeyHash = L.KeyHash . mkDummyHash (Proxy @(L.ADDRHASH c))

-- | Current UTCTime plus 30 seconds
getCurrentTimePlus30 :: MonadIO m => m UTCTime
getCurrentTimePlus30 =
  plus30sec <$> liftIO getCurrentTime
 where
  plus30sec :: UTCTime -> UTCTime
  plus30sec = addUTCTime (30 :: NominalDiffTime)

-- @readRelays fp@ reads the relays specification from a file
readRelays
  :: ()
  => MonadIO m
  => FilePath
  -- ^ The file to read from
  -> ExceptT GenesisCmdError m (Map Word [L.StakePoolRelay])
readRelays fp = do
  relaySpecJsonBs <-
    handleIOExceptT (GenesisCmdStakePoolRelayFileError fp) (LBS.readFile fp)
  firstExceptT (GenesisCmdStakePoolRelayJsonDecodeError fp)
    . hoistEither
    $ A.eitherDecode relaySpecJsonBs

-- TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters
  :: ()
  => ShelleyBasedEra era
  -> ProtocolParamsFile
  -> ExceptT ProtocolParamsError IO (L.PParams (ShelleyLedgerEra era))
readProtocolParameters sbe (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ProtocolParamsErrorFile . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ProtocolParamsErrorJSON fpath . Text.pack) . hoistEither $
    shelleyBasedEraConstraints sbe $
      A.eitherDecode' pparams
